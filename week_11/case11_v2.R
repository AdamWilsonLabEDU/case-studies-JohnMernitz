library(tidyverse)
library(spData)
library(sf)
library(dplyr)
library(magrittr)

## New Packages
library(mapview)  # new package that makes easy leaflet maps
library(foreach)
library(doParallel)
registerDoParallel(4)
getDoParWorkers()  # check registered cores

library(tidycensus)
census_api_key("5c44b7b2fafef549b34391564b3d2d38246fde43", install = TRUE)

## Code to specify race populations
race_vars <- c(
  "Total Population" = "P1_001N",
  "White alone" = "P1_003N",
  "Black or African American alone" = "P1_004N",
  "American Indian and Alaska Native alone" = "P1_005N",
  "Asian alone" = "P1_006N",
  "Native Hawaiian and Other Pacific Islander alone" = "P1_007N",
  "Some Other Race alone" = "P1_008N",
  "Two or More Races" = "P1_009N"
)

options(tigris_use_cache = TRUE)

# Download the decennial census data for Erie County, NY (2020)
erie <- get_decennial(geography = "block", 
                      variables = race_vars, 
                      year = 2020,
                      state = "NY", 
                      county = "Erie County", 
                      geometry = TRUE,
                      sumfile = "pl", 
                      cache_table = TRUE)
  
# Crop the data to a specific bounding box
erie_crop <- st_crop(erie, xmin = -78.9, xmax = -78.85, ymin = 42.888, ymax = 42.92)

# Reshape the data into wide format
erie_wide <- erie_crop %>%
  dplyr::select(GEOID, variable, value, geometry) %>%
  spread(key = variable, value = value)

# Foreach loop to sample census points for each racial group
census_points <- foreach(i = 2:7, .combine = rbind) %dopar% {
  
  # Specify the race group for the loop
  race_group <- names(race_vars)[i]
  race_column <- race_vars[i]
  
  # Ensure the correct column name is referenced in the filtering step
  race_data <- erie_wide %>%
    filter(!!sym(race_column) > 0)
  
  # Sample points based on the population of each racial group
  sampled_points <- st_sample(race_data, size = race_data[[race_column]], type = "random")
  
  # Convert sampled points to an sf object
  sampled_points_sf <- st_as_sf(sampled_points)
  
  # Add the race group as a new column
  sampled_points_sf$variable <- race_group
  
  return(sampled_points_sf)
}


# Visualize the sampled points on a map with the racial identity as the zcol
mapview(census_points, cex = 2)


