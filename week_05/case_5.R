library(spData)
library(sf)
library(units)
library(ggplot2)

data(world)
str(world)

data(us_states)

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

world <- st_transform(world, crs = albers)

world_filter <- world %>% filter(name_long == "Canada")

canada_buffer <- st_buffer(world_filter, 10000)

us_states <- st_transform(us_states, crs = albers)

new_york <- us_states %>% filter(NAME == "New York")

st_crs(canada_buffer)
st_crs(new_york)

#canada_buffer <- st_transform(canada_buffer, st_crs(new_york)) found error, wasn't storing changed coords.

border <- st_intersection(canada_buffer, new_york)

area_m2 <- st_area(border)

area_km2 <- set_units(area_m2, "km^2")

ggplot() +
  geom_sf(data = new_york) +
  geom_sf(data = border, fill = "red") +
  ggtitle("New York Land within 10km of Canada")
  
