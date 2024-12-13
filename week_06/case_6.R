library(terra)
library(spData)
library(tidyverse)
library(sf)
library(ncdf4)

download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc","crudata.nc",method="curl")

tmean=rast("crudata.nc")

plot(tmean)

max_temp <- max(tmean)

plot(max_temp)

max_temp_by_country <- terra::extract(max_temp, world, fun = max)

colnames(max_temp_by_country)[ncol(max_temp_by_country)] <- "max_temp"

world_clim <- bind_cols(world, max_temp_by_country)

ggplot(world_clim) +
  geom_sf(aes(fill = max_temp)) +
  scale_fill_viridis_c(name = "Maximum\nTemperature (C)") +
  theme(legend.position = "bottom")

hottest_continents <- world_clim %>%
  group_by(continent) %>%
  slice_max(max_temp, n = 1) %>%
  select(continent, name_long, max_temp) %>%
  arrange(desc(max_temp)) %>%
  st_set_geometry(NULL)

print(hottest_continents)
