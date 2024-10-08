install.packages("ncdf4")

library(terra)
library(spData)
library(tidyverse)
library(sf)

library(ncdf4)
download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc","crudata.nc", method="curl")

# read in the data using the rast() function from the terra package
tmean=rast("crudata.nc")
plot(tmean)
plot(max(tmean))

data(world)
max_temp_by_country <- terra::extract(tmean, world, fun = max, na.rm=T, small=T)
head(max_temp_by_country)

world_clim <- bind_cols(world, as_tibble(max_temp_by_country)[, -1])

# Display the new combined data
head(world_clim)