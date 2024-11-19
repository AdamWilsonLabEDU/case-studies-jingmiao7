#install.packages("terra")
#install.packages("ncdf4")

library(terra)
library(spData)
library(tidyverse)
library(sf)

library(ncdf4)
download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc","crudata.nc", method="curl")

# read dataset using the rast() function from the terra package
tmean <- rast("crudata.nc")

plot(tmean)
tmean_max <- max(tmean)
plot(tmean_max)

data(world)
max_temp_by_country <- terra::extract(tmean_max, world, fun = max, na.rm = T)


head(max_temp_by_country)

world_clim <- bind_cols(world, max_temp_by_country)
# world_clim2 <- bind_cols(max_temp_by_country, world)

# Display the new combined data
head(world_clim)


ggplot(world_clim) +
  geom_sf(aes(fill = max)) +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_viridis_c(name="Maximum\nTemperature (C)") +
  labs(title = "Maximum Temperature by Country",
       subtitle = "CRU Temperature Dataset",
       caption = "Data source: CRU")
  
hottest_continents <- world_clim %>%
  group_by(continent) %>%
  top_n(1) %>%
  select(name_long, continent, max) %>%
  arrange(desc(max)) %>%
  st_set_geometry(NULL)

