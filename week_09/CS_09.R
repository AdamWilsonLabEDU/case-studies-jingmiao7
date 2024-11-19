# load the necessary packages and datasets
# install.packages("lubridate")
library(lubridate)
library(sf)
library(tidyverse)
library(ggmap)
library(spData)
library(dplyr)
library(ggplot2)
data(world)
data(us_states)



# Download a csv from noaa with storm track information
dataurl="https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/csv/ibtracs.NA.list.v04r01.csv"
storm_data <- read_csv(dataurl)

# Wrangle the data
storm_data_wrapped <- storm_data %>%
  mutate(Year = year(ISO_TIME)) %>% # add a column "Year"
  filter(Year >= 1950) %>% # select the data from 1950 to present
  mutate_if(is.numeric, function(x) ifelse(x==-999.0,NA,x)) %>% # convert -999.0 to NA in all numeric columns
  mutate(decade = floor(Year / 10) * 10) # add a column "decade"

# convert objectives to sf
storm_sf <- st_as_sf(storm_data_wrapped, coords = c("LON", "LAT"), crs = 4326)
# identify the bounding box of the storm data
region <- st_bbox(storm_sf)

# plot
ggplot(data = world) +
  geom_sf(fill = "lightgray", color = "white") +                         
  geom_sf(data = storm_sf, alpha = 0.2) +    # Plot storm points
  stat_bin2d(data = storm_sf, aes(x = st_coordinates(storm_sf)[,1],      # Add storm density
                                  y = st_coordinates(storm_sf)[,2]), 
             bins = 100) +
  scale_fill_distiller(palette = "YlOrRd", trans = "log",                
                       direction = -1, breaks = c(1, 10, 100, 1000)) +
  facet_wrap(~decade) +                                                  # Facet by decade
  coord_sf(xlim = region[c("xmin", "xmax")],                             # Crop plot to bounding box
           ylim = region[c("ymin", "ymax")]) +
  theme_minimal() +
  labs(title = "Storm Density by Decade", 
       x = "Longitude", 
       y = "Latitude", 
       fill = "Storm Density (log scale)")


# table 

# reproject us_states to the CRS of storm_sf
states <- us_states %>%
  st_transform(st_crs(storm_sf)) %>%
  select(state = NAME) # rename the "NAME" column to "state" select(new_name = old_name)

# join storm_sf and states
storm_states <- st_join(storm_sf, states, join = st_intersects, left = FALSE)

# organize table: 
top_states <- storm_states %>%
  group_by(state) %>% # group by "state"
  summarize(storm_sf = length(unique(NAME))) %>% # summarize by unique name in storm_sf
  arrange(desc(storm_sf)) %>% # arrange table by storm_sf
  slice(1:5) %>% # pick top 5
  st_drop_geometry() # ingore geometry in table

# print table
top_states

