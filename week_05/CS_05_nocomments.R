# install.packages("spData")
# install.packages("sf")
install.packages("units")

library(spData)
library(sf)
library(tidyverse) 
library(units) #this one is optional, but can help with unit conversions.

#load 'world' data from spData package
data(world)
glimpse(world)
# load 'states' boundaries from spData package
data(us_states)
glimpse(us_states)
plot(world[1])  #plot if desired
plot(us_states[1]) #plot if desired

albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

canada <- world[world$name_long == "Canada", ]

canada_albers <- st_transform(canada, crs = albers)
canada_buffered <- st_buffer(canada_albers, dist = 10000)
canada_buffered2 <- st_buffer(canada_albers, dist = units::set_units(10000, "m"))

# Plot Canada and its buffer
ggplot() +
  geom_sf(data = canada, fill = "lightblue", color = "gray") +  # Original Canada
  geom_sf(data = canada_buffered, fill = NA, color = "black", linetype = "dashed") +  # Buffered Canada
  geom_sf(data = canada_buffered2, fill = NA, color = "red") +
  theme_minimal() +
  ggtitle("Canada with 10 km Buffer")

newyork <- us_states[us_states$NAME == "New York", ]
newyork_proj <- st_transform(newyork, crs = albers)

border <- st_intersection(canada_buffered, newyork_proj)

ggplot() +
  geom_sf(data = newyork_proj, fill = NA, color = "black") +
  geom_sf(data = border, fill = "red", color = "black", alpha = 0.5) +
  theme_minimal() +
  ggtitle("Border Area Between Buffered Canada and New York")

border_area <- units::set_units(st_area(border), km^2)
border_area

#install.packages("leaflet")
library(leaflet)

border_wgs84 <- st_transform(border, crs = 4326)

leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addPolygons(data = border_wgs84, color = "black", weight = 2, opacity = 0.7,
              fillColor = "red", fillOpacity = 0.5, popup = ~as.character(st_area(border_wgs84))) %>%
  addLegend(position = "bottomright", colors = "black", labels = "Border Area", title = "Legend") %>%
  setView(lng = -75, lat = 45, zoom = 5)  # Set the center of the map to North America
