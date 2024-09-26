# install.packages("tidyverse")
# install.packages("nycflights13")

library(tidyverse)
library(nycflights13)
library(dplyr)
#check the contents of this dataset
head(flights)
glimpse(flights)
view(flights)
str(flights)

glimpse(weather)
glimpse(airlines)
glimpse(airports)
glimpse(planes)
#name, distance, and destination come from flights(distance,origin and dest),ffa and name come from airports 

# find the farthest airport code from the New York Airports (JFK, LGA, EWR)
farthest_airport_code <- flights %>%
  filter(origin == "JFK" | origin == "LGA" | origin == "EWR") %>% #filter(origin %in% c("JFK", "LGA", "EWR"))
  arrange(desc(distance)) %>%
  slice(1) %>%
  select(dest)

# join this table with the 'airports' table to get the full airport name
farthest_airport <- farthest_airport_code %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  select(name) # only keep the name after left_join

# convert the result to a single character value
farthest_airport <- as.character(farthest_airport$name)

# print the result
farthest_airport


airports %>%
  distinct(lon,lat) %>% # Keep only unique/distinct rows from a data frame.it's similar to  unique.data.frame() but considerably faster
  ggplot(aes(lon, lat)) +
  borders("world") +
  geom_point(col="red") +
  coord_quickmap() # projects a portion of the earth

# calculate the average delay per airport (destination)
avg_delay_per_airport <- flights %>%
  group_by(dest) %>%
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE))  # calculate average delay

# Join with the 'airports' table
airport_delay_data <- avg_delay_per_airport %>%
  left_join(airports, by = c("dest" = "faa"))  # join on airport code

# create the plot with the average delay mapped to color
airport_delay_data %>%
  ggplot(aes(lon, lat, color = avg_delay)) +
  borders("usa") +
  geom_point(size = 3) +  # plot points for airports 
  scale_color_gradient2(low = "#3575B3", mid = "white", high = "#BF3935", midpoint = 10, na.value = "grey50") +  # Color gradient with hex codes
  coord_quickmap(xlim = c(-125, -65), ylim = c(20, 50)) +
  labs(color = "Avg Delay (min)") +  # label for the color legend
  theme_minimal()  # use a minimal theme
