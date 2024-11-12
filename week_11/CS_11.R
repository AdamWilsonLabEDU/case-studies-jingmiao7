# install.packages("mapview")
# install.packages("foreach")
# install.packages("doParallel")

# loadthe necessary packages
library(tidyverse)
library(spData)
library(sf)
library(mapview)
library(foreach)
library(doParallel)

# to seperate the work into 4 cores
registerDoParallel(4)
getDoParWorkers() # check registered cores

# load the tidycensus and read the oersonal key to use api
library(tidycensus)
census_api_key("60986de2c2c33c7beb144d5aed62a15407826fd9")

#read all the data into the race_vars
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

# obtain data and feature geometry for the decennial US Census
options(tigris_use_cache = TRUE)
erie <- get_decennial(geography = "block", variables = race_vars, year=2020,
                      state = "NY", county = "Erie County", geometry = TRUE,
                      sumfile = "pl", cache_table=T) 

## data preparation
# crop the data into a specific region
erie_cropped <- st_crop(erie, xmin=-78.9,xmax=-78.85,ymin=42.888,ymax=42.92)

# convert the 'variable' into the factor
erie_cropped$variable <- as.factor(erie_cropped$variable)

# do the foreach loop
erie_cropped_filtered <- foreach(race = levels(erie_cropped$variable), .combine = rbind) %do% {
  
  # filter for the current racial group
  erie_filtered <- erie_cropped %>% filter(variable == race)
  
  # generate random points within each polygon based on 'value'
  points <- erie_filtered %>%
    st_sample(size = .$value) %>%
    st_as_sf() %>%                         # convert points to sf object
    mutate(variable = race)                # add a column for the racial group
}

# map it
mapview(erie_cropped_filtered, zcol = "variable", cex = 2)
