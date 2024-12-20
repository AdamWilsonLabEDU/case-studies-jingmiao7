Case Study 12
================
Jing Miao
Nov 19, 2024

# install and load necessary packages

``` r
#install.packages("htmlwidgets") #install.packages("widgetframe")

#install.packages("xts") #install.packages("dygraphs")
#install.packages("openmeteo")
```

``` r
library(tidyverse) 
library(htmlwidgets) 
library(widgetframe)
library(xts) 
library(dygraphs) 
library(openmeteo)
```

# download the weather dataset

``` r
d <- weather_history(c(43.00923265935055, -78.78494250958327),start =
"2023-01-01",end=today(),
daily=list("temperature_2m_max","temperature_2m_min","precipitation_sum"))%>%
mutate(daily_temperature_2m_mean=(daily_temperature_2m_max+daily_temperature_2m_min)/2)
```

# convert the data into xts

``` r
weather <- as.xts(d, order.by=d$date)
```

# plot

## the daily max temperature

``` r
dygraph(weather[, "daily_temperature_2m_max"], main = "Daily Maximum
Temperature in Buffalo, NY") %>% dyRangeSelector(dateWindow =
c("2023-01-01", "2024-10-31"))
```

![](case_study_12_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## the daily precipitation

``` r
dygraph(weather[, "daily_precipitation_sum"], main = "Daily
Precipitation in Buffalo, NY") %>% dyRangeSelector(dateWindow =
c("2023-01-01", "2024-10-31"))
```

![](case_study_12_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## the weather in one graph

``` r
dygraph(weather, main = "Daily Weather in Buffalo, NY") %>%
dyRangeSelector(dateWindow = c("2023-01-01", "2024-10-31"))%>%
dySeries("daily_temperature_2m_max", label = "Max Temp")%>%
dySeries("daily_temperature_2m_min", label = "Min Temp")%>%
dySeries("daily_temperature_2m_mean", label = "Average Temp")%>%
dySeries("daily_precipitation_sum", label = "Precipitation")
```

![](case_study_12_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
