library(dplyr)
library(acs)
library(tigris)
library(sp)
library(rgdal)
library(maps)
library(scales)
library(ggplot2)
library(rgeos)
library(maptools)
library(gstat)

abc_violations <- read.csv("~/Desktop/abc_cleaned.csv", stringsAsFactors = FALSE)


abc_violations <- read_abc %>%
  mutate(crime_categories = str_detect(string = description,
                                       pattern =
                                         '(DRUNK|DUI|ASSAULT|POSSESSION|SEXUAL|DISORDERLY)'))
crime_report <- crime_report %>%
  filter(crime_categories == TRUE)

# Get GIS data
Arlington <- block_groups("Virginia", c("Arlington County"))
plot(Arlington)

#tell R that lat and long are actually lat and long, not just numbers
coordinates(crime_report) <- c('longitude', 'latitude')










