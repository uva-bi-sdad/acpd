library(DBI)
library(RPostgreSQL)
library(sdalr)
library(ggplot2)
library(stringr)
library(dplyr)
library(maptools)

data <- read.csv("~/acpd/data/acpd/final/acpd_clean_data.csv")

arlington <- rgdal::readOGR("~/sdal/projects/arl/arlington911/data/original/gis/Arlington_County_Polygon/County_Polygon.shp")


relevant_crime_types <- c("PUBLIC DRUNKENNESS (DRUNK IN PUBLIC)",
                          "DUI",
                          "ASSAULT & BATTERY",
                          "PUBLIC SWEARING OR INTOXICATION (DRUNK IN PUBLIC)",
                          "DRINKING WHILE DRIVING / OPEN CONTAINER",
                          "ASSAULT & BATTERY: POLICE/FIRE/EMT/JUDGE/ETC OR BIAS MOTIVATED W/ BODILY INJURY",
                          "POSSESSION OF SCHEDULE I/II CONTROLLED SUBSTANCE",
                          "POSSESSION OF MARIJUANA",
                          "MANUFACTURE/SELL/POSSESS FAKE IDENTIFICATION",
                          "PARKING VIOLATION",
                          "RAPE",
                          "DRINKING ALCOHOL IN PUBLIC",
                          "SEXUAL BATTERY",
                          "DRUNKENNESS",
                          "DUI 3+ OFFENSE OR 2+ FELONY OFFENSE",
                          "REFUSAL OF BREATH/BLOOD TEST 2+ DUI OFFENSE W/IN 10Y",
                          "URINATING AND DEFACATING IN PUBLIC",
                          "RECKLESS DRIVING: GENERAL",
                          "ASSAULT OR BATTERY BY MOB",
                          "SELL/GIVE/DISTRIBUTE/PWID MARIJUANA: MISDEMEANOR",
                          "FORCIBLE SODOMY",
                          "REFUSAL OF BREATH/BLOOD TEST",
                          "POSSESSION OR DISTRIBUTION OF CONTROLLED (DRUG) PARAPHERNALIA",
                          "POSSESS/CONSUME ALCOHOL: 21YO/INTERDICTED/INTOXICATED",
                          "LICENSE: USE FICTITIOUS LIC TO OBTAIN ALCOHOL ",
                          "DRUNK   DRUNKENNESS",
                          "ASSAULT - OTHER",
                          "DWILIQR DRIVING UNDER INFLUENCE-LIQUOR",
                          "SMPLASLTSIMPLE ASSAULT POLICE OFFICER",
                          "NARCOTICMARIJUANA-POSSESSING",
                          "SIMPLE ASSAULT NON FAMILY",
                          "PUBPEACEPUBLIC PEACE - OTHER",
                          "DISORDERDISORDERLY CONDUCT",
                          "DRIVING UNDER INFLUENCE-LIQUOR",
                          "PUBLIC PEACE - OTHER",
                          "SIMPLE ASSAULT POLICE OFFICER",
                          "SIMPLE ASSAULT FAMILY",
                          "MARIJUANA-POSSESSING",
                          "MARIJUANA-SELLING",
                          "NARCOTICS COCAINE-SELLING",
                          "SYNTHETIC NARCOTICS-POSSESSING",
                          "LIQUOR POSSESSING",
                          "NARCOTICS COCAINE-POSSESSING",
                          "INDECENT EXPOSURE",
                          "RAPE-STRONG ARM",
                          "DRIVING UNDER INFLUENCE - OTHER",
                          "SEX OFFENSE - OTHER",
                          "DISTURBING PEACE",
                          "OTHER NON-MOVING TRAFFIC VIOLATION",
                          "DRIVING UNDER INFLUENCE-DRUGS",
                          "LIQUOR MISREPRESENTING AGE-MINOR")



data <- data %>% filter(description %in% relevant_crime_types)


month_var <- c(1,6,3)
day_of_wk <- c("Friday", "Saturday")

ggplot() +
  geom_polygon(data = arlington, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(ratio = 2) +
  geom_point(data = data %>%
               filter(longitude>(-77.172276) & longitude<(-77.032143) &
                        latitude>38.827357 & latitude<38.934343 &
                        month %in% month_var &
                        day_of_week %in% day_of_wk),
             aes(x = longitude, y = latitude, color = as.numeric(description)), size = .53, alpha = .5) +
  scale_colour_gradient(low = 'green', high = 'red') +
  coord_map()

