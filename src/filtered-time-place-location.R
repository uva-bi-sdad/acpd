acpd <- read.csv("~/git/acpd/data/initial_filtering.csv")
pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)
some_data = function() {
  #' Getting some_data from the database.
  #' 
  #' @example some_data()
  conn = con_db(dbname = 'jbsc',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = '') %>%
    data.table()
  on.exit(dbDisconnect(conn = conn))
  return(value = output)
}

acpd_data <- some_data()

pacman::p_load(httr, readr, stringr, dplyr, data.table, dtplyr)
pacman::p_load(docstring, purrr, stringi, stringr, lubridate, geosphere, dplyr, data.table, dtplyr)

compute_distance_from_hq_in_kim <- function(lon, lat) {
  output = distm(x = c(lon, lat),
                 y = c(-77.0950, 38.8872),
                 fun = distVincentyEllipsoid) / 1000
  return(value = output)
}

crime_type <- acpd_data %>%
  mutate(nearby = map2_dbl(.x = longitude,
                           .y = latitude,
                           .f = compute_distance_from_hq_in_kim) <= 0.804672)

# need to keep the rows that column 'nearby' is TRUE
nearby_incidents <- filter(crime_type, nearby %in% TRUE)

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



filtered <- acpd %>% filter(description %in% relevant_crime_types)

write.csv(filtered, "~/git/acpd/data/filtered_by_location_type_time.csv")










