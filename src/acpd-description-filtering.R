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
                          "ASSULT & BATTERY",
                          "ELUDING POLICE -ENDANGER PERSONS OR POLICE CAR",
                          "PUBLIC SWEARING OR INTOXICATION (DRUNK IN PUBLIC)",
                          "HIT AND RUN - UNATTENDED PROPERTY: DAMAGE ONLY",                                           
                          "HIT AND RUN - ATTENDED PROPERTY: DAMAGE $1,000",                                          
                          "DRINKING WHILE DRIVING / OPEN CONTAINER",
                          "INTENTIONALLY DESTROY/DEFACE/DAMAGE PROPERTY $1000
                          (VANDALISM/DESTRUCTION OF PROPERTY)",
                          "POSSESSION OF SCHEDULE I/II CONTROLLED SUBSTANCE",
                          "POSSESSION OF SCHEDULE III/IV/V/VI/CANNABIMIMETIC AGENT",
                          "SEXUAL BATTERY",
                          "ASSAULT & BATTERY: FAMILY MEMBER",
                          "POSSESSION OF MARIJUANA",
                          "DISORDERLY CONDUCT",
                          "ASSAULT & BATTERY: POLICE/FIRE/EMT/JUDGE/ETC 
                          OR BIAS MOTIVATED W/ BODILY INJURY",
                          )







