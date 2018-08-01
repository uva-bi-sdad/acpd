pacman::p_load(httr, readr, stringr, dplyr, data.table, dtplyr)
pacman::p_load(docstring, purrr, stringi, stringr, lubridate, geosphere, dplyr, data.table, dtplyr)
pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)
library(lubridate)
library(dplyr)
 
some_data = function() {
  conn = con_db(dbname = 'jbsc',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = 'crime') %>%
    data.table()
  on.exit(dbDisconnect(conn = conn))
  return(value = output)
}
police_dta <- some_data()


compute_distance_from_hq_in_kim <- function(lon, lat) {
  output = distm(x = c(lon, lat),
                 y = c(-77.0950, 38.8872),
                 fun = distVincentyEllipsoid) / 1000
  return(value = output)
}

police_dta_nearby <- police_dta %>%
  mutate(nearby = map2_dbl(.x = longitude,
                           .y = latitude,
                           .f = compute_distance_from_hq_in_kim) <= 0.804672)

# need to keep the rows that column 'nearby' is TRUE
nearby_incidents <- filter(police_dta_nearby, nearby %in% TRUE)



peak_times <- nearby_incidents[1, ]
pos <- 1;

peak_drinking_times <- function(time, row_num)
{
  if((wday(time, label = TRUE) %>% "Thur") && (hour(time) %in% c('21', '22', '23', '24'))){
    pos = pos + 1
    return(police_dta_filtered[row_num, ])
  } else if((wday(time, label = TRUE) %in% "Fri") && (hour(time) %in% c('0', '1', '2', '3', '21', '22', '23', '24'))){
    pos = pos + 1
    return(police_dta_filtered[row_num, ])
  } else if((wday(time, label = TRUE) %in% "Sat") && (hour(time) %in% c('0', '1', '2', '3', '21', '22', '23', '24'))){
    pos = pos + 1
    return(police_dta_filtered[row_num, ])
  } else if ((wday(time, label = TRUE) %in% "Sun") && (hour(time) %in% c('0', '1', '2', '3', '21', '22', '23', '24'))){
    pos = pos + 1
    return(police_dta_filtered[row_num, ])
  } else if ((wday(time, label = TRUE) %in% "Mon") && (hour(time) %in% c('0', '1', '2', '3'))){
    pos = pos + 1
    return(police_dta_filtered[row_num, ])
  } 
}

for (i in 1:nrow(police_dta_filtered))
{
  peak_times <- rbind(peak_times, peak_drinking_times(police_dta_filtered$firstReportDtm[i], i), make.row.names = FALSE)
}

#peak_times <- filter(peak_times, firstReportDtm = c(''))

