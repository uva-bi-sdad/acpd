library(lubridate)
library(sdalr)
pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)

db_data = function() {
  conn = con_db(dbname = 'acpd',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = 'clean_acpd_data') %>%
    data.table()
  on.exit(dbDisconnect(conn = conn))
  return(value = output)
}

test <- db_data()

upload_file = function(data) {
  conn = con_db(dbname = 'jbsc',
                pass = get_my_password())
  dbWriteTable(conn = conn,
               name = 'clean_acpd_data',
               value = data,
               row.names = FALSE,
               overwrite = TRUE)
  on.exit(expr = dbDisconnect(conn = conn))
}


acpd_data <- db_data()


# data$firstReportDtm <- as.Date(data$firstReportDtm)
# data$lastReportDtm <- as.Date(data$lastReportDtm)
# acpd_data$start <- as.Date(acpd_data$start)

acpd_data$year <- NA
acpd_data$month <- NA
acpd_data$day_of_week <- NA
acpd_data$hour <- NA

for (i in 1:nrow(acpd_data)) 
{
  acpd_data$year[i] <- as.character(year(acpd_data$start[i]))
  acpd_data$month[i] <- as.character(month(acpd_data$start[i]))
  acpd_data$day_of_week[i] <- as.character(lubridate::wday(acpd_data$start[i], label = TRUE, abbr = FALSE))
  acpd_data$hour[i] <- hour(acpd_data$start[i])
  print(i)
}

upload_file(data = test)

write.csv(test, "~/acpd/data/acpd/final/acpd_clean_data.csv")

  

