library(lubridate)
data$firstReportDtm <- as.Date(data$firstReportDtm)
data$lastReportDtm <- as.Date(data$lastReportDtm)

data <- read.csv("~/acpd/data/TEST_acpd_open_data.csv")
data$year <- NA
data$month <- NA
data$day_of_week <- NA
data$hour <- NA


for (i in 1:nrow(data))
{
  data$year[i] <- as.character(year(data$firstReportDtm[i]))
  data$month[i] <- as.character(month(data$firstReportDtm[i]))
  data$day_of_week[i] <- as.character(wday(data$firstReportDtm[i], label = TRUE, abbr = FALSE))
  
}

data2 <- read.csv("~/acpd/data/filteredby_location_peaktime.csv")
