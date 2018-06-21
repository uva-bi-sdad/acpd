library(lubridate)
library(dplyr)
# Need to find a way to download the data using the url to be able to get the most recent data each time you run the script
# Worst-case scenario is to have to download and then upload the data to R/ Lightfoot

# Arlington County Open Data - Police Incident Log
#police_dta <- read.csv(url('http://bit.ly/21nPzoE')) <- for this to work the url needs to lead directly to a csv file

# Let's try to AC OD API
# api_key <- "23cb052a88d6cdc8b1b1831a9909869d0c9b5093"
# 
# police_dta <- paste('https://api.data.arlingtonva.us/api/v2/datastreams/POLIC-INCID-LOG-19589/data.json/?auth_key=', api_key, '&limit=50', sep="")

pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)
some_data = function() {
  conn = con_db(dbname = 'jbsc',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = 'crime') %>%
    data.table()
  on.exit(dbDisconnect(conn = conn))
  return(value = output)
}
data <- some_data()


police_dta <- read.csv("~/acpd/data/TEST_acpd_open_data.csv")

filter_vars <- c('offenseDsc', 'latitudeCrd', 'longitudeCrd', 'firstReportDtm')
police_dta_filtered <- police_dta[filter_vars]

police_dta_filtered$firstReportDtm <- gsub("T", " ", police_dta_filtered$firstReportDtm)

peak_times <- police_dta_filtered[1, ]
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

