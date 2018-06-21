pacman::p_load(httr, readr, stringr, dplyr, data.table, dtplyr)
arlington_api_key = '140c712ce0aaf752160366975a7fa1cca8c81445'

crime_incidence = str_c('https://api.data.arlingtonva.us/api/v2/',
                        'datastreams/POLIC-INCID-LOG-19589/data.csv/?auth_key=',
                        arlington_api_key) %>%
  GET() %>%
  content(as = 'text', encoding = 'UTF-8') %>%
  read_csv() %>%
  data.table()


pacman::p_load(docstring)

night_life = function(event) {
  #' Determines if the event is within a nightlife time.
  #' 
  #' @description Is the event during a weekend or a special day?
  #' 
  #' Nightlife is defined as:
  #' 
  #' - Friday between 21:00 - 24:00
  #' 
  #' - Saturday between 00:00 - 03:00 and 21:00 - 24:00
  #' 
  #' - Sunday between 00:00 - 03:00 and 21:00 - 24:00
  #' 
  #' - Monday between 00:00 - 03:00
  #' 
  #' - Saint Patrick's Day (03-17) between 21:00 - 24:00
  #' 
  #' - Day after Saint Patrick's Day (03-17) between 00:00 - 03:00
  #' 
  #' - 5 de mayo (05-05) between 21:00 - 24:00
  #' 
  #' - Day after 5 de mayo (05-06) between 00:00 - 03:00
  #' 
  #' @param event a POSIXct object
  #' @usage night_life(event)
  #' @return Boolean indicator
  
  tz = attr(event, 'tzone')
  # Helpers
  pre_game = function(ocassion) {
    output = hour(x = ocassion) >= 21L
    return(value = output)
  }
  party_time = function(occasion) {
    tz = attr(occasion, 'tzone')
    h = hour(x = event)
    if (h < 3L) {
      output = TRUE
    } else if ((h %in% 3L) & (minute(x = event) %in% 0L))
      output = TRUE
    else {
      output = event %within% interval(start = as.POSIXct(x = str_c(date(x = event),
                                                                    ' 21:00:00'),
                                                          tz = tz),
                                       end = as.POSIXct(x = str_c(date(x = event) + days(),
                                                                  ' 03:00:00'),
                                                        tz = tz))
    }
    return(value = output)
  }
  # HIMYM reference S1E18
  nothing_good_happens_after_2 = function(occasion) {
    tz = attr(event, 'tzone')
    output = occasion <= as.POSIXct(x = str_c(date(x = event),
                                              ' 03:00:00'),
                                    tz = tz)
  }
  month_day_wday = function(date) {
    output = c(month(x = date), day(x = date), wday(x = date, week_start = 7))
    return(value = output)
  }
  month_day_wday = month_day_wday(date = event)
  output = FALSE
  if ((month_day_wday[3] %in% 6L) |
      (any(map_lgl(.x = list(c(3L, 17L),c(5L, 5L)),
                   .f = function(x) {
                     all(x %in% month_day_wday[1:2])
                   })))) {
    output = pre_game(ocassion = event)
  } else if ((month_day_wday[3] %in% 2L) |
             (any(map_lgl(.x = list(c(3L, 18L),c(5L, 6L)),
                          .f = function(x) {
                            all(x %in% month_day_wday[1:2])
                          })))) {
    output = nothing_good_happens_after_2(occasion = event)
  } else if ((month_day_wday[3] %in% c(1L, 7L))) {
    output = party_time(occasion = event)
  }
  return(value = output)
}
library(lubridate)
is_it_sunday <- function(x) {
  wday(x, label = TRUE) %in% 'Sun'
}
is_it_sunday(ymd("2018-06-10"))

is_it_monday = function(x) {
  wday(x, label = TRUE) %in% 'Mon'
}
is_it_sunday(ymd('2018-06-10'))

?wday
wday(today(), label = TRUE, abbr = FALSE)

is_it_cinco = function(x) {
  ((day(x) == 5) & (month(x) == 5))
}

is_it_cinco(ymd("2018-05-05"))

df = crime_incidence %>%
  mutate(alc_crime = str_detect(string = offenseDsc, pattern = '[(DRUNK)|(DUI)]'))
names(crime_incidence)

View(df)

df = crime_incidence %>%
  mutate(alc_crime = str_detect(string = offenseDsc, pattern = '[(DRUNK)|(DUI)]'))
names(crime_incidence)

str_detect(string = crime_incidence$offenseDsc[1],
           pattern = '((DRUNK)|(DUI))')
str_detect(string = crime_incidence$offenseDsc[45],
           pattern = '((DRUNK)|(DUI))')

str_detect(string = crime_incidence$offenseDsc[2],
           pattern = '((DRUNK)|(DUI))')

install.packages("swirl")
library(swirl)
install_course("R Programming")
swirl()




