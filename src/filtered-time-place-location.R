acpd <- read.csv("~/git/acpd/data/initial_filtering.csv")
pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)
get_crime = function() {
  #' Getting some_data from the database.
  #' 
  #' @example some_data()
  conn = con_db(dbname = 'acpd',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = 'crime') %>%
    data.table()
  on.exit(dbDisconnect(conn = conn))
  return(value = output)
}

acpd_data <- get_crime()

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
crime_type = crime_type %>%
  filter(nearby & (description %in% relevant_crime_types)) %>%
  mutate(wday = wday(start, label = TRUE),
         time_of_day = ifelse(test = (5 <= hour(start)) & (hour(start) < 17),
                              yes = 'Day Time',
                              no = 'Night Time'),
         time = str_c(wday, time_of_day, sep = ' '))
map(.x = group_by(.data = crime_type),
    .f = function(df) {
      data.table(table(df$time), key = 'N')
      })
helper = function(df) {
  data.table(table(df$time), key = 'N')
}

helper(crime_type %>%
         filter(year(start) %in% 2015L))
helper(crime_type %>%
         filter(year(start) %in% 2016L))
helper(crime_type %>%
         filter(year(start) %in% 2017L))
helper(crime_type %>%
         filter(year(start) %in% 2018L))
crime_type = crime_type %>%
  mutate(data_year = ifelse(test = start %within% interval(start = as.Date('2015-06-01'),
                                                           end = as.Date('2016-05-30')),
                            yes = 2015,
                            no = ifelse(test = start %within% interval(start = as.Date('2016-06-01'),
                                                                       end = as.Date('2017-05-30')),
                                        yes = 2016,
                                        no = 2017)))
output = plyr::ddply(.data = crime_type,
                     .variables = 'data_year',
                     .fun = function(df) {
                       output = data.table(table(df$time))
                     }) %>%
  data.table(key = c('data_year', 'N')) %>%
  setnames(old = 'V1', new = 'time') %>%
  setnames(old = 'N', new = 'count')
helper(crime_type %>%
         filter(start %within% interval(start = as.Date('2015-06-01'),
                                        end = as.Date('2016-05-30'))))
helper(crime_type %>%
         filter(start %within% interval(start = as.Date('2016-06-01'),
                                        end = as.Date('2017-05-30'))))
helper(crime_type %>%
         filter(start %within% interval(start = as.Date('2017-06-01'),
                                        end = as.Date('2018-05-30'))))
max(crime_type$start)

# ------------


?group_by

crime_type %>%
  filter(description %in% relevant_crime_types) %>%
  mutate(year = year(end)) %>%
  # select(description, year) %>%
  group_by(description, year) %>%
  do(function(df) {
    output = data.table(df$description[1],
                        df$year[1],
                        n = nrow(df))
    })

table(crime_type %>%
        filter(description %in% relevant_crime_types) %>%
        pull(end) %>%
        year())


# need to keep the rows that column 'nearby' is TRUE
nearby_incidents <- filter(crime_type, nearby %in% TRUE)
nearby_incidents$yearOfCrime = year(nearby_incidents$start)

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


nearby_incidents = filter(nearby_incidents, yearOfCrime %in% 2015:2017) %>%
  filter(description %in% relevant_crime_types)

View(table(nearby_incidents$description, nearby_incidents$yearOfCrime))


filtered <- acpd %>% filter(description %in% relevant_crime_types)

write.csv(filtered, "~/git/acpd/data/filtered_by_location_type_time.csv")

library(ggplot2)
##ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))




#date(x) and count(y) scatter plot
#color by the day of the week 

### Filter out just date, choosing start date
class(crime_type$start)
crime_type$start_date <- ymd(as.Date(crime_type$start))
crime_type$end_date <- ymd(as.Date(crime_type$end))

View(crime_type)

chk = data.table(crime_type) %>%
  setnames(old = 'start', new = 'startdate')
chk[, count := nrow(df), by = startdate] %>%
  data.table()

names(chk)
chk = chk %>%
  select(startdate, count) %>%
  unique()
  
  plyr::ddply(.data = crime_type,
                  .variables = 'start',
                  .fun = function(df) {
                    output = data.table(date = df$start[1L],
                                        count = nrow(df))
                  })
View(chk)
ggplot(data = crime_type,
       mapping = aes(x = start_date, color = wday(start_date))) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
head(crime_type)

chk = data.table(table(crime_type$start_date))
names(chk)

ggplot(data = chk,
       mapping = aes(x = V1,
                     y = N)) +
  geom_point()

#plot <- ggplot(crime_type, aes(x = ))

crime_type = crime_type %>%
  mutate(type_of_day = wday(start_date, label = TRUE) %in%
           c('Sun', 'Thu', 'Fri', 'Sat'))
dat = plyr::ddply(.data = crime_type,
                  .variables = 'start_date',
                  .fun = function(df) {
                    output = data.table(start_date = df$start_date[1],
                                        type_of_day = df$type_of_day[1],
                                        count = nrow(df))
                  })
names(crime_type)
table(crime_type$type_of_day)

ggplot(data = dat,
       mapping = aes(x = type_of_day,
                     y = count)) +
  geom_boxplot()

ggplot(crime_type) + geom_boxplot()
library(stringr)
filtered = filtered %>%
  mutate(ym = str_c(year(start), '-', month(start)))
table(filtered$`str_c(year(start), "-", month(start))`)

#create a data table like the 1st and 3rd ones on slack based on weekday, hour, and tabel

library(lubridate)
# crime_type$wday <- NA
# crime_type$data_year <- NA
# 
# for (i in 1:nrow(filtered))
# {
#   filtered$day_name[i] <- as.character(wday(filtered$start[i], label = TRUE, abbr = FALSE))
#   filtered$year[i] <- as.character(year(filtered$start[i]))
# }

# ^^ can be replaced by = mutate(wday = wday(start, label = TRUE),

####################TIME AND DAY OF WEEK#####################################

library(ggplot2)

crime_type = crime_type %>%
  filter(nearby & (description %in% relevant_crime_types)) %>%
  mutate(wday = wday(start, label = TRUE),
         time_of_day = ifelse(test = (5 <= hour(start)) & (hour(start) < 17),
                              yes = 'Day Time',
                              no = 'Night Time'),
         time = str_c(wday, time_of_day, sep = ' '))

p = ggplot(data = crime_type) + 
  geom_bar(mapping = aes(x = wday, fill = time_of_day)) + 
  facet_wrap(~ data_year) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Crime Count by Time and Day of the Week", subtitle = NULL) +
  xlab("Weekday") + ylab("Count") #+
  #scale_fill_discrete(name="Time of Day")
p
  
ggsave(filename = 'count_crimes_time.png',
       plot = p)
?ggsave
getwd()
list.files(path = './data/acpd/final')

#below is the table of count per day/time

library(gridExtra)
library(grid)
d = (output)
grid.table(d)


