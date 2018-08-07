
acpd <- read.csv("./data/acpd/working/crime data.csv")
pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)
#' some_data = function() {
#'   #' Getting some_data from the database.
#'   #' 
#'   #' @example some_data()
#'   conn = con_db(dbname = 'acpd',
#'                 pass = get_my_password())
#'   output = dbReadTable(conn = conn,
#'                        name = 'crime') %>%
#'     data.table()
#'   on.exit(dbDisconnect(conn = conn))
#'   return(value = output)
#' }

#acpd_data <- some_data()

pacman::p_load(httr, readr, stringr, dplyr, data.table, dtplyr)
pacman::p_load(docstring, purrr, stringi, stringr, lubridate, geosphere, dplyr, data.table, dtplyr)


#Clarendon metro station coordinates
compute_distance_from_hq_in_kim <- function(lon, lat) {
  output = distm(x = c(lon, lat),
                 y = c(-77.0950, 38.8872),
                 fun = distVincentyEllipsoid) / 1000
  return(value = output)
}


for_analysis <- data.frame(crime_by_category)
for_analysis <- subset(crime_by_category, select = -c(id, description, end))
#nearby_incidents comes from '01_arl_open_data_cleaning.R' in code chunk that includes police_dta_nearby
nearby_incidents <- filter(crime_type, nearby %in% TRUE)

nearby_clarendon <- nearby_incidents %>%
  mutate(nearby = map2_dbl(.x = longitude,
                           .y = latitude,
                           .f = compute_distance_from_hq_in_kim) <= 0.402336)



bar_table <- for_analysis %>% filter(latitude & longitude %in% nearby_incidents)

#saveRDS(object = filtered, file = './data/acpd/working/filtered_by_location_type_time.RDS')

filtered1 = readRDS(file = './data/acpd/working/filtered_by_location_type_time.RDS')

library(ggplot2)
##ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

#date(x) and count(y) scatter plot
#color by the day of the week 

### Filter out just date, choosing start date
class(crime_by_category$datetime)
crime_by_category$datetime <- ymd(as.Date(crime_by_category$datetime))
# for_analysis$end_date <- ymd(as.Date(crime_type$end))

View(crime_by_category)

chk = data.table(crime_by_category) 
  
chk[, count := nrow(df), by = datetime] %>%
  data.table()

# graph_table <- nearby_clarendon %>%
#   filter(nearby) %>%
#   mutate(id = str_c(longitude, latitude, sep = ' : ')) %>%
#   pull(var = id) %>%
#   unique()

nearby_clarendon <- nearby_incidents %>%
  mutate(nearby = map2_dbl(.x = longitude,
                           .y = latitude,
                           .f = compute_distance_from_hq_in_kim) <= 0.402336)

for_analysis <- nearby_clarendon %>%
  left_join(nearby_clarendon, by = c('longitude','latitude')) 
  
 for_analysis <- nearby_clarendon %>%
    select(-'description', -'id', -'end') %>%
   left_join(nearby_clarendon, by = ('longitude' & 'latitude'))
 
 
 
  filter(nearby) %>%
  select(-nearby) %>%
  data.table()


names(chk)

plyr::ddply(.data = crime_type,
            .variables = 'start',
            .fun = function(df) {
              output = data.table(date = df$start[1L],
                                  count = nrow(df))
            })

#chk = chk %>%
#select(stardate, count %>%                   #do we need this code?
# unique()

View(chk)


ggplot(data = for_analysis,
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
           c('Sun','Fri', 'Sat'))
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
