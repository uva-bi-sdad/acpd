library(readxl)
library(readr)
library(lubridate)
library(ggplot2)

some_data = function() {
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


#coordinates for Clarendon metro station
compute_distance_from_hq_in_kim <- function(lon, lat) {
  output = distm(x = c(lon, lat),
                 y = c(-77.0950, 38.8872),
                 fun = distVincentyEllipsoid) / 1000
  return(value = output)
}



acpd_data <- some_data()
crime_categories <- read_excel('crime_categories.xlsx')
crime_data <- read_csv('crime data.csv')


crime_type <- acpd_data %>%
  mutate(nearby = map2_dbl(.x = longitude,
                           .y = latitude,
                           .f = compute_distance_from_hq_in_kim) <= 0.804672)



nearby_incidents <- filter(crime_type, nearby %in% TRUE)

nearby_clarendon <- nearby_incidents %>%
  mutate(nearby = map2_dbl(.x = longitude,
                           .y = latitude,
                           .f = compute_distance_from_hq_in_kim) <= 0.402336)





crime_by_category <- crime_data %>%
  dplyr::inner_join(crime_categories, by = c('description' = 'Description'))
crime_by_category


# create 1 dataset w/ nearby_clarendon and crime_by category -----

nearby_crime_clarendon <- nearby_clarendon %>%
  tibble::as_data_frame() %>%
  dplyr::inner_join(crime_by_category, by = 'id')


# filter for nearby == TRUE

filtered <- nearby_crime_clarendon %>% 
  filter(nearby == TRUE) %>%
  mutate(wkday = wday(start.x)) %>%
  filter(wkday %in% c(6, 7, 1), 
         year(start.x) %in% c(2015, 2016, 2017))

y_wd_td <- recode_time_interval(filtered$start.x)

combined_data <- cbind(filtered, y_wd_td)

combined_filtered <-  combined_data %>%
  select(Category, longitude.y, latitude.y, start.x, year, wkday, wday, time_of_day)
combined_filtered


#Create Bar Graph


ggplot(combined_filtered, mapping = aes(x = wday, 
                                        fill = time_of_day)) +
  geom_bar() +
  scale_x_discrete(limits = c('Fri', 'Sat', 'Sun')) +
  labs(title = 'Alcohol-Related Crime in Clarendon',
       x = 'Weekday',
       y = 'Total Crime Count',
       fill = 'Time of Day') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Time of Day",
                           breaks = c("morning", "midday", "evening", "night"),
                           labels = c("Morning\n3:00-9:00\n",
                                      "Midday\n9:00-15:00\n",
                                      "Evening\n15:00-21:00\n",
                                      "Night\n21:00-3:00")) +
  
  
  facet_wrap(~year)

  
  
  
  
  ggsave(filename = './figures/crime_count.jpeg')
list.files('./figures')
  





















