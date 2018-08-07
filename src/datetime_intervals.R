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
