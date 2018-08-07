#install.packages('docstring, purrr, stringi, stringr, lubridate, geosphere, dplyr, data.table, dtplyr')

library(dplyr)
library(acs)
library(tigris)
library(sp)
library(rgdal)
library(maps)
library(scales)
library(ggplot2)
library(rgeos)
library(maptools)
library(gstat)

crime_data <- read.csv("./data/acpd/working/crime data.csv", stringsAsFactors = FALSE)

crime_report <- crime_data %>%
  mutate(crime_categories = str_detect(string = description,
                                       pattern =
                                         '(DRUNK|DUI|ASSAULT|POSSESSION|SEXUAL|DISORDERLY)'))
crime_report <- crime_report %>%
  filter(crime_categories == TRUE)

# Get GIS data
Arlington <- block_groups("Virginia", c("Arlington County"))
plot(Arlington)

#tell R that lat and long are actually lat and long, not just numbers
coordinates(crime_report) <- c('longitude', 'latitude')


#add column to crime report
head(Arlington@data)
proj4string(crime_report) <- proj4string((Arlington)) #projecting both projects on same projection 
crime_report$block_group <- over(crime_report, Arlington)$GEOID
class(crime_report)
head(crime_report@data)

#create data frame with crime report data

crime_df <- crime_report@data
class(crime_df)

#sum incidents in crime data
crime_incidents <- group_by(crime_df, block_group) %>%
  summarize(number_of_incidents = n())   #counts number of rows for each GEOID


#link numbers back to plot(Arlington)
#assign each census block a number (that number is the number of incidents)

# convert Roanoke block groups to a dataframe
Arlington@data$id = rownames(Arlington@data)
class(Arlington )
Arlington.points = fortify(Arlington, region ="id")
Arlington.df = left_join(Arlington.points, Arlington@data, by="id")
# merge with ACS data
ArlingtonACS<-left_join(crime_incidents,Arlington.df,by=c("block_group"="GEOID"))






ggplot(ArlingtonACS) + 
  geom_polygon(aes(x=long, y=lat,group=group,fill= number_of_incidents),
               alpha=1,color="grey70",lwd=.5) +
  scale_fill_gradient(low="white",high="red",name=expression("Number of \nAlcohol-Related Incidents")) +
  coord_equal(ratio=1) +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
        plot.caption = element_text(hjust=0)) + #labels
  labs(title="Alcohol Related Incidents in Arlington County", x="", y="", 
       caption="ACPD dataset 2015-2018")


