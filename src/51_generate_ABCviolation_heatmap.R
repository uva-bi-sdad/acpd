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


abc_violations <- read.csv("./data/acpd/working/abc_cleaned copy.csv")


# Get GIS data
Arlington <- block_groups("Virginia", c("Arlington County"))
plot(Arlington)


coordinates(abc_violations) <- c('longitude', 'latitude')
proj4string(abc_violations) <- proj4string((Arlington)) #projecting both projects on same projection 
abc_violations$block_group <- over(abc_violations, Arlington)$GEOID
class(abc_violations)
violation_df <- abc_violations@data
class(violation_df)


Charges <- group_by(violation_df, block_group) %>%
  summarize(number_of_charges = n())   #counts number of rows for each GEOID


Arlington@data$id = rownames(Arlington@data)
class(Arlington )
Arlington.points = fortify(Arlington, region ="id")
Arlington.df = left_join(Arlington.points, Arlington@data, by="id")
# merge with ACS data
ArlingtonACS<-left_join(Charges,Arlington.df,by=c("block_group"="GEOID"))

ggplot() + 
  geom_polygon(data = ArlingtonACS, aes(x=long, y=lat,group=group,fill= number_of_charges),
               alpha=1,color="grey70",lwd=.5) +
  geom_path(data = Arlington.df, aes(x=long, y=lat, group = group),
            color="grey70",lwd=.5) +
  scale_fill_gradient(low="white",high="red",name=expression("Number of Violations")) +
  coord_equal(ratio=1) +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1, hjust = .5),
        plot.caption = element_text(hjust=0)) + #labels
  labs(title="ABC Violations", x="", y="", 
       caption="Virginia Alcoholic Beverage Control Authority Violation dataset 2013-2018")

