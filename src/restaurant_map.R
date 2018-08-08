pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)
library(sf)
library(ggmap)
library(ggplot2)
library(rgdal)
#arl_shape <- read_sf("~/sdal/projects/arl/arlington911/data/original/gis/Arlington_PoliceDistricts/Police_District.shp")
arl_shape <- readOGR(dsn = "~/sdal/projects/arl/arlington911/data/original/gis/Arlington_County_Polygon", layer = 'County_Polygon')
plot <- plot(arl_shape)
abc = function() {
  conn = con_db(dbname = 'acpd',
                pass = get_my_password())
  output = dbReadTable(con = conn,
                       name = 'geocoded') %>%
    data.table()
  on.exit(dbDisconnect(conn = conn))
  return(value = output)
}
arl_data <- abc()

arl_data <- filter(.data = arl_data, longitude > -77.25)

# coords <- arl_data[c("long", "lat")]
# coords <- coords[complete.cases(coords),]
# sp <- SpatialPoints(coords)

#restaurants_fortify <- fortify(arl_data, region="Arlington")
gg <- ggplot()
gg <- gg + geom_polygon(data=arl_shape, aes(x=long, y=lat, group=group), color = "black", fill = "white", size=0.5) 
gg <- gg + geom_point(data=arl_data, aes(x=longitude, y=latitude, color="red"))
gg <- gg +  coord_map()
gg


