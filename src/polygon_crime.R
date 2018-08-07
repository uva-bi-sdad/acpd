pacman::p_load(sdalr, DBI, geojsonio)
block = geojson_read(x = 'https://opendata.arcgis.com/datasets/1ec04543da0546d38b63d8fd8e1019d5_25.geojson',
                     what = 'sp')

polygon_df <- block@data
block
View(block)

#Assigning block group polygons to lat long points
bg_sf <-
  sf::st_read(
    sdalr::con_db("sdal"),
    query = paste0(
      "SELECT * FROM geospatial$census_cb.cb_2016_51_bg_500k WHERE \"COUNTYFP\" = '013'"
    )
  )

pts <- data.table::fread("../acpd.git/categories_of_crime.R")
pts_sf <- sf::st_as_sf(pts, coords = c("longitude","latitude"))
sf::st_crs(pts_sf) <- sf::st_crs(bg_sf)

bg_pts_sf <- sf::st_join(pts_sf, bg_sf, join = st_intersects)

View(bg_pts_sf)

#Join Nearby Incidents with category_description dataset\
for_analysis <- for_analysis %>%
  left_join(nearby_incidents, by = c('longitude','latitude')) %>%
  arrange (nearby)


for_analysis <- subset(for_analysis, select = -id)
    
View(for_analysis)
table(for_analysis$nearby)
View(nearby)
names(nearby)
nearby = for_analysis %>%
  filter(nearby) %>%
  select(-nearby) %>%
  data.table() %>%
  setnames(old = 'Category', new = 'category')
 

#Drop and Add variables to crime category dataset

for_analysis <- data.frame(crime_by_category)
for_analysis <- subset(crime_by_category, select = -c(id, description, end))

names(crime_by_category)[names(crime_by_category) == 'start'] <- 'datetime'

View(for_analysis)

names(crime_by_category)

cords_in_clarendon <- nearby_incidents %>%
  mutate(nearby = map2_dbl(.x = longitude,
                           .y = latitude,
                           .f = compute_distance_from_hq_in_kim) <= 0.402336)

cords_in_clarendon <- nearby_incidents %>%
  filter(nearby) %>%
  mutate(id = str_c(longitude, latitude, sep = ' : ')) %>%
  pull(var = id) %>%
  unique()
View(cords_in_clarendon)

#Filter datetimes 9pm-3am Friday, Saturday, Sunday
#script for ACPD model


library(ggplot2)
for_analysis$start = as_datetime(for_analysis$datetime)
str(crime_by_category)
View(for_analysis2)
head(for_analysis2)
str(for_analysis2)
for_analysis2 <- crime_by_category %>%
  filter(str_c(longitude, latitude, sep = ' : ') %in% cords_in_clarendon) %>%
  mutate(hour = hour(x = datetime),
         minute = minute(datetime)) %>%
  filter((hour >= 21) | (hour < 3) | ((hour == 3) & (minute == 0))) %>%
  mutate(day = if_else(condition = ((hour >= 0) & (hour < 3)) | ((hour == 3) & (minute == 0)),
                       true = as.Date(datetime - days()),
                       false = as.Date(x = datetime)),
         wday = wday(x = day, label = TRUE)) %>%
  filter(wday %in% c('Fri', 'Sat', 'Sun')) %>%
  select(-c(hour, minute, datetime)) %>%
  data.table() %>%
  setnames(old = 'Category', new = 'category')



#write to database
write_for_analysis = function(data) {
  conn = con_db(dbname = 'acpd',
                pass = get_my_password())
  dbWriteTable(conn = conn,
               name = 'for_analysis',
               value = data,
               row.names = FALSE,
               overwrite = TRUE)
  on.exit(expr = dbDisconnect(conn = conn))
  }
write_for_analysis(data = for_analysis2)


str()









       















