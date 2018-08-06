# Code to include in the dashboard a map for restaurants and crimes

#### Reading Spatial Data ####
# https://gisdata-arlgis.opendata.arcgis.com/datasets/police-district
# geo = geojson_read(x = 'https://opendata.arcgis.com/datasets/1ec04543da0546d38b63d8fd8e1019d5_21.geojson',
#                    what = 'sp')
# https://gisdata-arlgis.opendata.arcgis.com/datasets/census-blocks-2010
# geo = geojson_read(x = 'https://opendata.arcgis.com/datasets/1ec04543da0546d38b63d8fd8e1019d5_25.geojson',
#                    what = 'sp')
blocks = readRDS(file = 'arlington_census_blocks.RDS')

#### Loading Crime data ####
# Reads the crime data from 2015-01-01 to 2017-12-31 within a 0.25 miles radius
# of the Clarendon Metro Station for 'Aggravated Assault', 'DUI',
# 'Disorderly Conduct', 'Drunkenness', 'Sexual Assault/Rape', and
# 'Underage Drinking/Fake ID'.
read_crime = function() {
  conn = con_db(dbname = 'acpd',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = 'for_analysis') %>%
    select(category, day, longitude, latitude) %>%
    filter((day < as.Date(x = '2018-01-01')) &
             (category %in% c('Aggravated Assault', 'DUI', 'Disorderly Conduct',
                              'Drunkenness', 'Sexual Assault/Rape', 
                              'Underage Drinking/Fake ID')) &
             (map2_lgl(.x = longitude,
                       .y = latitude,
                       .f = function(lon, lat) {
                         distm(x = c(-77.09523, 38.8871),
                               y = c(lon, lat)) < 402.336
                       })) &
             complete.cases(.)) %>%
    data.table() %>%
    mutate(label = str_c('Date: ', day, '<br>', 'Category: ', category),
           type = 'Incident') %>%
    select(category, label, longitude, latitude) %>%
    st_as_sf(coords = c('longitude', 'latitude'))
  on.exit(expr = dbDisconnect(conn = conn))
  return(value = output)
}
crime = read_crime()

#### Loading Restaurant Data ####
# Loads the restaurant data for active ABC-licensed (beer | wine) in the area
# of interest. It also identifies ARI from non ARI-restaurants.
read_vabc = function() {
  conn = con_db(dbname = 'acpd',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = 'vabc_arlington_restaurants') %>%
    filter(privilege_status %in% 'Active') %>%
    filter(str_detect(string = privilege_description, pattern = '(Wine|Beer)')) %>%
    select(trade_name, longitude, latitude) %>%
    mutate(trade_name = str_to_title(string = trade_name) %>%
             str_trim()) %>%
    unique() %>%
    filter(map2_lgl(.x = longitude,
                    .y = latitude,
                    .f = function(lon, lat) {
                      distm(x = c(-77.09523, 38.8871),
                            y = c(lon, lat)) < 402.336
                    })) %>%
    mutate(ARI = ifelse(test = trade_name %in% c('Whitlows On Wilson',
                                                 'Wilson Hardware',
                                                 'Bar Bao',
                                                 'Pamplona'),
                        yes = 'ARI Restaurant',
                        no = 'Non-ARI Restaurant')) %>%
    data.table(key = 'trade_name') %>%
    setnames(old = 'trade_name', new = 'restaurant') %>%
    select(restaurant, ARI, longitude, latitude) %>%
    st_as_sf(coords = c('longitude', 'latitude'))
  on.exit(expr = dbDisconnect(conn = conn))
  return(value = output)
}
vabc = read_vabc()

#### Map for Dashboard ####
factpal = colorFactor(rainbow(n = 2), vabc$ARI)
m = leaflet(options = leafletOptions(minZoom = 14.75, maxZoom = 20)) %>%
  addTiles() %>%
  addCircles(lng = -77.09523, lat = 38.8871, weight = 1,
             radius = 402.336) %>%
  addMarkers(data = crime,
             clusterOptions = markerClusterOptions(),
             popup = ~label) %>%
  addCircles(data = vabc,
             color = ~factpal(ARI),
             label = ~(restaurant),
             labelOptions = labelOptions(noHide = F, direction = 'auto')) %>%
  addLegend(pal = factpal,
            values = vabc$ARI)
m
