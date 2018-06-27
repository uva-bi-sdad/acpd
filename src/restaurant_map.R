pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)
library(sf)
arl_shape <- read_sf("~/sdal/projects/arl/arlington911/data/original/gis/Arlington_MetroStations/Metro_Stations.shp")


abc = function() {
  conn = con_db(dbname = 'oss',
                pass = get_my_password())
  output = dbReadTable(con = conn,
                       name = 'geocoded') %>%
    data.table()
  on.exit(dbDisconnect(conn = conn))
  return(value = output)
}
arl_data <- abc()
