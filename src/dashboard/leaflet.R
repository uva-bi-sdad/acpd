

make_crime_map <- function() {
  crime_types <-
    c(
      "Aggravated Assault",
      "Disorderly Conduct",
      "Drunkenness",
      "DUI",
      "Sexual Assault/Rape"
    )
  crime_years <- c(2015, 2016, 2017, 2018)
  
  get_census_block_year_crime_type_count <- function() {  
    if (file.exists("map_polys_sf.RDS")) {
      # Get Polygons (CENSUS blocks)
      conn <- DBI::dbConnect(drv =RPostgreSQL::PostgreSQL(),
                             dbname = "acpd",
                             host = "postgis_1",
                             port = 5432L,
                             user = Sys.getenv("db_userid"),
                             password = Sys.getenv("db_pwd"))
      census_blocks <- sf::st_read(dsn = conn, layer = "arlington_census_blocks")
      
      police_incidents <- dbReadTable(conn = conn,
                                      name = "incidents_filtered") %>%
        dt_select(id, start, crime_cat, nearby, day, hour, nightlife, longitude, latitude) %>%
        dt_mutate(crime_year = year(x = start)) %>%
        setnames(old = "crime_cat", new = "crime_categories") %>%
        st_as_sf(coords = c("longitude", "latitude")) %>%
        st_set_crs(value = st_crs(x = census_blocks))
      
      skeleton <- expand.grid(na.omit(object = unique(x = census_blocks$FULLBLOCKID)),
                              na.omit(object = unique(x = police_incidents$crime_year)),
                              na.omit(object = unique(x = police_incidents$crime_categories))) %>%
        data.table() %>%
        setNames(nm = c("GEOID10", "crime_year", "crime_categories"))
      st_join(x = census_blocks,
              y = police_incidents) %>%
        dt_select(FULLBLOCKID, crime_year, crime_categories, id) %>%
        setnames(old = "FULLBLOCKID", new = "GEOID10") %>%
        merge(y = skeleton, all.y = TRUE) %>%
        dt_mutate(count := sum(x = !is.na(x = id)),
                  by = c("GEOID10", "crime_year", "crime_categories")) %>%
        dt_select("GEOID10", "crime_year", "crime_categories", "count") %>%
        unique() %>%
       dcast(GEOID10 + crime_year ~ crime_categories, value.var = "count") %>%  
        merge(y = census_blocks %>%
                dt_select(FULLBLOCKID, geometry) %>%
                setnames(old = "FULLBLOCKID", new = "GEOID10"),
              by = "GEOID10") %>%
        dt_arrange(GEOID10, crime_year) %>% st_as_sf() 
    }
  }
  
  census_block_year_crime_type_count <- get_census_block_year_crime_type_count()
  
  saveRDS(census_block_year_crime_type_count, "map_polys_sf.RDS")
  
  # Get Points (restaurant locations)
  conn <- DBI::dbConnect(drv =RPostgreSQL::PostgreSQL(),
                         dbname = "acpd",
                         host = "postgis_1",
                         port = 5432L,
                         user = Sys.getenv("db_userid"),
                         password = Sys.getenv("db_pwd"))
  restaurants <- DBI::dbReadTable(conn = conn,
                                  name = 'vabc_arlington_restaurants') %>%
    data.table::data.table() %>% dt_mutate(priv = str_detect(string = PrivDesc,
                                                             pattern ="(?i)(wine|beer)")) %>% 
    dt_filter(priv) %>% dt_filter(LicStatus_StatusDesc %in% 'Active')
  
  restaurants <- sf::st_as_sf(restaurants, coords = c("X", "Y"))
  pnts_2_sf <- restaurants
  
  
  # Prepare Point Dataset for Mapping
  saveRDS(pnts_2_sf, "map_pnts_sf.RDS")
  
  
  # Prepare Second Point Dataset for Mapping
  within_circle <- function(lon, lat, ctr_pnt = 402.336) {
    geosphere::distm(x = c(-77.09523, 38.8871),
                     y = c(lon, lat)) < ctr_pnt
  }
  . <- cbind(pnts_2_sf, sf::st_coordinates(pnts_2_sf))
  .$in_circle <- mapply(within_circle, .$X, .$Y)
  .$ARI <- !is.na(.$ARIAccredit)
  map_pnts_2_sf <- .
  
  saveRDS(map_pnts_2_sf, "map_pnts_2_sf.RDS")
  
  # Load Data Files
  print("Loading Data Files...")
  map_polys_sf <- readRDS("map_polys_sf.RDS")
  map_pnts_sf <- readRDS("map_pnts_sf.RDS")
  map_pnts_2_sf <- readRDS("map_pnts_2_sf.RDS")
  
  if (TRUE) {
    # Map Polygons and Points
    # color palette function
    pal <- leaflet::colorBin(
      palette = "viridis",
      bins = c(0, 3, 6, 12, 24, 48),
      reverse = TRUE
    )
    pal2 <- leaflet::colorFactor(c("gray17", "darkblue"),
                                 map_pnts_2_sf$ARI)
    
    # map
    print("Building Map...")
    m <- leaflet::leaflet()
    m <- leaflet::setView(m,-77.09500, 38.88700, 17)
    m <- leaflet::addTiles(m)
    m <- leaflet::addMapPane(m, "base_layers", zIndex = 410)
    m <- leaflet::addMapPane(m, "boundaries", zIndex = 420)
    m <- leaflet::addMapPane(m, "under_places", zIndex = 405)
    m <- leaflet::addMapPane(m, "places", zIndex = 440)
    
    # add polygon data layers
    print("Adding Polygon Layers...")
    for (c in crime_types) {
      for (y in crime_years) {
        plydt <-
          dplyr::filter(map_polys_sf, crime_year == y)[, c(c, "GEOID10")]
        
        labels <- lapply(
          paste("<strong>year:",
                y,
                "</strong><br />", "county:",
                substr(plydt$GEOID10, 3, 5),
                "</strong><br />",
                "tract:",
                substr(plydt$GEOID10, 6, 11),
                "<br />",
                "block group:",
                substr(plydt$GEOID10, 12, 12),
                "<br />",
                "crime type:",
                c,
                "<br />",
                "measure: count<br />",
                "value:",
                plydt[, c][[1]]
          ),
          htmltools::HTML
        )
        m <- leaflet::addPolygons(
          m,
          data = plydt,
          stroke = TRUE,
          weight = .8,
          color = "Black",
          smoothFactor = 0.2,
          fillOpacity = .6,
          fillColor = ~ pal(get(c)),
          label = labels,
          group = paste(c, y),
          options = leaflet::pathOptions(pane = "base_layers")
        )
      }
    }
    # add point data layers
    print("Adding Point Layers...")
    for (c in crime_types) {
      for (y in crime_years) {
        pnt_dt <-
          map_pnts_sf[map_pnts_sf$crime_year == y &
                        map_pnts_sf$crime_type == c, ]
        
        labels <- lapply(
          paste(
            "<strong>crime description:",
            pnt_dt$crime_description,
            "</strong><br />",
            "crime date:",
            pnt_dt$crime_date_time,
            "<br />"
          ),
          htmltools::HTML
        )
        
        m <- leaflet::addCircleMarkers(
          m,
          data = pnt_dt,
          label = labels,
          radius = 3,
          color = "black",
          group = paste(c, y),
          clusterOptions = leaflet::markerClusterOptions(),
          options = leaflet::pathOptions(pane = "places")
        )
      }
    }
    
    # add study circle
    m <- leaflet::addCircles(
      m,
      lng = -77.09523,
      lat = 38.8871,
      weight = 5,
      stroke = TRUE,
      color = "Black",
      fillColor = "Black",
      fillOpacity = .1,
      radius = 402.336,
      group = "study circle",
      options = leaflet::pathOptions(pane = "under_places")
    )
    
    # add second points data layer
    # m <- leaflet::addCircleMarkers(
    #   m,
    #   #data = map_pnts_2_sf[map_pnts_2_sf$in_circle == T,],
    #   data = map_pnts_2_sf,
    #   color = ~ pal2(ARI),
    #   radius = 8,
    #   fillOpacity = .7,
    #   label = ~ as.character(trade_name),
    #   group = "restaurants",
    #   options = leaflet::pathOptions(pane = "places")
    # )
    
    print("Adding Marker Layers...")
    ari_tf <- map_pnts_2_sf$ARI
    getColor <- function(aritf) {
      sapply(aritf, function(ARI) {
        if (ARI == TRUE) {
          "darkblue"
        } else {
          "lightblue"
        }
      })
    }
    
    icons <- leaflet::awesomeIcons(icon = 'fa-cutlery',
                                   library = 'fa',
                                   markerColor = getColor(ari_tf))
    
    m <- leaflet::addAwesomeMarkers(
      m,
      data = map_pnts_2_sf,
      group = "restaurants",
      icon = icons,
      label = ~ as.character(Restaurant),
      options = leaflet::pathOptions(pane = "places")
    )
    
    # make group names
    if (exists("cys"))
      rm(cys)
    for (c in crime_types) {
      for (y in crime_years) {
        cy <- paste(c, y)
        if (exists("cys"))
          cys <- c(cys, cy)
        else
          cys <- cy
      }
    }
    
    # add Layer Control
    print("Building Controls...")
    m <- leaflet::addLayersControl(
      m,
      baseGroups = cys,
      overlayGroups = c("restaurants", "study circle"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
    
    m <- leaflet::showGroup(m, cys[1])
    
    # add Legend
    m <- leaflet::addLegend(
      m,
      position = "topleft",
      pal = pal,
      values = c(0, 3, 6, 12, 24, 48),
      title = "Crime Count",
      opacity = 1
    )
    saveRDS(m, "m.RDS")
  }
  
  m <- readRDS("m.RDS")
  print("Launching Map...")
  m
}

