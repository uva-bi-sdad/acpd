make_crime_map <- function() {
  library(sf)
  county_fips <- "013"
  crime_types <-
    c(
      "Aggravated Assault",
      "Disorderly Conduct",
      "Drunkenness",
      "DUI",
      "Sexual Assault/Rape"
    )
  crime_years <- c(2015, 2016, 2017, 2018)
  
  
  if (!file.exists("map_polys_sf.RDS")) {
    # Get Polygons (CENSUS blocks)
    . <- sf::st_read(
      sdalr::con_db("sdal"),
      query = paste0(
        "select * from geospatial$census_tl.tl_2017_51_tabblock10 where \"COUNTYFP10\" = '",
        county_fips,
        "'"
      )
    )
    . <- sf::st_transform(., 4326)
    . <- rmapshaper::ms_simplify(., keep = 0.005)
    polys_sf <- .
    
    # Get Points (crime locations)
    . <- sf::st_read(
      sdalr::con_db("acpd"),
      query = paste0(
        "select * from clean_acpd_cat_data where cast(year as int) in (",
        paste(crime_years, collapse = ","),
        ")"
      )
    )
    names(.) <-
      c(
        "id",
        "crime_description",
        "crime_address",
        "crime_latitude",
        "crime_longitude",
        "start",
        "end",
        "crime_year",
        "month",
        "day_of_week",
        "crime_hour",
        "Description",
        "crime_type"
      )
    .$crime_date <- .$end
    .$crime_date_time <- as.character(.$end)
    . <-
      sf::st_as_sf(., coords = c("crime_longitude", "crime_latitude"))
    sf::st_crs(.) <- 4326
    pnts_sf <- .
    
    
    
    # Get Points (restaurant locations)
    . <- sf::st_read(
      sdalr::con_db("acpd"),
      query = "select INITCAP(trade_name) trade_name, longitude, latitude
      from vabc_arlington_restaurants where privilege_status = 'Active'
      and (privilege_description like '%Wine%' or privilege_description like '%Beer%')"
    )
    . <- sf::st_as_sf(., coords = c("longitude", "latitude"))
    pnts_2_sf <- .
    
    
    # Join Polygons to Points (retains polygon geometry)
    . <- sf::st_join(polys_sf, pnts_sf, join = sf::st_intersects)
    cj <-
      data.table::CJ(polys_sf$"GEOID10", crime_years)[, .(GEOID10 = V1, crime_year =
                                                            V2)]
    cj_sf <- merge(polys_sf, cj, by = "GEOID10", all.y = TRUE)
    . <- sf::st_join(cj_sf, ., join = sf::st_equals)
    . <-
      .[, c(
        "GEOID10.x",
        "crime_year.x",
        "crime_description",
        "crime_date_time",
        "crime_address",
        "crime_date",
        "crime_hour",
        "crime_type"
      )]
    names(.)[names(.) == "GEOID10.x"] = "GEOID10"
    names(.)[names(.) == "crime_year.x"] = "crime_year"
    polys_pnts_sf <- .
    
    
    # polys_pnts_sf <- merge(polys_pnts_sf, cj, by = c("GEOID10", "crime_year"), all.y = TRUE)
    # polys_pnts_sf <- polys_pnts_sf[!is.na(.$crime_latitude),]
    
    # Join Points to Polygons (retains point geometry)
    pnts_polys_sf <-
      sf::st_join(pnts_sf, polys_sf, join = sf::st_intersects)
    
    
    # Prepare Polygon Dataset for Mapping
    . <-
      dplyr::group_by(polys_pnts_sf, GEOID10, crime_type, crime_year)
    . <- dplyr::summarise(., N = length(GEOID10))
    . <- tidyr::spread(data = .,
                       key = c("crime_type"),
                       value = N)
    map_polys_sf <- .
    
    saveRDS(map_polys_sf, "map_polys_sf.RDS")
    
    
    # Prepare Point Dataset for Mapping
    . <- pnts_polys_sf[!is.na(pnts_polys_sf$GEOID10), ]
    map_pnts_sf <- .
    
    saveRDS(map_pnts_sf, "map_pnts_sf.RDS")
    
    
    # Prepare Second Point Dataset for Mapping
    within_circle <- function(lon, lat, ctr_pnt = 402.336) {
      geosphere::distm(x = c(-77.09523, 38.8871),
                       y = c(lon, lat)) < ctr_pnt
    }
    . <- cbind(pnts_2_sf, sf::st_coordinates(pnts_2_sf))
    .$in_circle <- mapply(within_circle, .$X, .$Y)
    .$ARI <- FALSE
    .[.$trade_name %in% c('Whitlows On Wilson',
                          'Wilson Hardware',
                          'Bar Bao',
                          'Pamplona'), "ARI"] <- TRUE
    map_pnts_2_sf <- .
    
    saveRDS(map_pnts_2_sf, "map_pnts_2_sf.RDS")
  }
  
  
  
  # Load Data Files
  print("Loading Data Files...")
  map_polys_sf <- readRDS("map_polys_sf.RDS")
  map_pnts_sf <- readRDS("map_pnts_sf.RDS")
  map_pnts_2_sf <- readRDS("map_pnts_2_sf.RDS")
  
  
  
  if (!file.exists("m.RDS")) {
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
        paste(
          "<strong>county:",
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
    label = ~ as.character(trade_name),
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
