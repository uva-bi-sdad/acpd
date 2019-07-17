make_crime_map <- function(crime_type) {
  crime_tp <- crime_type
  crime_years <- crime_years <- c(2015, 2016, 2017, 2018)

  get_census_block_year_crime_type_count <- function() {
    if (!file.exists("map_polys_sf.RDS")) {
      # Get Polygons (CENSUS blocks)
      conn <- dbConnect(drv = PostgreSQL(),
                        dbname = "acpd",
                        host = "postgis_1",
                        port = 5432L,
                        user = Sys.getenv("db_userid"),
                        password = Sys.getenv("db_pwd"))
      census_blocks <- st_read(dsn = conn, layer = "arlington_census_blocks")

      police_incidents <- dbReadTable(conn = conn,
                                      name = "incidents_filtered") %>%
        dt_select(id, start, crime_category, nearby, day, hour, nightlife, longitude, latitude) %>%
        dt_mutate(crime_year = year(x = start)) %>%
        dt_filter(crime_category == crime_tp) %>%
        setnames(old = "crime_category", new = "crime_categoryegories") %>%
        st_as_sf(coords = c("longitude", "latitude")) %>%
        st_set_crs(value = st_crs(x = census_blocks))
      dbDisconnect(conn)
      skeleton <- expand.grid(na.omit(object = unique(x = census_blocks$fullblockid)),
                              na.omit(object = unique(x = police_incidents$crime_year)),
                              na.omit(object = unique(x = police_incidents$crime_categoryegories))) %>%
        data.table() %>%
        setNames(nm = c("geoid10", "crime_year", "crime_categoryegories"))
      st_join(x = census_blocks,
              y = police_incidents) %>%
        dt_select(fullblockid, crime_year, crime_categoryegories, id) %>%
        setnames(old = "fullblockid", new = "geoid10") %>%
        merge(y = skeleton, all.y = TRUE) %>%
        dt_mutate(count := sum(x = !is.na(x = id)),
                  by = c("geoid10", "crime_year", "crime_categoryegories")) %>%
        dt_select("geoid10", "crime_year", "crime_categoryegories", "count") %>%
        unique() %>%
       dcast(geoid10 + crime_year ~ crime_categoryegories, value.var = "count") %>%
        merge(y = census_blocks %>%
                dt_select(fullblockid, geometry) %>%
                setnames(old = "fullblockid", new = "geoid10"),
              by = "geoid10") %>%
        dt_arrange(geoid10, crime_year) %>%
        st_as_sf() %>%
        saveRDS("map_polys_sf.RDS")
    }
  }

  get_census_block_year_crime_type_count()

  # Get Points (restaurant locations)
  conn <- dbConnect(drv = PostgreSQL(),
                    dbname = "acpd",
                    host = "postgis_1",
                    port = 5432L,
                    user = Sys.getenv("db_userid"),
                    password = Sys.getenv("db_pwd"))
  restaurants <- dbReadTable(conn = conn,
                             name = 'vabc_arlington_restaurants') %>%
    setDT() %>%
    dt_mutate(priv = str_detect(string = priv_desc,
                                pattern = "(?i)(wine|beer)")) %>%
    dt_filter(priv) %>%
    dt_filter(lic_status_status_desc %in% "Active") %>%
    st_as_sf(coords = c("x", "y"))
  pnts_2_sf <- restaurants %>% dt_select(key, restaurant, address, ari, ask_angela, geometry) %>% distinct()

  # skeleton <- expand.grid(address = na.omit(object = unique(x = pnts_2_sf$address)),
  #                         year = na.omit(object = unique(x = crime_years))) %>% setDT()
  #
  # pnts_2_sf <- merge(pnts_2_sf, skeleton, by = 'address')
  # bring in violations data and filter to violations in past month
  violations <- dbReadTable(conn = conn,
                             name = 'abc_violations') %>% data.table()

  dbDisconnect(conn)
  violations_by_rest <- violations %>%  dt_mutate(year = year(violation_date)) %>%
    group_by(year,licensee_name, physical_address) %>%
    arrange(physical_address,licensee_name, year) %>%
    summarise(all_charges  = paste(unique(charges), collapse ="; "), total_charges = sum(number_of_charges))  %>%
    arrange(physical_address,licensee_name, year, desc(total_charges), all_charges)

  violations_complete <- violations_by_rest %>% data.table() %>% dt_mutate(key = paste(substr(tolower(licensee_name),1,5), substr(tolower(physical_address),1,5)) %>%
              str_remove_all(pattern = "\\s"))

  pnts_2_sf_violations <- merge(pnts_2_sf, violations_complete, by = 'key', all.x = TRUE,fill = TRUE)
  pnts_2_sf_violations$total_charges[is.na(pnts_2_sf_violations$total_charges)] <- 0
  pnts_2_sf_violations$year[is.na(pnts_2_sf_violations$year)] <- crime_yr
  pnts_2_sf_violations <- pnts_2_sf_violations %>% data.table() %>% st_as_sf()

  # Prepare Second Point Dataset for Mapping
  within_circle <- function(lon, lat, ctr_pnt = 402.336) {
    distm(x = c(-77.09523, 38.8871),
          y = c(lon, lat)) < ctr_pnt
  }
  . <- cbind(pnts_2_sf_violations, sf::st_coordinates(pnts_2_sf_violations))
  .$in_circle <- mapply(within_circle, .$X, .$Y)
  map_pnts_2_sf <- .

  saveRDS(map_pnts_2_sf, "map_pnts_2_sf.RDS")

  # Crime Points
  conn <- dbConnect(drv = PostgreSQL(),
                    dbname = "acpd",
                    host = "postgis_1",
                    port = 5432L,
                    user = Sys.getenv("db_userid"),
                    password = Sys.getenv("db_pwd"))
  census_blocks <- st_read(dsn = conn, layer = "arlington_census_blocks")
  police_incidents <- dbReadTable(conn = conn,
                                  name = "incidents_filtered") %>%
    dt_select(id, description, start, crime_category, nearby, day, hour, nightlife, longitude, latitude) %>%
    dt_mutate(crime_year = year(x = start)) %>%
    dt_filter(crime_category == crime_tp) %>%
    st_as_sf(coords = c("longitude", "latitude")) %>%
    st_set_crs(value = st_crs(x = census_blocks)) %>% st_as_sf()

  dbDisconnect(conn)

  saveRDS(police_incidents, "map_pnts_sf.RDS")

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
                                 map_pnts_2_sf$ari)
  }

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
   for (c in crime_years) {
        plydt <-
          dplyr::filter(map_polys_sf, crime_year == c)[, c(crime_tp, "geoid10")]

        labels <- lapply(
          paste("<strong>Year:</strong>",
                y,
                "<br />", "<strong/>County:</strong>",
                substr(plydt$geoid10, 3, 5),
                "<br />",
                "<strong>Tract:</strong>",
                substr(plydt$geoid10, 6, 11),
                "<br />",
                "<strong>Block Group:</strong>",
                substr(plydt$geoid10, 12, 12),
                "<br />",
                "<strong>Crime Type:</strong>",
                c,
                "<br />",
                "<strong>Measure:</strong> count
                <br />",
                "<strong>Value:</strong>",
                plydt[, crime_tp][[1]]
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
          fillColor = ~ pal(get(crime_tp)),
          label = labels,
          group = paste(crime_tp, crime_yr),
          options = leaflet::pathOptions(pane = "base_layers")
        )
   }

    # add point data layers
    print("Adding Point Layers...")
    for (c in crime_years) {
        pnt_dt <- map_pnts_sf[year = c,]

        labels <- lapply(
          paste(
            "<strong>Crime Description:</strong>",
            pnt_dt$description,
            "<br />",
            "<strong>Crime Timestamp:</strong>",
            pnt_dt$start,
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
          group = paste(crime_tp, crime_yr),
          clusterOptions = leaflet::markerClusterOptions(),
          options = leaflet::pathOptions(pane = "places")
        )
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

    print("Adding Marker Layers...")
    ari_tf <- map_pnts_2_sf$ari
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

    # add restaurant tooltips
    for (c in crime_years) {
    data <- map_pnts_2_sf %>% filter(year == c)

    rest_label <- lapply(
      paste(
        "<strong>Restaurant:</strong>",
        data$restaurant,
        "<br />",
        "<strong>Ask Angela:</strong>",
        data$ask_angela,
        "<br />",
        "<strong>Total Charges:</strong>",
        data$total_charges,
        "<br />",
        "<strong>Charges Types:</strong>",
        data$all_charges
      ),
      htmltools::HTML
    )

    m <- leaflet::addAwesomeMarkers(
      m,
      data = map_pnts_2_sf,
      group = "restaurants",
      icon = icons,
      label = rest_label,
      options = leaflet::pathOptions(pane = "places")
    )
    }

  #  make group names
    # if (exists("cys"))
    #   rm(cys)
    # for (c in crime_types) {
    #   for (y in crime_years) {
    #     cy <- paste(c, y)
    #     if (exists("cys"))
    #       cys <- c(cys, cy)
    #     else
    #       cys <- cy
    #   }
    # }


    # add Layer Control
    # print("Building Controls...")
    m <- leaflet::addLayersControl(
      m,
      baseGroups = crime_years,
      overlayGroups = c("restaurants", "study circle"),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )

    m <- leaflet::showGroup(m, crime_years[1])
    # m <- leaflet::showGroup(m, cys[1])

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

  m <- readRDS("m.RDS")
  print("Launching Map...")
  m
}