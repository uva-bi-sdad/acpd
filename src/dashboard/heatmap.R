
# Get Crime Hours

if (file.exists("crime_hours.RDS")) {
  get_crime = function() {
    conn <- dbConnect(drv = PostgreSQL(),
                      dbname = "acpd",
                      host = "postgis_1",
                      port = 5432L,
                      user = Sys.getenv("db_userid"),
                      password = Sys.getenv("db_pwd"))
    output = dbReadTable(conn = conn,
                         name = c('incidents_filtered')) %>%
      data.table::data.table()
    on.exit(dbDisconnect(conn = conn))
    return(value = output)
  }
  
  acpd_data <- get_crime()
  data.table::setDT(acpd_data)
  acpd_data <- acpd_data[crime_cat != "Traffic/Parking Violations",]
  acpd_data %>% dt_mutate(year = year(acpd_data$start))
  crime_hours <- acpd_data[, .N, list(hour, type = crime_cat, year)]
  
  #saveRDS(crime_hours, "./src/dashboard/crime_hours.RDS")
  saveRDS(crime_hours, "crime_hours.RDS")
}

print("Loading Data Files...")
crime_hours <- readRDS("crime_hours.RDS")

make_heatmap <- function(crime_cat, crime_hours) {
  data.table::setDT(crime_hours)
  ch <- crime_hours[type == crime_cat,]
  heatmap <- plotly::plot_ly(
    y = ch$hour, 
    x = ch$year,
    z = ch$N,
    type = "heatmap"
  ) %>%
    plotly::layout(
      title = paste(crime_cat),
      xaxis = list(type = "category"),
      yaxis = list(type = "numeric", dtick = 1))
  
  heatmap
}

make_heatmap("DUI", crime_hours)
