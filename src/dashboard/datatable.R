
# Get Crime Data

if (!file.exists("crimes.RDS")) {
  get_crime = function() {
    conn <- dbConnect(drv = PostgreSQL(),
                      dbname = "acpd",
                      host = Sys.getenv("host"),
                      port = Sys.getenv("port"),
                      user = Sys.getenv("db_userid"),
                      password = Sys.getenv("db_pwd"))
    output <- dbReadTable(conn = conn,
                         name = 'incidents_filtered') %>%
      data.table()
    on.exit(dbDisconnect(conn = conn))
    return(value = output)
  }
  crimes_data <- get_crime()
  
  #saveRDS(crime_hours, "./src/dashboard/crime_hours.RDS")
  saveRDS(acpd_data, "crimes.RDS")
}

print("Loading Crimes Data File...")
crimes_data <- readRDS("crimes.RDS")
#crimes_data <- readRDS("src/dashboard/crimes.RDS")

make_datatable <- function(crime_type, crimes_data) {
  data.table::setDT(crimes_data)
  c <- crimes_data[crime_cat == crime_type, ]
  dt <-
    DT::datatable(
      c,
      extensions = c('Buttons', 'FixedColumns'),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'print'),
        scrollX = TRUE,
        fixedColumns = TRUE
      )
    )
  dt
}
make_datatable("DUI", crimes_data)  

