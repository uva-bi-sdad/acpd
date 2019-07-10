
# Get Crime Data

if (file.exists("crimes.RDS")) {
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
  crimes_data$Category <- crimes_data$crime_category

  #saveRDS(crime_hours, "./src/dashboard/crime_hours.RDS")
  saveRDS(crimes_data, "crimes.RDS")
}

print("Loading Crimes Data File...")
crimes_data <- readRDS("crimes.RDS")
#crimes_data <- readRDS("src/dashboard/crimes.RDS")

make_datatable <- function(crime_category, crimes_data) {
  #browser()
  setDT(crimes_data)
  c <- crimes_data[Category == crime_category, ]
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
make_datatable("Aggrevated Assault", crimes_data)

