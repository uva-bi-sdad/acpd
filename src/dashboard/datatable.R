# Get Crime Data

if (!file.exists("crimes.RDS")) {
  get_crime = function() {
    conn = con_db(dbname = 'acpd',
                  pass = get_my_password())
    output = dbReadTable(conn = conn,
                         name = c('clean_acpd_cat_data')) %>%
      data.table::data.table()
    on.exit(dbDisconnect(conn = conn))
    return(value = output)
  }
  acpd_data <- get_crime()
  
  #saveRDS(crime_hours, "./src/dashboard/crime_hours.RDS")
  saveRDS(acpd_data, "crimes.RDS")
}

print("Loading Crimes Data File...")
crimes_data <- readRDS("crimes.RDS")
#crimes_data <- readRDS("src/dashboard/crimes.RDS")

make_datatable <- function(crime_type, crimes_data) {
  data.table::setDT(crimes_data)
  c <- crimes_data[Category == crime_type, ]
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
#make_datatable("DUI", crimes_data)  

