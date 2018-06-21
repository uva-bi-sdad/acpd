pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)
some_data = function() {
  #' Getting some_data from the database.
  #' 
  #' @example some_data()
  conn = con_db(dbname = 'jbsc',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = '') %>%
    data.table()
  on.exit(dbDisconnect(conn = conn))
  return(value = output)
}

acpd_data <- some_data()
