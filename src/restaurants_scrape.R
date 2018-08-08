pacman::p_load(sdalr, DBI, httr, readr, stringr, dplyr, data.table, dtplyr)
arlington_api_key = '140c712ce0aaf752160366975a7fa1cca8c81445'
hannah_api_key = 'AIzaSyC9FKW-kjQlEXjfM3OgMZBJ7xE6zCN1JQI'

abc = function() {
  conn = con_db(dbname = 'jbsc',
                pass = get_my_password())
  output = dbReadTable(con = conn,
                       name = 'abc_violations_2014_07_2017_06') %>%
    data.table()
  on.exit(dbDisconnect(conn = conn))
  return(value = output)
}
abc = abc()
names(x = abc)

arlington <- subset(abc, city == "ARLINGTON")



#"https://maps.googleapis.com/maps/api/geocode/json?address=Arlington,+VA&key=AIzaSyAJwMuwL73uIxLhhhYoPkDymDFcQJVCtPA"
