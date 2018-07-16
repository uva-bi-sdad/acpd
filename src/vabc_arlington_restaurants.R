# Loading the ABC data

# Housekeeping
setwd(dir = rprojroot::find_rstudio_root_file())
pacman::p_load(sdalr, DBI, stringr, stringdist, dplyr, data.table, dtplyr, purrr)
google_api_key = 'AIzaSyAJwMuwL73uIxLhhhYoPkDymDFcQJVCtPA'

geocode = function(address) {
  response = str_c('https://maps.googleapis.com/maps/api/geocode/json',
                   '?address=',
                   address,
                   '&key=',
                   google_api_key) %>%
    str_replace_all(pattern = '\\s',
                    replacement = '+') %>%
    GET()
  if (status_code(x = response) == 200L) {
    lon_lat = response %>%
      content(as = 'text',
              encoding = 'UTF-8') %>%
      fromJSON() %>%
      getElement(name = 'results')
    best_match = with(lon_lat,
                      which.min(x = stringdist(a = address,
                                               b = formatted_address)))
    lon_lat = lon_lat %>%
      getElement(name = 'geometry') %>%
      getElement(name = 'location') %>%
      filter(1:nrow(x = .) %in% best_match)
    output = data.table(address = address,
                        longitude = lon_lat$lng,
                        latitude = lon_lat$lat)
  } else {
    output = data.table(address = address,
                        longitude = NA,
                        latitude = NA)
  }
  return(value = output)
}

# Virginia Alcoholic Beverage Control Authority (VABC)
# https://www.abc.virginia.gov/licenseesearch/retail_search_view.do
# Select the data for all
# - Establishment Type: Restaurant
# - City/County: Arlington County
# Save it to the server and put the filename to the path

filename = ''
abc_arlington_restaurants = fread(input = filename)
names(x = abc_arlington_restaurants) = c('license',
                                         'trade_name',
                                         'company_name',
                                         'establishment_type',
                                         'establishment_subtype',
                                         'address',
                                         'trade_phone',
                                         'company_phone',
                                         'origination_date',
                                         'privilege_description',
                                         'privilege_status',
                                         'effective_date',
                                         'expiration_date',
                                         'renewal_date',
                                         'privilege_description_mixed_beverage',
                                         'privilege_status_mixed_beverage_restaurant',
                                         'effective_date_mixed_beverage_restaurant',
                                         'expiration_date_mixed_beverage_restaurant',
                                         'renewal_date_mixed_beverage_restaurant')
restaurants_address = abc_arlington_restaurants %>%
  pull(address) %>%
  unique()
geocode_restaurants = map_df(.x = restaurants_address,
                             .f = geocode)
output = merge(x = abc_arlington_restaurants,
               y = geocode_restaurants)
conn = con_db(dbname = 'acpd',
              pass = get_my_password())
# dbWriteTable(con = conn,
#              name = 'vabc_arlington_restaurants',
#              value = output,
#              row.names = FALSE,
#              overwrite = FALSE)
dbDisconnect(conn = conn)
