

#Load data set
library(readr)
dataset <- read_csv('./data/acpd/working/crime data.csv')
View(dataset)

#load excel sheet; excel sheet was done manually, it is now in 'acpd' on the database as crime_categories
library(readxl)
crime_categories <- read_excel('./data/acpd/working/crime_categories.xlsx')
View(crime_categories)


#upload excel spread sheet to the database
pacman::p_load(sdalr, DBI, dplyr, data.table, dtplyr)
upload_to_db = function(data, name) {
  conn = con_db(dbname = 'acpd',
                pass = get_my_password())
  dbWriteTable(conn = conn,
               name = name,
               value = data,
               row.names = FALSE,
               overwrite = TRUE)
  on.exit(expr = dbDisconnect(conn = conn))
  }
upload_to_db(data = Crime_categories,
             name = 'crime_categories')

#join excel sheet and dataset by description 
category_description <- dataset %>%
  select(-location) %>%
  left_join(Crime_categories, by = c('description' = 'Description')) %>%
  select(id, description, Category, everything()) %>%
  arrange(Category)

View(category_description)
            
            