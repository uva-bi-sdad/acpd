#Input data frame from database

library(sdalr)
library(DBI)

con <- con_db('jbsc', pass = get_my_password())

df <- dbGetQuery(con,
                 'SELECT * FROM "abc_violations_2014_07_2017_06"')
df

library(ggmap
        )

location = str_c(df$address, df$city, df$state)
a <- geocode(location = with(df,str_c(address[1], city[1], state[1], sep = ", ")), source = 'dsk')

sub_df <- df %>%
  filter(city == 'ARLINGTON') %>% 
  mutate(address = str_c(address, city, state, sep = ", ")) %>%
  select(id, address) %>% 
  unique()

View(sub_df)
