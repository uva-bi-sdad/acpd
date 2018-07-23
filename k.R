chk = abc %>%
  filter(city %in% 'ARLINGTON') %>%
  select(name, address) %>%
  unique()
