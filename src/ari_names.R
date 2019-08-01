library(rio)
library(here)

d <- here('data', 'acpd','original', 'ARI Nightlife Detail Logs',  'Clarendon Detail Calls for Service 2016 - YTD.xlsx')
details <- import_list(d, setclass = "tbl", rbind = TRUE)
est <- unique(details$Establishment)

ari_names <- function(rest_name) {

  if (str_detect(string = rest_name,
                 pattern = "(?i)(Los Tios|Los Tios Grill)")) {
    "Los Tios"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(ba ba|baba|baba bar|ba ba bar)")) {
    "BA BA"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(buena|vida)")) {
    "Buena Vida"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(bracket|brakett|BR)")) {
    "BR"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(don tito|DT)")) {
    "DT"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(GOAT|G.O.A.T.|GT)")) {
    "GT"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(freddie)")) {
    "Freddie’s Beach Bar & Restaurant"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(freddie)")) {
    "Freddie’s Beach Bar & Restaurant"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(fredrico|federico)")) {
    "Federico Ristorante Italiano"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(four courts)")) {
    "Ireland’s Four Courts"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(mexicali|MB)")) {
    "MB"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(sullivan)")) {
    "O’Sullivan’s Irish Pub"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(punch bowl|PBS)")) {
    "PBS"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(rhodeside)")) {
    "Rhodeside Grill"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(samuel becket)")) {
    "Samuel Beckett’s Irish Gastro Pub"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(celtic)")) {
    "The Celtic House Irish Pub & Restaurant"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(spirits of|spirit of|76)")) {
    "Spirits of ‘76"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(ttt|mexican diner)")) {
    "TTT"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(whitlow|WT)")) {
    "WT"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(william jeffrey)")) {
    "William Jeffrey’s Tavern"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(wilson hardware|wh)")) {
    "WH"
  } else if (str_detect(string = rest_name,
                        pattern = "(?i)(wilson hardware|wh)")) {
    "WH"
  } else {
    rest_name
  }
}


d <- here('data', 'acpd','original', 'ARI Nightlife Detail Logs',  'Clarendon Detail Calls for Service 2016 - YTD.xlsx')
details <- import_list(d, setclass = "tbl", rbind = TRUE)

