#### Housekeeping ####
library(dplyr)
library(data.table)
library(dtplyr)
library(sdalr)
library(DBI)
library(lubridate)

acpd = fread("~/git/acpd/data/initial_filtering.csv")
acpd <- read.csv("~/git/acpd.git/crime data.csv")

get_crime = function() {
  conn = con_db(dbname = 'acpd',
                pass = get_my_password())
  output = dbReadTable(conn = conn,
                       name = 'crime') %>%
    data.table()
  on.exit(dbDisconnect(conn = conn))
  return(value = output)
  }
acpd_data <- get_crime()



# getting the year the crime took place 

crime_type %>%
  filter(description %in% relevant_crime_types) %>%
  mutate(year = year(end)) %>%
  group_by(description, year) %>%
  do(function(df) {
    output = data.table(df$description[1],
                        df$year[1],
                        n = nrow(df))
    })

table(crime_type %>%
        filter(description %in% relevant_crime_types) %>%
        pull(end) %>%
        year())


# filtering the nearby incidents by crimes we thought were alcohol related
nearby_incidents = nearby_incidents %>%
    filter(nearby %in% TRUE) %>%
    mutate(yearOfCrime = year(x = start))

relevant_crime_types <- c("PUBLIC DRUNKENNESS (DRUNK IN PUBLIC)",
                          "DUI",
                          "ASSAULT & BATTERY",
                          "PUBLIC SWEARING OR INTOXICATION (DRUNK IN PUBLIC)",
                          "DRINKING WHILE DRIVING / OPEN CONTAINER",
                          "ASSAULT & BATTERY: POLICE/FIRE/EMT/JUDGE/ETC OR BIAS MOTIVATED W/ BODILY INJURY",
                          "POSSESSION OF SCHEDULE I/II CONTROLLED SUBSTANCE",
                          "POSSESSION OF MARIJUANA",
                          "MANUFACTURE/SELL/POSSESS FAKE IDENTIFICATION",
                          "PARKING VIOLATION",
                          "RAPE",
                          "DRINKING ALCOHOL IN PUBLIC",
                          "SEXUAL BATTERY",
                          "DRUNKENNESS",
                          "DUI 3+ OFFENSE OR 2+ FELONY OFFENSE",
                          "REFUSAL OF BREATH/BLOOD TEST 2+ DUI OFFENSE W/IN 10Y",
                          "URINATING AND DEFACATING IN PUBLIC",
                          "RECKLESS DRIVING: GENERAL",
                          "ASSAULT OR BATTERY BY MOB",
                          "SELL/GIVE/DISTRIBUTE/PWID MARIJUANA: MISDEMEANOR",
                          "FORCIBLE SODOMY",
                          "REFUSAL OF BREATH/BLOOD TEST",
                          "POSSESSION OR DISTRIBUTION OF CONTROLLED (DRUG) PARAPHERNALIA",
                          "POSSESS/CONSUME ALCOHOL: 21YO/INTERDICTED/INTOXICATED",
                          "LICENSE: USE FICTITIOUS LIC TO OBTAIN ALCOHOL ",
                          "DRUNK   DRUNKENNESS",
                          "ASSAULT - OTHER",
                          "DWILIQR DRIVING UNDER INFLUENCE-LIQUOR",
                          "SMPLASLTSIMPLE ASSAULT POLICE OFFICER",
                          "NARCOTICMARIJUANA-POSSESSING",
                          "SIMPLE ASSAULT NON FAMILY",
                          "PUBPEACEPUBLIC PEACE - OTHER",
                          "DISORDERDISORDERLY CONDUCT",
                          "DRIVING UNDER INFLUENCE-LIQUOR",
                          "PUBLIC PEACE - OTHER",
                          "SIMPLE ASSAULT POLICE OFFICER",
                          "SIMPLE ASSAULT FAMILY",
                          "MARIJUANA-POSSESSING",
                          "MARIJUANA-SELLING",
                          "NARCOTICS COCAINE-SELLING",
                          "SYNTHETIC NARCOTICS-POSSESSING",
                          "LIQUOR POSSESSING",
                          "NARCOTICS COCAINE-POSSESSING",
                          "INDECENT EXPOSURE",
                          "RAPE-STRONG ARM",
                          "DRIVING UNDER INFLUENCE - OTHER",
                          "SEX OFFENSE - OTHER",
                          "DISTURBING PEACE",
                          "OTHER NON-MOVING TRAFFIC VIOLATION",
                          "DRIVING UNDER INFLUENCE-DRUGS",
                          "LIQUOR MISREPRESENTING AGE-MINOR")


nearby_incidents = nearby_incidents %>%
  filter(yearOfCrime %in% 2015:2017) %>%
  filter(description %in% relevant_crime_types)



filtered = acpd %>% filter(description %in% relevant_crime_types)


saveRDS(object = filtered, file = './data/acpd/working/filtered_by_location_type_time.RDS')
fwrite(filtered, "./data/working/filtered_by_location_type_time.csv")
