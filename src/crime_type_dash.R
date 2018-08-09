library(readr)
library(readxl)
library(magrittr)
library(dplyr)
library(sdalr)
library(DBI)
#crime_by_category dataset created in crime_bar_graph.R script
crime_data <- read_csv('data/acpd/working/crime data.csv')
crime_categories <- read_excel('data/acpd/working/crime_categories.xlsx')
crime_by_category <- crime_data %>%
  dplyr::inner_join(crime_categories, by = c('description' = 'Description'))
category_description <- crime_data %>%
  select(-location) %>%
  left_join(crime_categories, by = c('description' = 'Description')) %>%
  select(id, description, Category, everything()) %>%
  arrange(Category)

crime_datasf <- sf::st_as_sf(crime_by_category, coords = c("longitude", "latitude"))

plot(crime_datasf[,c("Category")])




# Get Crime Hours

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

acpd_data <- acpd_data[, c("hour", "Category", "year")]
data.table::setDT(acpd_data)
acpd_data <- acpd_data[Category != "Traffic/Parking Violations", ]
crime_hours <- acpd_data[, .N, list(hour, type = Category, year)]

# 
# categories = c("DUI", "Aggravated Assault")
# 
# filter_dta <- function(categories_to_keep) {
#   crime_hours <- crime_hours[Category %in% categories_to_keep]
# 
# }
# 
# crime_hours <- filter_dta(categories)

make_heatmap <- function(crime_type, crime_hours) {
  data.table::setDT(crime_hours)
  ch <- crime_hours[type == crime_type,]
  heatmap <- plotly::plot_ly(
    y = ch$hour, 
    x = ch$year,
    z = ch$N,
    type = "heatmap"
  ) %>%
    plotly::layout(
      title = "Title",
      xaxis = list(type = "category"),
      yaxis = list(type = "numeric", dtick = 1))
  
  heatmap
}

make_heatmap("DUI", crime_hours)

library(plotly)




# Make Heatmap


# 
# get_crime = function() {
#   conn = con_db(dbname = 'sdal',
#                 pass = get_my_password())
#   output = dbReadTable(conn = conn,
#                        name = c('behavior','va_pl_spotcrime_cat_08_18_dups')) %>%
#     data.table()
#   on.exit(dbDisconnect(conn = conn))
#   return(value = output)
# }
# acpd_data <- get_crime()
# 
# acpd_data$crime_time <- NA
# for (i in 1:nrow(acpd_data))
# {
#   hr <- acpd_data$crime_hour[i]
#   tm <- strsplit(acpd_data$crime_date_time[i], " ")[[1]][2]
#   min <- strsplit(tm, ":")[[1]][2]
#   acpd_data$crime_time[i] <- paste(hr, min, sep = ":")
# 
#   hr <- NULL
#   tm <- NULL
#   min <- NULL
#   print(i)
# }
# 
# for (k in 1:nrow(acpd_data))
# {
#   if (nchar(acpd_data$crime_time[k]) == 4)
#   {
#     acpd_data$crime_time[k] <- paste("0", acpd_data$crime_time[k], sep = "")
#     
#   }
#   
# }
# 
# acpd_data$crime_time2<-ifelse(nchar(acpd_data$crime_time) == 4, 
#                               paste("0", acpd_data$crime_time, sep = ""),
#                               acpd_data$crime_time)
# 
# acpd_data$crime_time2 <- paste0(acpd_data$crime_time2, ":00")
# library(chron)
# acpd_data$crime_time3 <- chron(times = acpd_data$crime_time2)
# 
# 
# 
# acpd_data$interval2<-ifelse(acpd_data$crime_time3 >= as.Date(03:00:00) & acpd_data$crime_time3 <= as.Date(08:59:00),
#                            "3_to_9",
#                            NA)
# acpd_data$interval <- NA
# for (j in 1:5)
# {
#   if ((acpd_data$crime_time[j] >= "3:00") && (acpd_data$crime_time[j] <= "8:59"))
#   {
#     acpd_data$interval[j] <- "3_to_9"
#   } else if ((acpd_data$crime_time[j] >= "9:00") && (acpd_data$crime_time[j] <= "14:59")) {
#     acpd_data$interval[j] <- "9_to_15"
#   } else if ((acpd_data$crime_time[j] >= "15:00") && (acpd_data$crime_time[j] <= "20:59")) {
#     acpd_data$interval[j] <- "15_to_21"
#   } else {
#     acpd_data$interval[j] <- "21_to_3"
#   }
# }


library(leaflet)

pal <- colorFactor("viridis", crime_datasf$description)


dash_map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(data = crime_datasf, radius = 1, color = ~pal(Category)) %>%
  addLegend(position = "topleft", pal = pal, values = crime_datasf$Category, title = "Arlington Crime")
dash_map

#========Interactive Dashboard======================

library(shiny)
library(semantic.dashboard)
library(shiny.semantic)
library(ggplot2)
library(plotly)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(color = "blue", title = "Alcohol-Related Crime", inverted = TRUE),
  dashboardSidebar(
    color = "teal",
    sidebarMenu(
      menuItem(tabName = "crime type", "Crime Type", icon = icon("ban")),
      menuItem(tabName = "data", "Data", icon = icon("info-circle")),
      menuItem(tabName = "map", "Map", icon = icon("globe")),
      menuItem(tabName = "heatmap", "Heatmap", icon = icon("th-large"))
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = "crime type",
        fluidRow(
          box(width = 8,
              title = "Graph 1",
              color = "green", ribbon = TRUE, title_side = "top right",
              column(8,
                     plotOutput("boxplot1")
              )
          ),
          box(width = 8,
              title = "Graph 2",
              color = "red", ribbon = TRUE, title_side = "top right",
              column(width = 8,
                     plotlyOutput("dotplot1")
              )
          )
        )
      ),

      tabItem(
        tabName = "data",
        fluidRow(
          dataTableOutput("crime_type")
        )
      ),
      tabItem(
        tabName = "map",
        
        fluidRow(
          box(width = 4, title = "Map Control",
              dropdown("dd1",
                       choices = c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape", "Traffic/Parking Violations", "Underage Drinking/Fake ID", "NA"),  #DRUNK|DUI|ASSAULT|POSSESSION|SEXUAL|DISORDERLY
                       choices_value = c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape", "Traffic/Parking Violations", "Underage Drinking/Fake ID", "NA"),
                       default_text = "Select",
                       value = "ALL")
          ),
          box(width = 12,
              title = "Arlington Crime",
              color = "red", ribbon = TRUE,
              leafletOutput("map")
          )
        )
      ),
      tabItem(
        tabName = "heatmap",
        
        fluidRow(
          box(width = 4, title = "Heatmap Control",
              dropdown("dd2",
                       choices = c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape", "Underage Drinking/Fake ID"),
                       choices_value = c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape", "Underage Drinking/Fake ID"),
                       default_text = "Select",
                       value = "ALL")
          ),
          box(width = 12,
              title = "Arlington Crime",
              color = "red", ribbon = TRUE,
              leafletOutput("map"))
        )
      )
    )
  ), theme = "cerulean"
)
#=================Crime Type Drop Down==========================
server <- shinyServer(function(input, output, session) {
  library(data.table)
  library(sf)
  library(magrittr)
  library(dplyr)
  library(stringr)
  
  #report <- read.csv("data/, stringsAsFactors = FALSE)
  #crime_datasf <- sf::st_as_sf(Crime_data, coords = c("longitude", "latitude"))
  
  crime_type <-  c("Aggravated Assault", "Disorderly Conduct", "Drunkenness", "DUI", "Sexual Assault/Rape", "Traffic/Parking Violations", "Underage Drinking/Fake ID", "NA" )
library(stringr)
  
  #need to figure out why pattern command won't fun
  
  crime_report <- category_description %>%
    mutate(category_description = str_detect(string = Category,
                                   pattern = str_c('(Aggravated Assault|',
                                                   'Disorderly Conduct|',
                                                   'Drunkenness|',
                                                   'DUI|',
                                                   'Sexual Assault/Rape|',
                                                   'Traffic/Parking Violations|',
                                                   'Underage Drinking/Fake ID|)')))
  
  crime_type <- crime_report[crime_report$crime_data==TRUE,]
  
  
  
  
  
  # DT
  output$crime_type <- renderDataTable(crime_type)
  
  # leaflet
  crime_data <- fread("~/acpd/data/acpd/working/crime data.csv")
  
  observeEvent(input$dd1, {
    data <- crime_data
    if (input$dd1 != "ALL") data <- data[description %like% input$dd1,]
    
    sf <- st_as_sf(data, coords = c("longitude", "latitude"))
    
    m <- leaflet(data = sf) %>% addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        popup = ~ as.character(location),
        label = ~ as.character(description),
        radius = 4,
        clusterOptions = markerClusterOptions()
      )
    
    output$map <- renderLeaflet(m)
  })
})

shinyApp(ui, server)












