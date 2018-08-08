library(readr)
library(readxl)
#crime_by_category dataset created in crime_bar_graph.R script
crime_data <- read_csv('crime data.csv')
crime_categories <- read_excel('crime_categories.xlsx')
crime_by_category <- crime_data %>%
  dplyr::inner_join(crime_categories, by = c('description' = 'Description'))
category_description <- crime_data %>%
  select(-location) %>%
  left_join(crime_categories, by = c('description' = 'Description')) %>%
  select(id, description, Category, everything()) %>%
  arrange(Category)

crime_datasf <- sf::st_as_sf(crime_by_category, coords = c("longitude", "latitude"))

plot(crime_datasf[,c("Category")])


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
      menuItem(tabName = "map", "Map", icon = icon("globe"))
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
  
  crime_type <- crime_report[crime_report$Crime_data==TRUE,]
  
  
  
  
  
  # DT
  output$crime_type <- renderDataTable(crime_type)
  
  # leaflet
  crime_data <- fread("data/crime data.csv")
  
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












