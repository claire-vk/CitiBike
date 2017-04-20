library(shinydashboard)

Agegroup = sort(unique(df$agegroup))
Gender = unique(df$gender)
Dayofweek = unique(df$dayofweek)
Stationname = sort(unique(df$start.station.name))
Hour = unique(df$starthour)

shinyUI(dashboardPage(
  dashboardHeader(title = 'Citi Tinder'),
  
  #sidebar content
  dashboardSidebar(
    sidebarUserPanel(
      "Claire Vignon Keser",
      image = 'picture.png'),
    
    sidebarMenu(
      menuItem("When", tabName = "when", icon = icon("calendar-times-o")),
      menuItem("Where", tabName = "where", icon = icon("map-marker")),
      menuItem("How", tabName = "how", icon = icon("map-signs"))
    )
  ),
  
  # body content
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      # first tab content
      tabItem(tabName = 'when',
              fluidRow(
                box(title = 'A title',
                    plotlyOutput('heatmap', height = 250)),
                box(
                  title = 'Filter your search',
                  selectInput(inputId = 'gender_heatmap', 
                              label = 'Gender:', 
                              choices = Gender, 
                              selected = Gender[1]),
                  selectInput(inputId = 'agegroup_heatmap', 
                              label = 'Age:', 
                              choices = Agegroup, 
                              selected = Agegroup[1])
                )
              )
      ),
      
      # second tab content
      tabItem(tabName = "where",
              fluidRow(
                box(title = 'A title',
                    leafletOutput('tile', height = 250)),
                box(
                  title = 'Filter your search',
                  selectInput(inputId = 'gender_tile', 
                              label = 'Gender:', 
                              choices = Gender, 
                              selected = Gender[1]),
                  sliderInput(inputId = "age_tile", 
                              label = "Age range:", 
                              17, 98, 25),
                  selectInput(inputId = 'dayofweek_tile', 
                              label = 'Day of week:', 
                              choices = Dayofweek, 
                              selected = Dayofweek[1]),
                  sliderInput(inputId = "time_tile", 
                              label = "Time of day:", 
                              0, 24, 8),
                  radioButtons(inputId = 'startstop_tile',
                               label = '',
                               choices = c('departing', 'arriving'))
                )
              )
      ),
      
      # third tab content
      tabItem(tabName = "how",
              fluidRow(infoBoxOutput("durationCitibike"),
                       infoBoxOutput("durationGoogle")),
              fluidRow(box(title = 'A title',
                           leafletOutput('map', height = 250)),
                       
                       box(title = 'Plan your trip',
                           selectInput(inputId = 'start_map', 
                                       label = 'Departing from:', 
                                       choices = Stationname, 
                                       selected = Stationname[1]),
                           selectInput(inputId = 'stop_map', 
                                       label = 'Going to:', 
                                       choices = Stationname, 
                                       selected = Stationname[1]),
                           selectInput(inputId = 'dayofweek_map', 
                                       label = 'Day of week:', 
                                       choices = Dayofweek, 
                                       selected = Dayofweek[1]),
                           selectInput(inputId = 'hour_map', 
                                       label = 'Leave at:', 
                                       choices = Hour, 
                                       selected = Hour[8])
                       )
              )
      )
    )
  )
)
)
