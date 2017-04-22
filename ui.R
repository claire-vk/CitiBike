library(shinydashboard)

Agegroup = sort(unique(df$agegroup))
Gender = unique(df$gender)
Dayofweek = unique(df$dayofweek)
Startstationname = sort(unique(df$start.station.name))
Stopstationname = sort(unique(df$end.station.name))
Startrange = unique(df$startrange)

shinyUI(dashboardPage(
  dashboardHeader(title = 'CitiTinder'),
  
  #sidebar content
  dashboardSidebar(
    sidebarUserPanel(
      "Claire Vignon Keser",
      image = 'picture.png'),
    
    sidebarMenu(
      menuItem("Intro", tabName = "intro", icon = icon("book")),
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
      tabItem(tabName = "intro",
              img(src = 'https://d21xlh2maitm24.cloudfront.net/nyc/CitiBike_Logo_p.svg?mtime=20160427183115'),
              h1('a headline'),
              p('some text')),
              
      tabItem(tabName = 'when',
              fluidRow(
                box(title = "Step 1- Find the best day and time of the day",
                    plotlyOutput('heatmap', height = 600),
                    width = 8),
                box(
                  title = 'Filter your search',
                  selectInput(inputId = 'gender_heatmap', 
                              label = 'Gender:', 
                              choices = Gender, 
                              selected = Gender[1]),
                  selectInput(inputId = 'agegroup_heatmap', 
                              label = 'Age:', 
                              choices = Agegroup, 
                              selected = Agegroup[1]),
                  width = 4
                )
              )
      ),
      
      tabItem(tabName = "where",
              fluidRow(
                box(title = 'Step 2- Locate the most concentrated areas',
                    leafletOutput('tile', height = 600),
                    width = 8),
                box(
                  title = 'Filter your search',
                  selectInput(inputId = 'gender_tile', 
                              label = 'Gender:', 
                              choices = Gender, 
                              selected = Gender[1]),
                  sliderInput(inputId = "age_tile", 
                              label = "Age range:", 
                              17, 98, c(17,29)),
                  selectInput(inputId = 'dayofweek_tile', 
                              label = 'Day of week:', 
                              choices = Dayofweek, 
                              selected = Dayofweek[1]),
                  radioButtons(inputId = 'startstop_tile',
                               label = 'Time of day:',
                               choices = c('departing', 'arriving'),
                               inline = TRUE),
                  sliderInput(inputId = "time_tile", 
                              label = "", 
                              0, 24, c(6,10)),
                  width = 4
                )
              )
      ),
      
      
      tabItem(tabName = "how",
              fluidRow(infoBoxOutput("durationGoogle"),
                       infoBoxOutput("durationCitibike")),
              fluidRow(box(title = 'Step 3- Plan your ride',
                           leafletOutput('map', height = 500), 
                           width = 8),
                       
                       box(selectInput(inputId = 'start_map', 
                                       label = 'Departing from:', 
                                       choices = Startstationname, 
                                       selected = Startstationname[1]),
                           selectInput(inputId = 'stop_map', 
                                       label = 'Going to:', 
                                       choices = Stopstationname, 
                                       selected = Stopstationname[1]),
                           selectInput(inputId = 'dayofweek_map',
                                       label = 'Day of week:',
                                       choices = Dayofweek,
                                       selected = Dayofweek[1]),
                           selectInput(inputId = 'startrange_map',
                                       label = 'Leave at (time of the day):',
                                       choices = Startrange,
                                       selected = Startrange[1]),
                            width = 4)
              )
      )
    )
  )
)
)
