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
      menuItem("The Data", tabName = "intro", icon = icon("book")),
      menuItem("Step 1- When", tabName = "when", icon = icon("calendar-times-o")),
      menuItem("Step 2- Where", tabName = "where", icon = icon("map-marker")),
      menuItem("Step 3- How", tabName = "how", icon = icon("map-signs"))
    )
  ),
  
  # body content
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    tabItems(
      tabItem(tabName = "intro",
              wellPanel(fluidRow(
                column(6,
                  h1("Using NYC Citi Bike Data to Help Bike Enthusiasts Find their Mate"),
                  br(),
                  p("There is no shortage of analyses on the NYC bike share system. 
                     Most of them aim at predicting the demand for bikes and balancing bike stock, 
                     i.e forecasting when to remove bikes from fully occupied stations, and refill 
                     stations before the supply runs dry.", 
                     style= "font-size: 18px"),
                  br(),
                  p("This is why I decided to take a different approach and use the Citi Bike data 
                     to help its users instead.",
                    style= "font-size: 18px")),
                  column(6,
                    img(src = 'https://d21xlh2maitm24.cloudfront.net/nyc/day-passes.png?mtime=20170331123924', 
                        width=450, height=300, alt="Citi Bike couple")))),

              wellPanel(fluidRow(
                column(6,
                  img(src = 'http://images2.miaminewtimes.com/imager/citi-bike-launches-in-miami-this-saturday/u/original/6505436/citibike_wynwood_colorful.jpg', 
                        width=450, height=300, alt="Citi Bike couple with graffiti in bg")),
                  column(6,
                    h2("The Challenge"),
                    br(),
                    p("The online dating scene is complicated and unreliable: 
                      there is a discrepancy between what online daters say and what they do. 
                      Although this challenge is not relevant to me anymore - I am married - I wished that, as a bike enthusiast, 
                      I had a platform where I could have spotted like-minded people who did ride a bike (and not just pretend they did).", 
                      style= "font-size: 18px"),
                    br(),
                    p("The goal of this project was to turn the Citi Bike data into an app where a rider could identify the best 
                      spots and times to meet other Citi Bike users and cyclists in general.", 
                      style= "font-size: 18px")))),

              wellPanel(fluidRow(
                column(6,
                  h2("The Data"),
                  br(),
                  p("As of March 31, 2016, the total number of annual subscribers was 163,865, and Citi Bike 
                    riders took an average of 38,491 rides per day in 2016 (source:" ,a("wikipedia)", href= "https://en.wikipedia.org/wiki/Citi_Bike", target="_blank"), 
                  p("This is more than 14 million rides in 2016!", style= "font-size: 18px"), style= "font-size: 18px"),
                  br(),
                  p("I used the Citi Bike data for the month of May 2016 (approximately 1 million observations). Citi Bike provides 
                    the following variables:",style= "font-size: 18px"),
                  tags$div(tags$ul(
                    tags$li(tags$span("Trip duration (in seconds)")), 
                    tags$li(tags$span("Timestamps for when the trip started and ended")), 
                    tags$li(tags$span("Station locations for where the trip started and ended (both the names and coordinates)")),
                    tags$li(tags$span("Rider’s gender and birth year - this is the only demographic data we have.")),
                    tags$li(tags$span("Rider’s plan (annual subscriber, 7-day pass user or 1-day pass user)")),
                    tags$li(tags$span("Bike ID"))), style= "font-size: 18px")),
                  column(6,
                    img(src = 'https://d21xlh2maitm24.cloudfront.net/nyc/day-passes.png?mtime=20170331123924', 
                        width=450, height=300, alt="Citi Bike couple")))),

              plotOutput('histogram'),
              plotOutput('boxplt'),
              tableOutput('duration_med'),
              plotOutput('density'),
              plotOutput('weekdays'),
              plotOutput('weekends'),
              plotOutput('medianage')
              ),
              
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
