library(shinydashboard)

ageGroup <- sort(unique(df$agegroup))

dashboardPage(
  dashboardHeader(title = 'Citi Tinder'),
  
  #sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("When", tabName = "when", icon = icon("calendar-times-o")),
      menuItem("Where", tabName = "where", icon = icon("map-marker")),
      menuItem("How", tabName = "how", icon = icon("map-signs"))
    )
  ),
  
  # body content
  dashboardBody(
    tabItems(
      # first tab content
      tabItem(tabName = 'when',
              fluidRow(
                box(plotOutput('heatmap', height = 250)),
                box(
                  title = 'Filter your search',
                  selectInput(inputId = 'gender', 
                              label = 'Gender:', 
                              choices = c('female','male'), 
                              selected = 'male'),
                  selectInput(inputId = 'agegroup', 
                              label = 'Age:', 
                              choices = ageGroup, 
                              selected = ageGroup[1])
                )
              )
      ),
      # second tab content
      tabItem(tabName = "where",
              h2("Widgets tab content")
      )
      
      # third tab content
      tabItem(tabName = "where",
              h2("Widgets tab content")
      )
      
    )
  )
  
  
  
  