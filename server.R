library(shiny)

# Define server logic for slider examples
function(input, output) {
  
  # Reactive expression to compose a data frame containing all of
  # the values
  output$values = renderPlot({
    ageGender = df[,c('age','gender')]
    
    minRange = input$age[1]
    maxRange = input$age[2]
    
    ageGender %>% 
      filter(age >= minRange & age <= maxRange & gender == input$gender) %>% 
          ggplot(aes(x=age)) +
          geom_line(aes(color = gender), stat = 'density', alpha = 0.5, size = 2, adjust = 5) +
          labs(title = paste('Distribution of', input$gender, 'users from', minRange,
                         'to', maxRange, 'years old using Citi Bike')) +
          theme_pander() +
          theme(axis.text.y=element_blank(),axis.title.y=element_blank(),legend.position="none")
  })
}