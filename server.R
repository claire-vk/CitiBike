library(shinydashboard)

shinyServer(
  function(input, output, session) {
    
    signals <- reactiveValues(dayofweek_map = TRUE,
                              hour_map = TRUE,
                              leaflet_map = TRUE)
      
    heatmap_df = reactive({
      df = df %>% select(gender, agegroup, dayofweek, startrange) %>% 
        filter(gender == input$gender_heatmap, 
               agegroup == input$agegroup_heatmap) %>%
        group_by(gender, 
                 agegroup, 
                 startrange, 
                 dayofweek) %>% 
        summarise(count = n())
      df$dayofweek = factor(df$dayofweek, 
                            levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
      df
    })

    
    tile_df = reactive({
      melt_df = melt_df[sample(nrow(melt_df),replace=F,size=0.1*nrow(melt_df)),]
      
      melt_df %>% filter(gender == input$gender_tile, 
                         age > input$age_tile[1], age < input$age_tile[2], 
                         hour > input$time_tile[1], hour < input$time_tile[2], 
                         dayofweek == input$dayofweek_tile, 
                         time == switch(input$startstop_tile,'departing' = 'start','arriving' = 'stop'))
      })
    
    # observe({
    # end.station.name = unique(dir_df[start.station.name==input$start_map, end.station.name])
    # updateSelectizeInput(
    #   session, "stop_map",
    #   choices = end.station.name,
    #   selected = end.station.name[1])
    # })
    
    observeEvent(input$start_map, {
      
      choices <- dir_df %>%
        filter(start.station.name == input$start_map) %>%
        select(end.station.name)
      
      choices <- unique(choices[,1])
      
      updateSelectInput(session = session, 
                        inputId = 'stop_map', 
                        choices = choices, 
                        selected = choices[1])
      signals$dayofweek_map <- FALSE
      signals$leaflet_map <- FALSE
    })
    
    observeEvent(signals$dayofweek_map, {
      signals$dayofweek_map <- TRUE
      choices <- dir_df %>%
        filter(start.station.name == input$start_map,
               end.station.name == input$stop_map) %>%
        select(dayofweek)
      
      choices <- unique(choices[,1])
      
      updateSelectInput(session = session,
                        inputId = 'dayofweek_map',
                        choices = choices,
                        selected = choices[1])
      signals$hour_map <- FALSE
    })
    
    observeEvent(signals$hour_map, {
      signals$hour_map <- TRUE
      choices <- dir_df %>%
        filter(start.station.name == input$start_map,
               end.station.name == input$stop_map,
               dayofweek == input$dayofweek_map) %>%
        select(starthour)
      
      choices <- unique(choices[,1])
      updateSelectInput(session = session,
                        inputId = 'hour_map',
                        choices = choices,
                        selected = choices[1])
      signals$leaflet_map <- FALSE
    })
    
    map_df = reactive({
      dir_df %>%
      filter (start.station.name == input$start_map,
              end.station.name == input$stop_map,
              dayofweek == input$dayofweek_map,
              starthour == input$hour_map) %>%
      group_by(start.lat_long, end.lat_long) %>%
      summarise(avg_duration = mean(tripduration.min))
    })

    # map_df = reactive({
    #   temp <- dir_df %>%
    #   filter (start.station.name == input$start_map,
    #           end.station.name == input$stop_map)
    #   print("====================")
    #   print(input$dayofweek_input)
    #   if(!is.null(input$dayofweek_input)){
    #     if(input$dayofweek_input !=""){
    #       print('I am here')
    #       temp <- temp %>%
    #           filter(dayofweek == input$dayofweek_input)
    #   }}
    #   # 
    #   # if(!is.null(input$hour_input)){
    #   #   temp <- temp %>% 
    #   #     filter(starthour == input$hour_input)
    #   # }
    #   return (temp)
    #   
    #           
    #           # dayofweek == input$dayofweek_map,
    #           # starthour == input$hour_map) %>%
    #   # group_by(start.lat_long, end.lat_long) %>%
    #   # summarise(avg_duration = mean(tripduration.min))
    # })
    #   
    # output$dayofweek_output <- renderUI({
    #   if(is.null(input$dayofweek_input)){
    #     print('*************')
    #     print('dayofweek is null')
    #     selectInput(inputId = 'dayofweek_input',
    #                 label = 'Day of week:',
    #                 choices = c('Monday', 'Tuesday'),
    #                 selected = NULL)
    #   }else{
    #     selectInput(inputId = 'dayofweek_input',
    #                 label = 'Day of week:',
    #                 choices = map_df()$dayofweek)
    #   }
    # 
    # })
    
    # output$hour_output <- renderUI({
    #   print(head(map_df()))
    #   selectInput(inputId = 'hour_input',
    #               label = 'Leave at (hour of the day):',
    #               choices = unique(map_df()$hour),
    #               selected = NULL)
    # })
    # 
    output$heatmap = renderPlotly({
      g = ggplot(heatmap_df(), aes(x=startrange, y=dayofweek, fill=count)) +
        geom_tile(color="white", size=0.1) +
        scale_x_discrete(expand = c(0, 0)) +
        labs(x=NULL, y=NULL) +
        scale_fill_gradient(low = "#deebf7", high = "#3182bd", name = "# Riders") +
        theme_tufte(base_family="Helvetica") +
        theme(plot.title=element_text(hjust=0)) +
        theme(axis.ticks=element_blank()) +
        theme(axis.text=element_text(size=10)) +
        theme(legend.title=element_text(size=12)) +
        theme(legend.text=element_text(size=10)) +
        theme(legend.key.size=unit(0.2, "cm")) +
        theme(legend.key.width=unit(1, "cm"))
      
      ggplotly(g) %>% config(displayModeBar = F)
    })
    
    output$tile = renderLeaflet({
      # arr_dep = switch(input$startstop_tile,
      #                  'departing' = ,
      #                  'arriving' =)

      leaflet(data = tile_df()) %>% 
        addTiles() %>% 
        addMarkers(~longitude, 
                   ~latitude, 
                   popup = ~name, 
                   label = ~name, 
                   icon = bike_icon) %>% 
        addProviderTiles('Esri.NatGeoWorldMap')
    })
    
    output$map <- renderLeaflet({
      coords <- dir_df %>%
        filter (start.station.name == isolate(input$start_map),
                end.station.name == input$stop_map) %>%
        select(start.lat_long,
               end.lat_long)
      origin = coords$start.lat_long[1]
      destination = coords$end.lat_long[1]
      
      res = google_directions(origin = origin,
                              destination = destination,
                              key = key)
      
      print(paste(origin, destination))
      print(res)
      
      df_polyline = decode_pl(res$routes$overview_polyline$points)
      
      leaflet() %>%
        addTiles() %>%
        addPolylines(data = df_polyline, lat = ~lat, lng = ~lon)
    })
    
    output$durationGoogle = renderInfoBox({
      coords <- dir_df %>%
        filter (start.station.name == isolate(input$start_map),
                end.station.name == input$stop_map) %>%
        select(start.lat_long,
               end.lat_long)
      origin = coords$start.lat_long[1]
      destination = coords$end.lat_long[1]
      google_time = select(mapdist(origin, destination, mode='bicycling'), minutes)
      infoBox("Google estimated duration:", paste(google_time, 'min'), icon = icon("google"), color = 'orange', fill = TRUE)
    })
    
    output$durationCitibike = renderInfoBox({
      citibike_time = map_df()
      infoBox("Citi Bike estimated duration:", paste(citibike_time$avg_duration, 'min'), icon = icon("bicycle"), fill = TRUE)
    })
    
  }
)

