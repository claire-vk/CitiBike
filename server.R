library(shinydashboard)

shinyServer(
  function(input, output, session) {
    
    signals <- reactiveValues(dayofweek_map = TRUE,
                              startrange_map = TRUE,
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
      
      colnames(df)[which(names(df) == "startrange")] <- "Time"
      colnames(df)[which(names(df) == "dayofweek")] <- "Day"
      colnames(df)[which(names(df) == "count")] <- "Count"

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
    
    observeEvent(input$start_map, {
      
      choices <- dir_df %>%
        filter(start.station.name == input$start_map) %>%
        select(end.station.name)
      
      choices <- sort(unique(choices[,1]))
      
      updateSelectInput(session = session, 
                        inputId = 'stop_map', 
                        choices = choices, 
                        selected = choices[2])
      signals$dayofweek_map <- FALSE
      signals$leaflet_map <- FALSE
    })
    
    observeEvent(signals$dayofweek_map, {
      signals$dayofweek_map <- TRUE
      choices <- dir_df %>%
        filter(start.station.name == input$start_map,
               end.station.name == input$stop_map) %>%
        select(dayofweek)
      
      choices <- levels(dir_df$dayofweek)
      choices <- unique(choices[,1])
      
      updateSelectInput(session = session,
                        inputId = 'dayofweek_map',
                        choices = choices,
                        selected = choices[1])
      signals$startrange_map <- FALSE
      signals$leaflet_map <- FALSE
    })
    
    observeEvent(signals$startrange_map, {
      signals$startrange_map <- TRUE
      choices <- dir_df %>%
        filter(start.station.name == input$start_map,
               end.station.name == input$stop_map,
               dayofweek == input$dayofweek_map) %>%
        select(startrange)
      
      choices <- sort(unique(choices[,1]))
      
      updateSelectInput(session = session,
                        inputId = 'startrange_map',
                        choices = choices,
                        selected = choices[1])
      signals$leaflet_map <- FALSE
    })
    
    map_df = reactive({
      dir_df %>%
        filter (start.station.name == input$start_map,
                end.station.name == input$stop_map,
                dayofweek == input$dayofweek_map,
                startrange == input$startrange_map) %>%
        group_by(start.lat_long, end.lat_long) %>%
        summarise(avg_duration = mean(tripduration.min))
    })
    
    output$heatmap = renderPlotly({
       g = ggplot(heatmap_df(), aes(x=Time, y=Day, fill=Count)) +
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
        infoBox("Google estimated duration:", paste(round(google_time,1), 'min'), icon = icon("google"), color = 'orange', fill = TRUE)
      })
      
      output$durationCitibike = renderInfoBox({
        citibike_time = dir_df %>%
          filter (start.station.name == input$start_map,
                  end.station.name == input$stop_map,
                  dayofweek == input$dayofweek_map,
                  startrange == input$startrange_map) %>%
          group_by(start.lat_long, end.lat_long, startrange, dayofweek) %>%
          summarise(avg_duration = mean(tripduration.min, na.rm=TRUE))
        
        if(length(citibike_time$avg_duration)==0){
          tmp = 'Not provided'
        }else{
          tmp = paste(round(citibike_time$avg_duration,1), 'min')
        }
        
        infoBox("CitiBike estimated duration:",
                tmp, 
                icon = icon("bicycle"), fill = TRUE)
      })
      
  }
    )
    
    