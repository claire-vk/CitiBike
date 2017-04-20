library(shinydashboard)

shinyServer(
  function(input, output, session) {
    
    tile_df = reactive({
      df %>% select(gender, agegroup, dayofweek, startrange) %>% 
        filter(gender == 'female', agegroup =='17-24') %>%
        group_by(gender, agegroup, startrange, dayofweek) %>% 
        summarise(count = n())
    })
    
    # observe({
    # tile_df()$dayofweek = factor(tile_df$dayofweek, levels = c("Sunday", "Saturday", "Friday", "Thursday", "Wednesday", "Tuesday", "Monday"))
    # tile_df()[order(tile_df$dayofweek), ]
    # })
    
    
    map_df = reactive({
      melt_df %>% filter(gender == 'female', 
                         age > 17, age < 25, 
                         hour > 1, hour <5, 
                         dayofweek == 'Friday', 
                         time == 'start')
    })
    
    direction_df = reactive({
      dir_df %>% 
      filter (start.station.name == '1 Ave & E 30 St', 
              end.station.name == 'E 17 St & Broadway', 
              dayofweek == 'Sunday', 
              starthour == '0') %>%
      group_by(start.lat_long, end.lat_long) %>%
      summarise(avg_duration = mean(tripduration.min))
    })
    
    output$heatmap = renderPlotly({
      g = ggplot(tile_df(), aes(x=startrange, y=dayofweek, fill=count)) +
        geom_tile(color="white", size=0.1) +
        scale_x_discrete(expand = c(0, 0)) +
        labs(x=NULL, y=NULL, title="Citi Bike users per day and hour") +
        scale_fill_gradient(low = "#deebf7", high = "#3182bd", name = "# Riders") +
        theme_tufte(base_family="Helvetica") +
        theme(plot.title=element_text(hjust=0)) +
        theme(axis.ticks=element_blank()) +
        theme(axis.text=element_text(size=7)) +
        theme(legend.title=element_text(size=8)) +
        theme(legend.text=element_text(size=6)) +
        theme(legend.key.size=unit(0.2, "cm")) +
        theme(legend.key.width=unit(1, "cm")) +
        facet_wrap(gender~agegroup)
      
      ggplotly(g)
    })
    
    output$tile = renderLeaflet({
      leaflet(data = map_df()) %>% 
        addTiles() %>% 
        addMarkers(~longitude, 
                   ~latitude, 
                   popup = ~name, 
                   label = ~name, 
                   icon = bike_icon) %>% 
        addProviderTiles('Esri.NatGeoWorldMap')
    })
    
    output$map = renderLeaflet({
      origin = direction_df()$start.lat_long  
      destination = direction_df()$end.lat_long 
      google_time = select(mapdist(origin, destination, mode='bicycling'), minutes)
    
      res = google_directions(origin = origin,
                              destination = destination,
                              key = key)
      
      df_polyline <- decode_pl(res$routes$overview_polyline$points)
      
      leaflet() %>%
        addTiles() %>%
        addPolylines(data = df_polyline, lat = ~lat, lng = ~lon)
    })
    
    
    output$durationGoogle = renderInfoBox({
      min_value <- min(state_stat[,input$selected])
      infoBox(min_value, icon = icon("hand-o-down"))
    })
    
    
    output$durationCitibike = renderInfoBox(
      infoBox(paste("AVG.", input$selected), mean(state_stat[,input$selected]),
              icon = icon("calculator"), fill = TRUE))
    
  }
)

