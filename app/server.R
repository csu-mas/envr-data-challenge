library(shiny)
library(ggplot2)
library(maps)
library(sf)
library(tools)
library(tidync)
library(dplyr)

map <- st_as_sf(map("world", plot = FALSE, fill = TRUE))
map <- cbind(map, st_coordinates(st_centroid(map)))
map$ID <- toTitleCase(as.vector(map$ID))
map$ID <- ifelse(map$ID == "USA", "", map$ID)

#### Retrieve US map plot points 
us.states <- map_data("state")
us.states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
us.states <- cbind(us.states, st_coordinates(st_centroid(us.states)))
us.states$ID <- toTitleCase(as.vector(us.states$ID))

world.map <- st_as_sf(map("world", plot = FALSE, fill = TRUE))
world.map <- cbind(map, st_coordinates(st_centroid(map)))

var.maxt <- tidync("../data/era-interim/MAXT.nc")
var.mint <- tidync("../data/era-interim/MINT.nc")
var.prec <- tidync("../data/era-interim/PREC.nc")

shinyServer(function(input, output, session) {
  output$dataset <- renderDataTable({
    maxt.slc <- var.maxt %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
    mint.slc <- var.mint %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
    prec.slc <- var.prec %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
    
    full_join(maxt.slc, mint.slc) %>% full_join(prec.slc) %>% 
      mutate(DIFF = MAXT - MINT, Longitude = lon - 360) %>% 
      select(Longitude, Latitude=lat, Max=MAXT, Min=MINT, Difference=DIFF, Precipitation=PREC)
  })

  output$date <- renderText(as.character(as.Date(input$timeFilter, origin = "1979-01-01")))

  output$maxt <- renderPlot({
    maxt.slc <- var.maxt %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
    
    ggplot() +
      geom_raster(data = maxt.slc, aes(x = lon - 360, y = lat, fill = MAXT)) +
      # Reds color pallete for heat map
      scale_fill_distiller(palette = "Reds") +
      geom_sf(data = world.map, fill = NA) +
      # Draws US states map
      geom_sf(data = us.states, fill = NA) +
      # Add US state names
      geom_text(data = us.states, aes(X, Y, label = ID), size = 2) +
      # Add country names
      geom_text(data = map, aes(X, Y, label = ID), size = 2) + 
      theme_bw() +
      ggtitle("Maximum") +
      coord_sf(xlim = c(min(var.maxt$transforms$lon$lon) - 360, 
                        max(var.maxt$transforms$lon$lon) - 360), 
               ylim = c(min(var.maxt$transforms$lat$lat), 
                        max(var.maxt$transforms$lat$lat)), 
               expand = FALSE) 
  })
  
  output$mint <- renderPlot({
    mint.slc <- var.mint %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
    
    ggplot() +
      geom_raster(data = mint.slc, aes(x = lon - 360, y = lat, fill = MINT)) +
      # Reds color pallete for heat map
      geom_sf(data = world.map, fill = NA) +
      # Draws US states map
      geom_sf(data = us.states, fill = NA) +
      # Add US state names
      geom_text(data = us.states, aes(X, Y, label = ID), size = 2) +
      # Add country names
      geom_text(data = map, aes(X, Y, label = ID), size = 2) + 
      theme_bw() +
      ggtitle("Minimum") +
      coord_sf(xlim = c(min(var.maxt$transforms$lon$lon) - 360, 
                        max(var.maxt$transforms$lon$lon) - 360), 
               ylim = c(min(var.maxt$transforms$lat$lat), 
                        max(var.maxt$transforms$lat$lat)), 
               expand = FALSE) 
  })

  output$diff <- renderPlot({
    maxt.slc <- var.maxt %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
    mint.slc <- var.mint %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
    
    data <- full_join(maxt.slc, mint.slc) %>% mutate(DIFF = MAXT - MINT)
    
    ggplot() +
      geom_raster(data = data, aes(x = lon - 360, y = lat, fill = DIFF)) +
      # Reds color pallete for heat map
      scale_fill_distiller(palette = "Purples") +
      geom_sf(data = world.map, fill = NA) +
      # Draws US states map
      geom_sf(data = us.states, fill = NA) +
      # Add US state names
      geom_text(data = us.states, aes(X, Y, label = ID), size = 2) +
      # Add country names
      geom_text(data = map, aes(X, Y, label = ID), size = 2) + 
      theme_bw() +
      ggtitle("Difference") +
      coord_sf(xlim = c(min(var.maxt$transforms$lon$lon) - 360, 
                        max(var.maxt$transforms$lon$lon) - 360), 
               ylim = c(min(var.maxt$transforms$lat$lat), 
                        max(var.maxt$transforms$lat$lat)), 
               expand = FALSE) 
  })
  
  output$prec <- renderPlot({
    prec.slc <- var.prec %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
    
    ggplot() +
      geom_raster(data = prec.slc, aes(x = lon - 360, y = lat, fill = PREC)) +
      # Reds color pallete for heat map
      scale_fill_distiller(palette = "Greens") +
      geom_sf(data = world.map, fill = NA) +
      # Draws US states map
      geom_sf(data = us.states, fill = NA) +
      # Add US state names
      geom_text(data = us.states, aes(X, Y, label = ID), size = 2) +
      # Add country names
      geom_text(data = map, aes(X, Y, label = ID), size = 2) + 
      theme_bw() +
      ggtitle("Precipitation") +
      coord_sf(xlim = c(min(var.maxt$transforms$lon$lon) - 360, 
                        max(var.maxt$transforms$lon$lon) - 360), 
               ylim = c(min(var.maxt$transforms$lat$lat), 
                        max(var.maxt$transforms$lat$lat)), 
               expand = FALSE) 
  })
})
