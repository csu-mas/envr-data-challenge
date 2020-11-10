library(shiny)
library(ggplot2)
library(maps)
library(sf)
library(tools)
library(tidync)
library(dplyr)

#### TODO: Add ability to load netCDF files from DropBox
var.maxt <- tidync("../data/era-interim/MAXT.nc")
var.mint <- tidync("../data/era-interim/MINT.nc")
var.prec <- tidync("../data/era-interim/PREC.nc")

#######################################################################################################
####################################         BASE MAP DATA         #################################### 
#######################################################################################################
# This map data is from ggplot2. Map data can easily be replaced with raster maps from Google Maps, 
# Statem, or OpenStreetMaps using the ggmaps package. The maps can be skinned with roads, terrain,
# buildings, etc. This may be useful depending on what our research question will be.
map <- st_as_sf(map("world", plot = FALSE, fill = TRUE))
map <- cbind(map, st_coordinates(st_centroid(map)))
map$ID <- toTitleCase(as.vector(map$ID))
map$ID <- ifelse(map$ID == "USA", "", map$ID)

us.states <- map_data("state")
us.states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
us.states <- cbind(us.states, st_coordinates(st_centroid(us.states)))
us.states$ID <- toTitleCase(as.vector(us.states$ID))

world.map <- st_as_sf(map("world", plot = FALSE, fill = TRUE))
world.map <- cbind(map, st_coordinates(st_centroid(map)))

base.map <- c(geom_sf(data = world.map, fill = NA),
              geom_text(data = map, aes(X, Y, label = ID), size = 2),
              geom_sf(data = us.states, fill = NA),
              geom_text(data = us.states, aes(X, Y, label = ID), size = 2),
              coord_sf(xlim = c(min(var.maxt$transforms$lon$lon) - 360, 
                                max(var.maxt$transforms$lon$lon) - 360), 
                       ylim = c(min(var.maxt$transforms$lat$lat), 
                                max(var.maxt$transforms$lat$lat)), 
                       expand = FALSE))

#######################################################################################################
#########################################         UI         ########################################## 
#######################################################################################################
ui <- function(request) { htmlTemplate("../app/template.html") }

#######################################################################################################
#######################################         SERVER         ######################################## 
#######################################################################################################
server <- function(input, output, session) {
  dataset <- reactive({
    switch (input$baseData,
            "CESM-LENS" = {
              
            },
            "ERA-Interim" = {
              maxt.slc <- var.maxt %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
              mint.slc <- var.mint %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
              prec.slc <- var.prec %>% hyper_filter(time = between(time, input$timeFilter, input$timeFilter)) %>% hyper_tibble()
              
              full_join(maxt.slc, mint.slc) %>% full_join(prec.slc) %>% 
                mutate(DIFF = MAXT - MINT, lon = lon - 360) %>% 
                select(lon, lat, MAXT, MINT, DIFF, PREC)
            },
            "WRF et. el." = {
              
            })
  })

  output$title <- renderText(input$baseData)
  output$coor <- renderText(paste("lon:", input$plot_hover$x[1],
                                  "lat:", input$plot_hover$y[1]))
  
  output$dataset <- renderDataTable({ dataset() })
  output$date <- renderText(as.character(as.Date(input$timeFilter, origin = "1979-01-01")))
  output$maxt <- renderPlot({
    ggplot() +
      ggtitle("Maximum") +
      geom_raster(data = dataset(), aes(x = lon, y = lat, fill = MAXT), interpolate = TRUE) +
      scale_fill_distiller(palette = "Reds") +
      base.map +
      theme_bw()
  })
  output$mint <- renderPlot({
    ggplot() +
      ggtitle("Minimum") +
      geom_raster(data = dataset(), aes(x = lon , y = lat, fill = MINT), interpolate = TRUE) +
      base.map +
      theme_bw()
  })
  output$diff <- renderPlot({
    ggplot() +
      ggtitle("Difference") +
      geom_raster(data = dataset(), aes(x = lon, y = lat, fill = DIFF), interpolate = TRUE) +
      scale_fill_distiller(palette = "Purples") +
      base.map +
      theme_bw()
  })
  output$prec <- renderPlot({
    ggplot() +
      ggtitle("Precipitation") +
      geom_raster(data = dataset(), aes(x = lon, y = lat, fill = PREC), interpolate = TRUE) +
      scale_fill_distiller(palette = "Greens") +
      base.map +
      theme_bw()
  })
}

shinyApp(ui, server, enableBookmarking = "url")

