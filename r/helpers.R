## Get days since index based on input dates
getTimeRange <- function(dateStart, dateEnd, origin) {
  library(lubridate)
  
  dateStart <- as.Date(dateStart)
  dateEnd <- as.Date(dateEnd)
  
  s <- as.Date(paste(year(dateStart), month(dateStart), day(dateStart), sep = "-"))
  e <- as.Date(paste(year(dateEnd), month(dateEnd), day(dateEnd), sep = "-"))
  
  start <- as.integer(difftime(s, as.Date(origin) - 1, units = "days")) 
  end <- start + as.integer(difftime(e, s, units = "days"))
  
  c(start, end)
}
## Pre-defined CONUS map as a ggplot layer
conus.map <- function() {
  library(maps)
  library(sf)
  library(tools)
  
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
                coord_sf(xlim = c(-136.5, -58.5),
                         ylim = c(17.25, 55.5),
                         expand = FALSE))
  
  base.map
}
## Scree plot for cluster analysis
wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  
  for (i in 2:nc) {
    set.seed(seed)
    
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}