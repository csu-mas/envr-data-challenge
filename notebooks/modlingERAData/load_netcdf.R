#### Function designed to load in a netCDF file for use in a notebook
#### Simply transforming Phong's original data wrangling script and making functions for:
#### -Loading netCDF files into R: getCDF.data()
#### -Parsing the time from the netCDF file to a useful date/time object: ParseNetCDFTime()


getCDF.data <- function(filePath, lonRange, latRange, timeRange, asCube = FALSE){
  
  ## Require packages
  
  require(dplyr)
  require(tidync)
  require(RNetCDF)
  
  ## Constraint data-frame for coordinates at fixed time
  
  q <- t(data.frame(lon = lonRange, 
                    lat = latRange, 
                    tim = timeRange, 
                    row.names = c("min", "max")))
  
  ## Lazy load the CDF file
  
  data <- tidync(filePath)
  
  ## Slice the data based on input
  
  data <- data %>% hyper_filter(lon = between(lon, q["lon", "min"], q["lon", "max"]), 
                                lat = between(lat, q["lat", "min"], q["lat", "max"]), 
                                time = between(time, q["tim", "min"], q["tim", "max"])) 

  if(asCube) {
    data %>% hyper_tbl_cube() 
  } else {
    data %>% hyper_tibble()
  }
}


#### Custom function to parse NetCDF dates
ParseNetCDFTime <- function(variableMetadata, variableData) {
  
  require(ncmeta)
  require(RNetCDF)
  
  time.unit <- nc_atts(variableMetadata, "time") %>% unnest(cols = c(value)) %>% filter(name == "units")
  time.parts <- utcal.nc(time.unit$value, variableData$time)
  ISOdatetime(time.parts[, "year"],
              time.parts[, "month"],
              time.parts[, "day"],
              time.parts[, "hour"],
              time.parts[, "minute"],
              time.parts[, "second"],
              tz = "UTC")
}

getTimeRange <- function(year, origin) {
  s <- as.Date(paste(year[1], 01, 01, sep = "-"))
  e <- as.Date(paste(year[2], 12, 31, sep = "-"))
  
  start <- as.integer(difftime(s, origin, units = "days"))
  end <- start + as.integer(difftime(e, s, units = "days"))
  
  c(start, end)
}

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