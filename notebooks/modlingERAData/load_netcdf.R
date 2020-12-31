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