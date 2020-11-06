library(tidyr)
library(RNetCDF)
library(tidync)
library(ncmeta)
library(dplyr)
library(ggplot2)

#### ERA-Interim dataset is a subset region covering CONUS and extending into Canada and Mexico.
#### This is a historical dataset ranging from 1979-01-01 to 2017-12-31. 
#### The following are paths to the nc files. This can be changed to whatever fits your needs
#### I downloaded the nc files to my local machine and the files are in a data folder under
#### my project folder. 
file.maxt <- "data/era-interim/MAXT.nc" # MAXT variable or Maximum temperature
file.mint <- "data/era-interim/MINT.nc" # MINT variable or Minimum temperature
file.prec <- "data/era-interim/PREC.nc" # PREC variable or Precipitation

#### Lazy load NetCDF file. Important because we are dealing with huge datasets and we don't want
#### to load everything into memory
var.maxt <- tidync(file.maxt)
var.mint <- tidync(file.mint)
var.prec <- tidync(file.prec)

#### print() function summarizes NetCDF metadata on variables
print(list(var.maxt, var.mint, var.prec))

#### Set constraints on datset to query
#### For example, slice data where longitude is between 250 and 258; latitude is between 37 and 41; 
#### time is between 0 and 1095 or 1979-01-01 to 1981-12-31. In scientific terms, what are the maximum 
#### temperatures from 1/1/1979 to 12/31/1981 over the following coordinates: 
#### Latitude(37, 41) and Longitude(250, 258)
q <- t(data.frame(lon = c(250, 258),
                  lat = c(37, 41),
                  tim = c(0, 1095), 
                  row.names = c("min", "max")))

#### Slice MAXT variable by contraint set above
maxt.slc <- var.maxt %>% hyper_filter(lon = between(lon, q["lon", "min"], q["lon", "max"]), 
                                      lat = between(lat, q["lat", "min"], q["lat", "max"]), 
                                      time = between(time, q["tim", "min"], q["tim", "max"]))

#### Slice MINT variable by contraint set above
mint.slc <- var.mint %>% hyper_filter(lon = between(lon, q["lon", "min"], q["lon", "max"]), 
                                      lat = between(lat, q["lat", "min"], q["lat", "max"]), 
                                      time = between(time, q["tim", "min"], q["tim", "max"]))

#### Slice PREC variable by contraint set above
prec.slc <- var.prec %>% hyper_filter(lon = between(lon, q["lon", "min"], q["lon", "max"]), 
                                      lat = between(lat, q["lat", "min"], q["lat", "max"]), 
                                      time = between(time, q["tim", "min"], q["tim", "max"]))

# We can now query the dataset and retrieve real data as a data.frame
maxt.slc <- maxt.slc %>% hyper_tibble()
mint.slc <- mint.slc %>% hyper_tibble()
prec.slc <- prec.slc %>% hyper_tibble()

#### Custom function to parse NetCDF dates
ParseNetCDFTime <- function(variableMetadata, variableData) {
  library(ncmeta)
  library(RNetCDF)
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

#### Add new variable with human readable time
maxt.slc <- maxt.slc %>% mutate(time1=ParseNetCDFTime(file.maxt, maxt.slc))
mint.slc <- mint.slc %>% mutate(time1=ParseNetCDFTime(file.mint, mint.slc))
prec.slc <- prec.slc %>% mutate(time1=ParseNetCDFTime(file.prec, prec.slc))

#### Combine all data to one "complete" dataset for analysis. Here is where we
#### added MINT, MAXT, PREC, and possibly any other spatial dataset, i.e. 
#### water quality measurements, forest data, migratory patterns, etc. 
#### The challenge will probably be in merging spatial data on different scales.
#### Maybe we could just place the data that falls in the square space of the climate data.
dataset <- full_join(maxt.slc, mint.slc) %>% full_join(prec.slc) %>% select(MINT, MAXT, PREC, lon, lat, time) 

#### Plot maximum temperature slice 
ggplot(dataset) +
  aes(x = lon, y = lat, fill = MAXT) +
  geom_tile() +
  scale_fill_distiller(palette = "Reds")
