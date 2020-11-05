library(fields)
library(maps)
library(ncdf4)

f <- nc_open("MAXT.nc")

lat = ncvar_get(f, "lat")
lon = ncvar_get(f, "lon")
tm = ncvar_get(f, "time")
tm = as.Date(tm, origin = "1979-01-01", tz = "UTC")
maxt = ncvar_get(f, "MAXT", start = c(1,1,1), count = c(-1,-1,1))

image.plot(lon,lat,maxt)

lat_rng = c(37,41)
lon_rng = c(250,258)

lat_ind = which(lat >= lat_rng[1] & lat <= lat_rng[2])
lon_ind = which(lon >= lon_rng[1] & lon <= lon_rng[2])

maxt_ts = ncvar_get(f, "MAXT", start = c(lon_ind[1], lat_ind[1], 1), count = c(length((lon_ind)),length(lat_ind),-1))
maxt_ts = apply(maxt_ts, 3, mean, na.rm = T)

plot(tm, maxt_ts, type = "l")
