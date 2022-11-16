## behind the scenes script for the climate velocity tutorial:
library(raster)
library(tidyverse)
library(ncdf4)

## create rasterstack of Berkeley Earth air temperatures 
years = seq(1880, 2020, by = 10)
files = paste("data-raw/climate/Global_BerkeleyEarth/Complete_TAVG_Daily_LatLong1_", years, ".nc", sep = "")
dates = c()
temps = stack()
mean_temps = stack()

i=1
while (i <= length(files)) {
  cur_nc = nc_open(files[i])
  y = ncvar_get(cur_nc, "year")
  m = ncvar_get(cur_nc, "month")
  d = ncvar_get(cur_nc, "day")
  nc_close(cur_nc)
  
  ymd = paste(y, m, d, sep = ".")
  
  dates = append(dates, ymd)
  
  cur = brick(files[i], var = "temperature")
  
  temps = stack(temps, cur)
  
  ncores <- 8 # define the number of cores to use
  beginCluster(ncores)
  
  ## split into years and calculate average across year
  n = 1
  index = 1
  while (n <= length(unique(y))) {
    year_length = length(which(y == unique(y)[n]))
    index2 = length(which(y == unique(y)[n])) + index - 1
    
    year = cur[[index:index2]]
    
    cur_mean <- clusterR(year, mean, args = list(na.rm=TRUE))
    
    mean_temps = stack(mean_temps, cur_mean)
    
    n = n + 1
    index = index + year_length
  }
  endCluster()
  
  i = i + 1
}

## change names to dates 
names(temps) = dates
## years
names(mean_temps) = 1880:2021

## save the rasterstack as an rds:
saveRDS(temps, "R/teaching/BerkEarth_rstack.rds")
saveRDS(mean_temps, "R/teaching/BerkEarth_rstack_mean.rds")

