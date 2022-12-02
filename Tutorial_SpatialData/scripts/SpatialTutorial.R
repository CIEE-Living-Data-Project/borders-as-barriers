## Working with spatial data 
# Andrea Brown & Isaac Eckert

# About Waterfowl Breeding Population and Habitat Survey:
# https://www.canada.ca/en/environment-climate-change/services/bird-surveys/waterfowl/breeding-population-habitat-survey.html 
# WBPHS data from: https://www.fws.gov/project/waterfowl-breeding-population-and-habitat-survey
# https://ecos.fws.gov/ServCat/Reference/Profile/140698


library(sf)
library(sp) 
library(raster)
library(ggspatial)
library(ggplot2)


#setwd("C:/Users/andre/OneDrive - McGill University/Borders_WG/SpatialData Tutorial")

## Canada and US outline - this will be used as the reference CRS ==============
canusa <- raster("data/USCanadaOutline.tif")
crs(canusa) # check the coordinate system
plot(canusa) 


## Bring in study area shapefile for the western boreal ==============
study <- st_read("data/WBI shapefile/WBI Study Area.shp")
plot(study) # take a look

# set crs to be the same as canusa
study_sp <- st_transform(study, crs = st_crs(canusa)) 
plot(st_geometry(study_sp)) # to plot outline of study area

# add 500 km buffer around study area if we want
study_buf <- st_buffer(study_sp, dist = 500000) 
plot(study_buf)

rm(study) # we no long need this object, remove from R environment


## Bring in survery transects ==============
segments <- st_read("data/WBPHS shapefile/MAS_segments.shp")
plot(segments) # what are we looking at here?

# convert crs of segments to the same are canusa
seg_sp <- st_transform(segments, crs(canusa)) 

# Crop segments to buffered study area
# To crop the segments to the buffered study area you can st_crop BUT it only crops to the maximum extent of 
# the buffered study area (crops to a rectangle); to crop to the actual polygon we use st_intersection
st_crs(seg_sp) == st_crs(study_buf) # make sure they have the same crs before cropping

seg_crop <- st_intersection(seg_sp, study_buf) 
plot(seg_crop)

rm(segments, seg_sp) 


## Plot using base R ==============
plot(canusa, ext = c(-5e+06, 5e+06, 5e+06, 4e+07), axes=F, box=F, legend = F)
plot(study_buf, col= alpha("red", 0.5), border = "transparent", add = T)
plot(study_sp, col = "grey", add = T)
plot(seg_crop, col = "black", lwd = 0.5, add = T)
title(main = "WBI survey area and smapling transects")



## Plot using ggplot ==============
library(rnaturalearth)
library(ggspatial)

# make everything into sf objects, easier to plot with ggplot
# study area
study_sp2 <- st_as_sf(study_sp)
# buffered study area
study_buf2 <- st_as_sf(study_buf)
# cropped segments
seg_crop2 <- st_as_sf(seg_crop)

  
# Get a shapefile of all countries from the rnaturalearth package
world <- ne_countries(scale = "medium",
                      returnclass = "sf")
# filter for the countries you want for your plot
world <- world %>% 
  subset(continent == "North America",
         adm0_a3 == "CAN" | adm0_a3 == "USA")
# change crs to be the same as the study area, buffered study area and segments
world <- st_transform(world, crs(study_sp2))
# set bounding box the zoom in on North America, you can play around with these numbers to zoom in or out
box <- c(xmin = -3717148, xmax = 2558043.4, ymin = 5886034, ymax = 11860535.3)
# crop world shpaefile to your bouding box
world <- st_crop(world, box)

  
# plot!
ggplot() +
  geom_sf(data = world, aes(geometry = geometry), fill = "yellowgreen", size =0.5) +
  geom_sf(data = study_buf2,
          aes(geometry = geometry),
          fill = "grey90", alpha = 0.75, color = "grey55") +
  geom_sf(data = study_sp2, aes(geometry = geometry), show.legend = F,
          fill = "red", alpha = 0.2) +
  geom_sf(data = seg_crop2,aes(geometry = geometry), 
          show.legend =  F) +
  annotation_scale(location = "br", width_hint = 0.3, pad_x = unit(2.5, "cm")) +
  annotation_north_arrow(which_north = "true", location = "br", width = unit(0.95, "cm"),
                         pad_x = unit(0.2, "cm")) +
  ggthemes::theme_map() +
  labs(title = "WBI survey area and smapling transects") +
  theme(plot.title = element_text(size = 20, face = "bold"))
  


