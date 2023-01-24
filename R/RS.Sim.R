#Simulate range shifts using traits and resistance 
library(raster)
library(dismo)
library(gbm)
library(mgcv)
library(sf)
library(terra)

#gridded base
canusa <- raster("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Documents/Spatial Datasets/Rasters/North America/Layers for SDMs/USCanadaOutline.tif")

#only projectv to 1000km buffer
buffer<-vect("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Documents/Spatial Datasets/Vectors/North America/Border Buffer/Border Shapefiles/Buffers/1000KM.Buffer.shp")
buffer<-project((buffer),rast(canusa))
buffer.rast<-terra::rasterize((buffer),rast(canusa))
plot(buffer.rast)

#load border line to add to plots
border<-vect("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Documents/Spatial Datasets/Vectors/North America/Border Buffer/Border Shapefiles/Canada_and_US_Border/Canada_and_US_Border.shp")
border<-project(border,rast(canusa))

#read in projections
species="Marmota olympus"

data<-readRDS(paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Clipped Projections 0km/",species,".1k",".rds"))
head(data)

#make NAs 0
data[is.na(data)]<-0

#convert back into probabilities (0-1)
data<-round(apply(data,2,function (x) x/100),2)

r <- raster(buffer.rast)
r[(which(values(buffer.rast)==1))] <- data[,"BRT.Current"]
r[(is.na(canusa))]<-NA
r<-trim(r)
plot(r,axes=F,box=F,axis.args=arg,legend=F)
plot(border,add=T)

#threshold distribution
r[r<0.1]<-0
r[r>0]<-1

plot(r,col=c("grey90","forestgreen"),axes=F,box=F,axis.args=arg,legend=F)
plot(border,add=T)

#set random dispersal limit
dispersal.rate=1 #km/year
max.dispersal=dispersal.rate*60 #since we are talking about 2020-2080

#set all 0 to NA
r[r==0]<-NA

r.dispersal<-terra::rasterize(terra::buffer(simplifyGeom(as.polygons(rast(r),dissolve=T),tolerance=10000000000),width=max.dispersal*1000),rast(r))
plot(r.dispersal)

#future dist
f <- raster(buffer.rast)
f[(which(values(buffer.rast)==1))] <- data[,"BRT.Future"]
f[(is.na(canusa))]<-NA
f<-trim(f)
plot(f,axes=F,box=F,legend=F)
plot(border,add=T)

#threshold distribution
f[f<0.1]<-0
f[f>0]<-1

plot(f,col=c("grey90","forestgreen"),axes=F,legend=F,box=F)
plot(border,add=T)
plot(as.polygons(r.dispersal),add=T)

#crop future outside of dispersal polygon
f[f>0 & is.na(raster(r.dispersal))]<-0
plot(f,col=c("grey90","forestgreen"),axes=F,legend=F,box=F)
plot(border,add=T)
plot(as.polygons(r.dispersal),add=T)



#hello eveyone!




