#SDMS
library(raster)
library(dismo)
library(gbm)
library(mgcv)
library(sf)
library(terra)

#species list
species<-gsub(".gridpointsLCC.RDS","",list.files("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Datasets/GBIF Border Species List/GBIF Download/SpeciesGriddedInput/"))

#set wd to place to save models
setwd("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/")

#load env layers to predict from 
#gridded base
canusa <- raster("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Rasters/North America/Layers for SDMs/USCanadaOutline.tif")

#env layers (current)
cli <- c("MAP.tif","DD_0.tif","PAS.tif","CMD.tif","DD18.tif")
cli.n <- paste("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Rasters/North America/Layers for SDMs/EnvUsCan",cli,sep="_")
clim <- stack(cli.n)
rm(cli.n)
clim

#geo and human layers
noncli <- c("effort.tif","twi.tif","tri.tif","foot.tif","lcov.tif","silt.tif","ph.tif","orgC.tif")
noncli.n <- paste("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Rasters/North America/Layers for SDMs/EnvUsCan",noncli,sep="_")
nonclim <- stack(noncli.n)
rm(noncli,noncli.n)
nonclim

#combine
env <- stack(clim,nonclim)
crs(env) <- crs(canusa)
env[[6]] <- round(env[[6]],0)
env[[7]] <- round(env[[7]],0)
env[[8]] <- round(env[[8]],0)
env[[11]] <- round(env[[11]],0)
env[[12]] <- round(env[[12]],0)
env[[13]] <- round(env[[13]],0)
names(env) <- c("MAP","DD_0","PAS","CMD","DD18","effort","twi","tri","foot","lcov","silt","ph","orgC")

#make raster stack a matrix
envval <- getValues(env)
rm(clim,nonclim)

#generate random absences 
indx.abs <- sample(which(!is.na(envval[,1]) & !is.na(envval[,11])),10000)

#load lauras prediction functions
source("/Users/isaaceckert/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Laura Pollock, Dr. - Predictions5km/ModelDataCode/PredictFunctions.R")

#set vals
pth="/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Datasets/GBIF Border Species List/GBIF Download/SpeciesGriddedInput/"
indx.rast=env[[1]]
indx.abs=indx.abs
envir=envval
sufx=".gridpointsLCC.RDS"

#run models
model.brt.gam(species=species[179:184],pth=pth,indx.rast=indx.rast,indx.abs=indx.abs,envir)

#Function to predict to Canada + USA at 1km (might take a while!)
#Start by making 2 CURRENT girds to predict on (based on sample effort)

#Pred grids no sample effort
all.preds.no.effort <- envval[,c(1:5,7:8,10:13)] #effort and footprint removed

#grids sample effort fixed at max
all.preds.effort <- envval[,1:13]

#We can also add future climate data to make predictions!
setwd("/Users/isaaceckert/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Laura Pollock, Dr. - Predictions5km/ModelDataCode/")

#Lets use RCP 4.5
future.climate.preds<-stack(c("EnvUsCan_rcp45_2080_MAP.tif","EnvUsCan_rcp45_2080_DD_0.tif","EnvUsCan_rcp45_2080_PAS.tif","EnvUsCan_rcp45_2080_CMD.tif","EnvUsCan_rcp45_2080_DD18.tif")) #creates a stacked raster of the climate predictors
other.preds<-stack(c("EnvUsCan_effort.tif","EnvUsCan_twi.tif","EnvUsCan_tri.tif","EnvUsCan_foot.tif",
                     "EnvUsCan_lcov.tif","EnvUsCan_silt.tif","EnvUsCan_ph.tif","EnvUsCan_orgC.tif")) #creates a stacked raster of the non-climate predictors

#Stack all predictors
future.preds <- stack(future.climate.preds,other.preds)
remove(future.climate.preds,other.preds)

#Set CRS
crs(future.preds) <- crs(canusa)

#Round some layers to integers 
future.preds[[6]] <- round(future.preds[[6]],0)
future.preds[[7]] <- round(future.preds[[7]],0)
future.preds[[8]] <- round(future.preds[[8]],0)
future.preds[[11]] <- round(future.preds[[11]],0)
future.preds[[12]] <- round(future.preds[[12]],0)
future.preds[[13]] <- round(future.preds[[13]],0)

#Set names
names(future.preds) <- c("MAP","DD_0","PAS","CMD","DD18","effort","twi","tri","foot","lcov","silt","ph","orgC")

#Make raster stack into a matrix
future.envval <- getValues(future.preds)

#Future pred grids no sample effort
future.preds.no.effort <- future.envval[,c(1:5,7:8,10:13)] #effort and footprint removed

#Future pred grids sample effort fixed at max
future.preds.effort <- future.envval[,1:13]

#checks
dim(all.preds.no.effort)
dim(future.preds.no.effort)
dim(all.preds.effort)
dim(future.preds.effort)

colnames(all.preds.no.effort)==colnames(future.preds.no.effort) #looks good!

#only projectv to 1000km buffer
buffer<-vect("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Vectors/North America/Border Buffer/Border Shapefiles/Buffers/1000KM.Buffer.shp")
buffer<-project((buffer),rast(canusa))
buffer.rast<-terra::rasterize((buffer),rast(canusa))
plot(buffer.rast)

all.preds.no.effort<-all.preds.no.effort[-which(is.na(values(buffer.rast))),]
future.preds.no.effort<-future.preds.no.effort[-which(is.na(values(buffer.rast))),]
all.preds.effort<-all.preds.effort[-which(is.na(values(buffer.rast))),]
future.preds.effort<-future.preds.effort[-which(is.na(values(buffer.rast))),]

#function to make SDM predictions!
predict.SDMs<-function(species,future){
  #set species 
  s <- species
  
  #PREDICT CURRENT
  #predict BRT with no sample effort 
  BRT <- readRDS(file=paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Models/BRT/",s,".RDS"))
  if (class(BRT)=="NULL") { 
    next 
  } #skip if doesnt exist
  
  brt.pred <- predict.gbm(BRT, as.data.frame(all.preds.no.effort), n.trees=BRT$gbm.call$best.trees, type="response")
  print("Predict current BRT withOUT effort = DONE")
  
  # BRT WITH sample effort set to max
  BRTe <- readRDS(file=paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Models/BRTwEffort/",s,".RDS"))
  
  brte.pred <- predict.gbm(BRTe, as.data.frame(all.preds.effort),n.trees=BRTe$gbm.call$best.trees, type="response")
  print("Predict current BRT WITH effort = DONE")
  
  #PREDICT FUTURE
  if (future==T){
    
    brt.pred.future <- predict.gbm(BRT, as.data.frame(future.preds.no.effort), n.trees=BRT$gbm.call$best.trees, type="response")
    print("Predict future BRT withOUT effort = DONE")

    brte.pred.future <- predict.gbm(BRTe, as.data.frame(future.preds.effort),n.trees=BRTe$gbm.call$best.trees, type="response")
    print("Predict future BRT WITH effort = DONE")
    
    #combine
    lst <- cbind(brt.pred,brt.pred.future,brte.pred,brte.pred.future)
    colnames(lst)<-c("BRT.Current","BRT.Future","BRTe.Current","BRTe.Future")
    
    #round to percentages 
    lst.a <- apply(lst,2,function(x) round(x,2)*100)
    
    #save as RDS
    saveRDS(lst.a,file=paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Projections/",s,".1k",".RDS"))
    
    
  }
  
  #OR DONT
  else {
    #combine
    lst <- cbind(brt.pred,brte.pred)
    colnames(lst)<-c("BRT.Current","BRTe.Current")
    
    #round to percentages 
    lst.a <- apply(lst,2,function(x) round(x,2)*100)
    
    #save as RDS
    saveRDS(lst.a,file=paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Projections/",s,".1k",".RDS"))
    
  }
  
  rm(BRT,BRTe,lst,lst.a,s,brt.pred,brt.pred.future,brte.pred,brte.pred.future)
  

} #this takes a LONG time (predicting at 1k)

#redefine species
species.w.models<-gsub(".RDS","",list.files("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Models/BRT/"))

#loop it
for (i in 2:length(species.w.models)){
predict.SDMs(species.w.models[i],future=T)
print(paste("DONE:",i,"out of",length(species.w.models)))}

#load border line to add to plots
border<-vect("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Vectors/North America/Border Buffer/Border Shapefiles/Canada_and_US_Border/Canada_and_US_Border.shp")
border<-project(border,rast(canusa))

#needs the buffer and canusa loaded
plot.SDM<-function(species){
  #read in data
  data<-readRDS(paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Projections/",species,".1k",".rds"))
  class(data)
  
  #make NAs 0
  data[is.na(data)]<-0
  
  #convert back into probabilities (0-1)
  data<-round(apply(data,2,function (x) x/100),2)
  
  #make vec from raster base
  basePixelNu <- dim(canusa)[1]*dim(canusa)[2]
  vec <- 1:basePixelNu
  
  #set breaks and colours
  require(RColorBrewer)
  breakpoints <- seq(0,1,length.out=100)
  colors <- colorRampPalette(brewer.pal(9,'YlGn'))(100)
  colors<-c(colors)
  arg <- list(at=breakpoints, labels=round(breakpoints, 1))
  
  #plot it!
  par(mfrow=c(2,2))
  for (i in 1:ncol(data)){
    r <- canusa
    r[(which(values(canusa)>0))]<-200
    r[(which(values(buffer.rast)==1))] <- data[,i]
    r[is.na(canusa)] <- NA
    plot(r,breaks=c(breakpoints,1.1,201),col=c(colors,"lightgrey","lightgrey"),axes=F,box=F,main=colnames(data)[i],axis.args=arg,legend=F)
    plot(border,add=T)
  }
  
}
plot.SDM("Uta stansburiana")


plot.SDM.border<-function(species){
  species="Uta stansburiana"
  #read in data
  data<-readRDS(paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Projections/",species,".1k",".rds"))
  class(data)
  
  #make NAs 0
  data[is.na(data)]<-0
  
  #convert back into probabilities (0-1)
  data<-round(apply(data,2,function (x) x/100),2)
  
  #set breaks and colours
  require(RColorBrewer)
  breakpoints <- seq(0,1,length.out=100)
  colors <- colorRampPalette(brewer.pal(9,'YlGn'))(100)
  colors<-c(colors)
  arg <- list(at=breakpoints, labels=round(breakpoints, 1))
  
  #plot it!
  par(mfrow=c(2,2))
  for (i in 1:ncol(data)){
    r <- raster(buffer.rast)
    r[(which(values(buffer.rast)==1))] <- data[,i]
    r[(is.na(canusa))]<-NA
    r<-trim(r)
    plot(r,breaks=c(breakpoints,1.1,201),col=c("lightgrey",colors),axes=F,box=F,main=colnames(data)[i],axis.args=arg,legend=F)
    plot(border,add=T)
  }
  
}
plot.SDM.border("Uta stansburiana")



#clip projections by IUCN range 
#read in polygons
#birds from BoTW
st_layers("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Vectors/Drawn Polygons/Bird Polys/BOTW_2022_1/BOTW.gdb")
bird.polys <- sf::st_read(dsn = "/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Vectors/Drawn Polygons/Bird Polys/BOTW_2022_1/BOTW.gdb", layer = "All_Species")

#mammals from IUCn
mammals.polys<-read_sf("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Range Polys/IUCN/Downloaded/IUCN/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")

#amphibians from IUCN
amphibians.polys<-read_sf("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Range Polys/IUCN/Downloaded/IUCN/AMPHIBIANS/AMPHIBIANS.shp")

#reptiles from IUCN
reptiles.polys<-read_sf("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Range Polys/IUCN/Downloaded/IUCN/REPTILES/REPTILES.shp")

#filter to just breeding range polys
bird.polys.breeding<-bird.polys[which(bird.polys$seasonal%in%c(1,2)),]
remove(bird.polys)

#recast birds as MP
bird.polys.breeding<-st_cast(bird.polys.breeding, "MULTIPOLYGON")

#only keep polygons we have models for 
library(dplyr)
species=gsub(".1k.RDS","",list.files("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Projections/"))

bird.polys.breeding.keep<-bird.polys.breeding[which(bird.polys.breeding$sci_name%in%species),]
mammals.polys.keep<-mammals.polys[which(mammals.polys$binomial%in%species),]
amphibians.polys.keep<-amphibians.polys[which(amphibians.polys$binomial%in%species),]
reptiles.polys.keep<-reptiles.polys[which(reptiles.polys$binomial%in%species),]

#who's missing? 10 species!
missing<-species[-which(species%in%c(unique(mammals.polys.keep$binomial),unique(amphibians.polys.keep$binomial),unique(reptiles.polys.keep$binomial),unique(bird.polys.breeding.keep$sci_name)))]

missing.sp<-read.csv("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Datasets/GBIF Border Species List/GBIF Download/missing.species.csv",row.names = 1)

missing[which(missing%in%missing.sp$Syn)]

missing.sp$Species[which(missing.sp$Syn%in%missing)]

#add missing polygons
bird.polys.breeding.keep.missing<-bird.polys.breeding[which(bird.polys.breeding$sci_name%in%missing.sp$Species[which(missing.sp$Syn%in%missing)]),]
mammals.polys.keep.missing<-mammals.polys[which(mammals.polys$binomial%in%missing.sp$Species[which(missing.sp$Syn%in%missing)]),]
amphibians.polys.keep.missing<-amphibians.polys[which(amphibians.polys$binomial%in%missing.sp$Species[which(missing.sp$Syn%in%missing)]),]
reptiles.polys.keep.missing<-reptiles.polys[which(reptiles.polys$binomial%in%missing.sp$Species[which(missing.sp$Syn%in%missing)]),]

nrow(bird.polys.breeding.keep.missing)
nrow(mammals.polys.keep.missing) #0
nrow(amphibians.polys.keep.missing) #0
nrow(reptiles.polys.keep.missing)

#combine all IUCN polygons into a single dataframe
birds<-bird.polys.breeding.keep[,c(2,17:19)]
birds.missing<-bird.polys.breeding.keep.missing[,c(2,17:19)]
mammals<-mammals.polys.keep[,c(2,27:29)]
amphibians<-amphibians.polys.keep[,c(2,27:29)]
reptiles<-reptiles.polys.keep[,c(2,27:29)]
reptiles.missing<-reptiles.polys.keep.missing[,c(2,27:29)]

names(birds)=names(reptiles)
st_geometry(birds) <- "geometry"
names(birds.missing)=names(reptiles)
st_geometry(birds.missing) <- "geometry"


all.polys<-rbind(birds,birds.missing,mammals,amphibians,reptiles,reptiles.missing)
saveRDS(all.polys,"/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Datasets/GBIF Border Species List/GBIF Download/all.polys.RDS")
all.polys<-readRDS("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Datasets/GBIF Border Species List/GBIF Download/all.polys.RDS")


length(unique(all.polys$binomial)) #143!!!

#load rasters
canusa <- raster("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Rasters/North America/Layers for SDMs/USCanadaOutline.tif")
buffer<-vect("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Vectors/North America/Border Buffer/Border Shapefiles/Buffers/1000KM.Buffer.shp")
buffer<-project((buffer),rast(canusa))
buffer.rast<-terra::rasterize((buffer),rast(canusa))
plot(buffer.rast)

#clip models (only current distributions)
for (i in 1:length(species)){
  sp=species[i]
  
  if (sp %in% all.polys$binomial){
    
    #rasterize poly to buffer zone with projection
    sp.poly<-vect(all.polys[which(all.polys$binomial==sp),])
    sp.poly<-project(sp.poly,buffer.rast)
    
    #buffer
    sp.poly.buffer<-buffer(sp.poly,width=500000)
    #sp.poly.buffer<-sp.poly
    
    
    #rasterize
    poly.rast<-rasterize(sp.poly.buffer,buffer.rast)
    plot(poly.rast)
    
    #make some cells NA
    poly.rast[is.na(buffer.rast)]<-NA
    buffer.rast[(which(values(poly.rast)==1))]<-2

    vals=values(buffer.rast)
    vals<-vals[complete.cases(vals)]

    #load data
    data=readRDS(paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Projections/",sp,".1k",".rds"))
    
    #clipp
    data[which(vals==1),c(1,3)]<-0
      
    #save
    saveRDS(data,(paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Clipped Projections 500km/",sp,".1k",".rds")))
      
 
    }    
  else {
    #rasterize poly to buffer zone with projection
    syn=missing.sp$Species[which(missing.sp$Syn==sp)]
    sp%in%all.polys$binomial
    sp.poly<-vect(all.polys[which(all.polys$binomial==syn),])
    sp.poly<-project(sp.poly,buffer.rast)
    
    #buffer
    sp.poly.buffer<-buffer(sp.poly,width=500000)
    #sp.poly.buffer<-sp.poly
    
    #rasterize
    poly.rast<-rasterize(sp.poly.buffer,buffer.rast)
    
    #make some cells NA
    poly.rast[is.na(buffer.rast)]<-NA
    buffer.rast[(which(values(poly.rast)==1))]<-2
    
    vals=values(buffer.rast)
    vals<-vals[complete.cases(vals)]
    
    #load data
    data=readRDS(paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Projections/",sp,".1k",".rds"))
    
    #clipp
    data[which(vals==1),c(1,3)]<-0
    
    #save
    saveRDS(data,(paste0("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Species Distribution Models/Border Species/Clipped Projections 500km/",sp,".1k",".rds")))
    

    
  }  
    
  
  print(paste("DONE:",i,"out of",length(species)))
  }
  
  
  



