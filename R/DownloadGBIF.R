#Download GBIF data for species not in Canada 
library(rgbif)
library(dplyr)
library(purrr)
library(readr)  
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(taxize) # for get_gbifid_
library(data.table)
library(raster, lib.loc = "/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library")
library(sp)
library(Matrix)
library(readr)

#species list
sp.list<-read.csv("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Datasets/GBIF Border Species List/species.list.csv") #from IUCN

setwd("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Datasets/GBIF Border Species List/GBIF Download/")

# fill in your gbif.org credentials 
user <- "laura.pollock" # your gbif.org username 
pwd <- "Voswic-2hikza-bucjug" # your gbif.org password
email <- "laura.pollock@mcgill.ca" # your email 


# match the names 
gbif_taxon_keys <- 
  sp.list[,1] %>% # use fewer names if you want to just test 
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(file = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  pull(usagekey) # get the gbif taxonkeys

#Canada download
occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred("country", "CA"),
  pred_gte("year", 1970),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email)

#USA download
occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred("country", "US"),
  pred_gte("year", 1990),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email)

#check status
occ_download_wait('0170827-220831081235567') #canada
occ_download_wait('0170829-220831081235567') #usa

#import downloads
gbif_ca <- fread("0170827-220831081235567.csv", na.strings = c("", NA),select=c("species","collectionCode","coordinateUncertaintyInMeters",
                                                                                        "individualCount","year","decimalLatitude","decimalLongitude","issue","month"))


gbif_us <- fread("0170829-220831081235567.csv", na.strings = c("", NA),select=c("species","collectionCode","coordinateUncertaintyInMeters",
                                                                                        "individualCount","year","decimalLatitude","decimalLongitude","issue","month"))
#start witn birds and only keep observations between months 6 and 7
gbif.birds<-rbind(gbif_ca[which(gbif_ca$species%in%sp.list$Species[which(sp.list$Class=="Birds")] & gbif_ca$month%in%c(6,7)),],
                  gbif_us[which(gbif_us$species%in%sp.list$Species[which(sp.list$Class=="Birds")] & gbif_us$month%in%c(6,7)),])

#reomve birds
gbif.no.birds<-rbind(gbif_ca[-which(gbif_ca$species%in%sp.list$Species[which(sp.list$Class=="Birds")]),],
                  gbif_us[-which(gbif_us$species%in%sp.list$Species[which(sp.list$Class=="Birds")]),])

dim(gbif.birds) #5542900
dim(gbif.no.birds) #398498

#make spatial
coordinates(gbif.birds)<-~decimalLongitude+decimalLatitude
crs(gbif.birds) <- "+proj=longlat +ellps=WGS84"
gbif.birds.lcc <- spTransform(gbif.birds,CRS("+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m 
                                     +no_defs"))

coordinates(gbif.no.birds)<-~decimalLongitude+decimalLatitude
crs(gbif.no.birds) <- "+proj=longlat +ellps=WGS84"
gbif.no.birds.lcc <- spTransform(gbif.no.birds,CRS("+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m 
                                     +no_defs"))

length(unique(gbif.birds$species))+length(unique(gbif.no.birds$species)) #176 species 
length(sp.list$Species) #228

write.csv(sp.list[-which(sp.list$Species%in%c(gbif.birds$species,gbif.no.birds$species)),],"missing.species.csv")

missing.sp<-read.csv("missing.species.csv")

missing.sp<-read.csv("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Datasets/GBIF Border Species List/GBIF Download/missing.species.csv",row.names = 1)

# rematch the names 
gbif_taxon_keys.missing <- 
  missing.sp[,3] %>% # use fewer names if you want to just test 
  taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
  bind_rows() %T>% # combine all data.frames into one
  readr::write_tsv(file = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
  pull(usagekey) # get the gbif taxonkeys

#redownload
#Canada download
occ_download(
  pred_in("taxonKey", gbif_taxon_keys.missing),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred("country", "CA"),
  pred_gte("year", 1970),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email)

#USA download
occ_download(
  pred_in("taxonKey", gbif_taxon_keys.missing),
  pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred("country", "US"),
  pred_gte("year", 1990),
  format = "SIMPLE_CSV",
  user=user,pwd=pwd,email=email)

occ_download_wait('0171084-220831081235567')
occ_download_wait('0171088-220831081235567')


#import downloads
gbif_ca.missing <- fread("0171084-220831081235567.csv", na.strings = c("", NA),select=c("species","collectionCode","coordinateUncertaintyInMeters",
                                                                                "individualCount","year","decimalLatitude","decimalLongitude","issue","month"))


gbif_us.missing <- fread("0171088-220831081235567.csv", na.strings = c("", NA),select=c("species","collectionCode","coordinateUncertaintyInMeters",
                                                                                "individualCount","year","decimalLatitude","decimalLongitude","issue","month"))
#start witn birds and only keep observations between months 6 and 7
gbif.birds.missing<-rbind(gbif_ca.missing[which(gbif_ca.missing$species%in%missing.sp$Syn[which(missing.sp$Class=="Birds")] & gbif_ca.missing$month%in%c(6,7)),],
                  gbif_us.missing[which(gbif_us.missing$species%in%missing.sp$Syn[which(missing.sp$Class=="Birds")] & gbif_us.missing$month%in%c(6,7)),])

#reomve birds
gbif.no.birds.missing<-rbind(gbif_ca.missing[-which(gbif_ca.missing$species%in%missing.sp$Syn[which(missing.sp$Class=="Birds")]),],
                     gbif_us.missing[-which(gbif_us.missing$species%in%missing.sp$Syn[which(missing.sp$Class=="Birds")]),])

dim(gbif.birds.missing) #1571372
dim(gbif.no.birds.missing) #5750

#make spatial
coordinates(gbif.birds.missing)<-~decimalLongitude+decimalLatitude
crs(gbif.birds.missing) <- "+proj=longlat +ellps=WGS84"
gbif.birds.missing.lcc <- spTransform(gbif.birds.missing,CRS("+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m 
                                     +no_defs"))

coordinates(gbif.no.birds.missing)<-~decimalLongitude+decimalLatitude
crs(gbif.no.birds.missing) <- "+proj=longlat +ellps=WGS84"
gbif.no.birds.missing.lcc <- spTransform(gbif.no.birds.missing,CRS("+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m 
                                     +no_defs"))

#combine
#save all coords for sampling effort variable 
saveRDS(rbind(coordinates(gbif.birds),coordinates(gbif.birds.missing),coordinates(gbif.no.birds),coordinates(gbif.no.birds.missing)),file="latlongsforALLpresences.RDS")

#check species
length(unique(c(gbif.birds$species,gbif.birds.missing$species,gbif.no.birds$species,gbif.no.birds.missing$species))) #196
length(sp.list$Species) #228


#grid data
#prep
urbanBuilt <- raster("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Rasters/North America/Layers for SDMs/WaterUrbanBuiltMask.tif")

land <- urbanBuilt
land[land>=0] <- 1
urban <- urbanBuilt
urban[urban==1] <- 0
urban[urban==2] <- NA

modis.2 <- raster("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Rasters/North America/Layers for SDMs/modis_type2_LCC.tif")
mask <- modis.2
mask[mask==0]<-NA
mask[mask>0]<-0
mask[is.na(urban)]<-NA


setwd("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Datasets/GBIF Border Species List/GBIF Download/SpeciesGriddedInput/")


#combine all and grid
birds.lcc<-rbind(gbif.birds.lcc,gbif.birds.missing.lcc)
dim(birds.lcc)

no.birds.lcc<-rbind(gbif.no.birds.lcc,gbif.no.birds.missing.lcc)
dim(no.birds.lcc)

#remove all obs with a coord uncertainty of more than 5000m
birds.lcc.percise<-birds.lcc[which(birds.lcc$coordinateUncertaintyInMeters<5000),]
no.birds.lcc.percise<-no.birds.lcc[which(no.birds.lcc$coordinateUncertaintyInMeters<5000),]

#combine
all<-rbind(birds.lcc.percise,no.birds.lcc.percise)

#sp list
species<-unique(all$species)
length(species)

library(terra)
#make xy locations of all species gridded and removing urban areas
for (i in 1:length(species)) {

  s <- species[i]
  dat <- all[which(all$species==s),]
  dim(dat)
  pts.grd <- terra::rasterize(coordinates(dat),rast(urban),values=1,fun="length")

  # remove rasterized points from urban areas
  pts.grd[is.na(rast(urban))]=NA
  pts.grd[is.na(rast(modis.2))]=NA
  
  #need ONE sighting to be considered present
  bin <- pts.grd[[1]]
  bin[bin<1] <- NA
  bin[bin>=1] <- 1
  
  bin.pts.t=terra::as.points(bin)

  saveRDS(terra::crds(bin.pts.t),file=paste(s,"gridpointsLCC.RDS",sep="."))
  print(paste("DONE:",i,"out of",length(species)))
  rm(s,dat,pts.grd,bin,bin.pts.t)
}

#clip all points outside of breeding IUCN range for birds ONLY
library(sf)
library(sp)
library(terra)

#read in polygons
#birds from BoTW
st_layers("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Vectors/Drawn Polygons/Bird Polys/BOTW_2022_1/BOTW.gdb")
bird.polys <- sf::st_read(dsn = "/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Vectors/Drawn Polygons/Bird Polys/BOTW_2022_1/BOTW.gdb", layer = "All_Species")
bird.polys.breeding<-bird.polys[which(bird.polys$seasonal%in%c(1,2)),]
bird.polys.breeding<-st_cast(bird.polys.breeding, "MULTIPOLYGON")

class(birds.lcc.percise)

#sp list GBIF names (not all IUCN)
birds<-as.data.frame(unique(birds.lcc.percise$species))
names(birds)="GBIF"
nrow(birds)

#add IUCN name col
birds$IUCN=birds$GBIF
for (i in 1:length(birds$IUCN[which(birds$GBIF%in%missing.sp$Syn)])){

  birds$IUCN[which(birds$GBIF==birds$IUCN[which(birds$GBIF%in%missing.sp$Syn)][i])]<-missing.sp$Species[which(missing.sp$Syn==birds$IUCN[which(birds$GBIF%in%missing.sp$Syn)][i])]
  
  
}

#check
length(which(birds$IUCN%in%bird.polys.breeding$sci_name))

#make xy locations of all species gridded and removing urban areas
#clip bird obs by breeding and resident area polygons (IUCN)
for (i in 67:nrow(birds)) {

  s <- birds$GBIF[i]
  dat <- birds.lcc.percise[which(birds.lcc.percise$species==s),]
  dim(dat)
  pts.grd <- terra::rasterize(coordinates(dat),rast(urban),values=1,fun="length")

  #set raster cells outside IUCN to 0
  poly<-bird.polys.breeding[which(bird.polys.breeding$sci_name==birds$IUCN[i]),]
  poly<-st_transform(poly, crs(pts.grd))
  poly.vect<-vect(poly)
  #poly.vect<-simplifyGeom(poly.vect,tolerance=5)
  #poly.vect<-terra::buffer(poly,width=500000)
  
  
  #rasterize poly
  poly.rast<-terra::rasterize(poly.vect,rast(urban))
  plot(poly.rast)

  # remove rasterized points from urban areas
  pts.grd[is.na(rast(urban))]=NA
  pts.grd[is.na(rast(modis.2))]=NA
  pts.grd[is.na(poly.rast)]=NA
  
  #need ONE sighting to be considered present
  bin <- pts.grd[[1]]
  bin[bin<1] <- NA
  bin[bin>=1] <- 1
  
  bin.pts.t=terra::as.points(bin)
  plot(bin.pts.t,add=T)
  
  #save with GBIF name
  saveRDS(terra::crds(bin.pts.t),file=paste(s,"gridpointsLCC.RDS",sep="."))
  print(paste("DONE:",i,"out of",nrow(birds)))
  rm(s,dat,pts.grd,bin,bin.pts.t)
}


birds$GBIF[c(14,49,50,60,61,66)] #issue birds




