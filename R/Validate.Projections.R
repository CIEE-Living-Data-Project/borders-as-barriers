#plot projection and choose best clipping distance 
library(terra)
library(raster)

#gridded base
canusa <- raster("Data/Base Layers/USCanadaOutline.tif")

#only projectv to 1000km buffer
buffer<-vect("Data/Base Layers/1000KM.Buffer.shp")
buffer<-project((buffer),rast(canusa))
buffer.rast<-terra::rasterize((buffer),rast(canusa))

#load border line to add to plots
border<-vect("Data/Base Layers/Canada_and_US_Border.shp")
border<-project(border,rast(canusa))

#species list
missing.sp<-read.csv("Data/Species Names/missing.species.csv",row.names = 1)

#polygons
all.polys<-readRDS("Data/Species Polygons/all.polys.RDS")

#plotting function with clip option
plot.projection<-function(species,clip.distance){
  species="Marmota olympus"
  
  data<-readRDS(paste0("Data/Projections/",species,".1k",".rds"))

  #make NAs 0
  data[is.na(data)]<-0
  
  #convert back into probabilities (0-1)
  data<-round(apply(data,2,function (x) x/100),2)
  
  #set par
  par(mfrow=c(2,1))
  
  #plot unclipped
  r.unclipped <- raster(buffer.rast)
  r.unclipped[(which(values(buffer.rast)==1))] <- data[,"BRT.Current"]
  r.unclipped[(is.na(canusa))]<-NA
  r.unclipped<-trim(r.unclipped)
  plot(r.unclipped,axes=F,box=F,axis.args=arg,legend=F,main="UNCLIPPED")
  plot(border,add=T,lwd=0.5)
  
  #plot clipped
  if (species %in% all.polys$binomial){
    print(paste(species,"NAME IN POLYGONS"))
    
    #rasterize poly to buffer zone with projection
    sp.poly<-vect(all.polys[which(all.polys$binomial==species),])
    sp.poly<-project(sp.poly,buffer.rast)
    
    #buffer
    sp.poly.buffer<-buffer(sp.poly,width=clip.distance*1000)

    #rasterize
    poly.rast<-rasterize(sp.poly.buffer,buffer.rast)
    
    #make some cells NA
    poly.rast[is.na(buffer.rast)]<-NA
    buffer.rast.int<-buffer.rast
    buffer.rast.int[(which(values(poly.rast)==1))]<-2
    
    vals=values(buffer.rast.int)
    vals<-vals[complete.cases(vals)]
    
    #load data
    data.clip<-data
    #clipp
    data.clip[which(vals==1),c(1,3)]<-0
    #plot clipped
    r.clipped <- raster(buffer.rast)
    r.clipped[(which(values(buffer.rast)==1))] <- data.clip[,"BRT.Current"]
    r.clipped[(is.na(canusa))]<-NA
    r.clipped<-trim(r.clipped)
    plot(r.clipped,axes=F,box=F,axis.args=arg,legend=F,main="CLIPPED")
    plot(border,add=T,lwd=0.5)
    
  }    
  else {    print(paste(sp,"NAME NOT IN POLYGONS"))
    
    #rasterize poly to buffer zone with projection
    syn=missing.sp$Species[which(missing.sp$Syn==species)]
    sp.poly<-vect(all.polys[which(all.polys$binomial==syn),])
    sp.poly<-project(sp.poly,buffer.rast)
    
    #buffer
    sp.poly.buffer<-buffer(sp.poly,width=clip.distance*1000)

    #rasterize
    poly.rast<-rasterize(sp.poly.buffer,buffer.rast)
    
    #make some cells NA
    poly.rast[is.na(buffer.rast)]<-NA
    buffer.rast.int<-buffer.rast
    buffer.rast.int[(which(values(poly.rast)==1))]<-2
    
    vals=values(buffer.rast.int)
    vals<-vals[complete.cases(vals)]
    
    #load data
    data.clip<-data
    
    #clipp
    data.clip[which(vals==1),c(1,3)]<-0
  
    #plot clipped
    r.clipped <- raster(buffer.rast)
    r.clipped[(which(values(buffer.rast)==1))] <- data.clip[,"BRT.Current"]
    r.clipped[(is.na(canusa))]<-NA
    r.clipped<-trim(r.clipped)
    plot(r.clipped,axes=F,box=F,axis.args=arg,legend=F,main="CLIPPED")
    plot(border,add=T,lwd=0.5)
      
  }  
  
  
  
  }
plot.projection("Marmota olympus",100)

#save clip of choice to new folder
save.clip<-function(species,clip.distance,folder.path){

  data<-readRDS(paste0("Data/Projections/",species,".1k",".rds"))
  
  #make NAs 0
  data[is.na(data)]<-0
  
  #convert back into probabilities (0-1)
  data<-round(apply(data,2,function (x) x/100),2)
  
  
  #plot clipped
  if (species %in% all.polys$binomial){

    #rasterize poly to buffer zone with projection
    sp.poly<-vect(all.polys[which(all.polys$binomial==species),])
    sp.poly<-project(sp.poly,buffer.rast)
    
    #buffer
    sp.poly.buffer<-buffer(sp.poly,width=clip.distance*1000)
    
    #rasterize
    poly.rast<-rasterize(sp.poly.buffer,buffer.rast)
    
    #make some cells NA
    poly.rast[is.na(buffer.rast)]<-NA
    buffer.rast.int<-buffer.rast
    buffer.rast.int[(which(values(poly.rast)==1))]<-2
    
    vals=values(buffer.rast.int)
    vals<-vals[complete.cases(vals)]
    
    #load data
    data.clip<-data
    #clipp
    data.clip[which(vals==1),c(1,3)]<-0
    saveRDS(data.clip,paste0(folder.path,species,".clipped.at.",as.character(clip.distance),"km.RDS"))


    
  }    
  else {
    
    #rasterize poly to buffer zone with projection
    syn=missing.sp$Species[which(missing.sp$Syn==species)]
    sp.poly<-vect(all.polys[which(all.polys$binomial==syn),])
    sp.poly<-project(sp.poly,buffer.rast)
    
    #buffer
    sp.poly.buffer<-buffer(sp.poly,width=clip.distance*1000)
    
    #rasterize
    poly.rast<-rasterize(sp.poly.buffer,buffer.rast)
    
    #make some cells NA
    poly.rast[is.na(buffer.rast)]<-NA
    buffer.rast.int<-buffer.rast
    buffer.rast.int[(which(values(poly.rast)==1))]<-2
    
    vals=values(buffer.rast.int)
    vals<-vals[complete.cases(vals)]
    
    #load data
    data.clip<-data
    
    #clipp
    data.clip[which(vals==1),c(1,3)]<-0

    saveRDS(data.clip,file=paste0(file.path,species,".clipped.at.",clip.distance,"km.RDS"))
    
  }  
  
  
  
  
  
  
  
  
}
save.clip("Marmota olympus",100,"/Users/isaaceckert/Desktop/Clipped Models/")



