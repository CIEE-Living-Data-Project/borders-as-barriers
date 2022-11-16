#Make SDMs for Lupine species in Canada and the USA
#Author: Isaac Eckert
#Date: Sept 9th, 2022

#data resolution = 1km
library(raster)
library(dismo)
library(gbm)
library(mgcv)
library(raster)
library(terra)
library(beepr)
library(RColorBrewer)

#set directory to files
setwd("/Users/isaaceckert/Library/CloudStorage/OneDrive-SharedLibraries-McGillUniversity/Laura Pollock, Dr. - Predictions5km/ModelDataCode/")

#Load in Data
#Raster Outline
canusa <- raster("USCanadaOutline.tif")
plot(canusa)
crs(canusa)

##Environmental Predictors
#creates a stacked raster of the climate predictors
climate.preds<-stack(c("EnvUsCan_MAP.tif",             # Mean Annual Precipitation (mm)
                       "EnvUsCan_DD_0.tif",            # degree-days below 0°C (chilling degree days)
                       "EnvUsCan_PAS.tif",             # precipitation as snow (mm)
                       "EnvUsCan_CMD.tif",             # Hargreave's climatic moisture index
                       "EnvUsCan_DD18.tif"))           # degree-days below 18°C

climate.preds

##Other predictors
#creates a stacked raster of the non-climate predictors
other.preds<-stack(c("EnvUsCan_effort.tif",            # sampling effort
                     "EnvUsCan_twi.tif",               # topographic wetness index
                     "EnvUsCan_tri.tif",               # topographic ruggedness index
                     "EnvUsCan_foot.tif",              # Human foot print
                     "EnvUsCan_lcov.tif",              # Land cover classes
                     "EnvUsCan_silt.tif",              # soil silt content
                     "EnvUsCan_ph.tif",                # soil Ph
                     "EnvUsCan_orgC.tif"))             # Organic Carbon
other.preds

#Stack all predictors
all.preds <- stack(climate.preds,other.preds)

#Set coordinate reference system (CRS)
crs(all.preds) <- crs(canusa)

#Round some layers to intergers 
all.preds[[6]] <- round(all.preds[[6]],0)
all.preds[[7]] <- round(all.preds[[7]],0)
all.preds[[8]] <- round(all.preds[[8]],0)
all.preds[[11]] <- round(all.preds[[11]],0)
all.preds[[12]] <- round(all.preds[[12]],0)
all.preds[[13]] <- round(all.preds[[13]],0)

#Set names
names(all.preds) <- c("MAP","DD_0","PAS","CMD","DD18","effort","twi","tri","foot","lcov","silt","ph","orgC")

#Make raster stack into a matrix
envval <- getValues(all.preds)
rm(climate.preds,other.preds) #keep things tidy

#Generate random absences (consistent across all species)
indx.abs <- sample(which(!is.na(envval[,1]) & !is.na(envval[,11])),8000) # sampling 8000 absence points

#Select species to model from list of all species
grep("Lupinus ",gsub(".gridpointsLCC.rds","",list.files("SpeciesData/")),value = T) 

#Function to make SDMs and save them (this does not project, just fits)
#Makes 3 models (GAM, BRT with effort, BRT without effort)
make.SDMs<-function(species){
  
  #set index
  indx.rast=all.preds[[1]]
  indx.abs=indx.abs
  
  #set species
  s <- species
  print(paste("Species=",s))
  
  #read in point data
  pts <- readRDS(file=paste0("SpeciesData/",s,".gridpointsLCC.rds"))
  
  #rows?
  print(paste("Number of cells with presences=",nrow(pts)))
  
  if(nrow(pts) <20) {
    print("WARNING: Too little points, are you sure bud?")
  }
  if(nrow(pts) >2000)  {pts <- pts[sample(nrow(pts),2000),]}
  
  cells <- cellFromXY(all.preds[[1]],pts)
  
  #remove any overlaps in pres and abs
  abs.sp <- indx.abs[!indx.abs %in% cells]
  
  #prep data for models
  indx <- c(cells,abs.sp)
  dat <- data.frame(envval[indx,])
  dat$occur <- c(rep(1,length(cells)),rep(0,length(abs.sp)))
  dat <- na.omit(dat)
  dat$lcov <- as.factor(dat$lcov)
  
  #run the GAM model using 4 knots (std)
  GAM <- gam(occur ~ s(MAP,k=4) + s(DD_0,k=4) + s(PAS,k=4) + s(CMD,k=4) + s(twi,k=4) + s(tri,k=4) + 
                s(effort,k=4) + s(silt,k=4) + s(ph,k=4) + s(orgC,k=4), select = TRUE, method = "GCV.Cp", 
                data=dat, family=binomial(link="logit"))
  print("GAM=DONE")
  #save the GAM
  saveRDS(GAM,file=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/GAMs/",s,".GAM",".rds"))
  
  #run the BRT with effort  
  BRTe <- gbm.step(data=dat,gbm.x = 1:13,gbm.y = 14,family = "bernoulli",tree.complexity = 4,
                   learning.rate = 0.008,bag.fraction = 0.6)
  print("BRTe=DONE")
  #save it
  saveRDS(BRTe,file=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/BRTs/",s,".BRTe",".rds"))
  
  #run the BRT withOUT effort  
  BRT <- gbm.step(data=dat,gbm.x = c(1:5,7:8,10:13),gbm.y = 14,family = "bernoulli",tree.complexity = 5,
                   learning.rate = 0.008,bag.fraction = 0.6)
  print("BRT=DONE")
  #save it
  saveRDS(BRT,file=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/BRTs/",s,".BRT",".rds"))
  beep(2)
}
make.SDMs("Lupinus perennis") #make models only for this species

#Function to predict to Canada + USA at 1km (might take a while!)
#Start by making 2 CURRENT girds to predict on (based on sample effort)

#Pred grids no sample effort
all.preds.no.effort <- envval[,c(1:5,7:8,10:13)] #effort and footprint removed

#grids sample effort fixed at max
all.preds.effort <- envval[,1:13]

#We can also add future climate data to make predictions!
#Lets use RCP 4.5 for 2080
future.climate.preds<-stack(c("EnvUsCan_rcp45_2080_MAP.tif","EnvUsCan_rcp45_2080_DD_0.tif","EnvUsCan_rcp45_2080_PAS.tif","EnvUsCan_rcp45_2080_CMD.tif","EnvUsCan_rcp45_2080_DD18.tif")) #creates a stacked raster of the climate predictors
other.preds<-stack(c("EnvUsCan_effort.tif","EnvUsCan_twi.tif","EnvUsCan_tri.tif","EnvUsCan_foot.tif",
                     "EnvUsCan_lcov.tif","EnvUsCan_silt.tif","EnvUsCan_ph.tif","EnvUsCan_orgC.tif")) #creates a stacked raster of the non-climate predictors

#Stack all predictors
future.preds <- stack(future.climate.preds,other.preds)
remove(future.climate.preds,other.preds)
#Set CRS
crs(future.preds) <- crs(canusa)

#Round some layers to intergers 
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

#function to make SDM predictions!
predict.SDMs<-function(species,future){
  
 #set species 
  s <- species
  
  #PREDICT CURRENT
  #predict BRT with no sample effort 
  BRT <- readRDS(file=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/BRTs/",s,".BRT",".rds"))
  if (class(BRT)=="NULL") { 
    next 
  } #skip if doesnt exist
  
  brt.pred <- predict.gbm(BRT, as.data.frame(all.preds.no.effort), n.trees=BRT$gbm.call$best.trees, type="response")
  print("Predict current BRT withOUT effort = DONE")
  
  # BRT WITH sample effort set to max
  BRTe <- readRDS(file=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/BRTs/",s,".BRTe",".rds"))

  brte.pred <- predict.gbm(BRTe, as.data.frame(all.preds.effort),n.trees=BRTe$gbm.call$best.trees, type="response")
  print("Predict current BRT WITH effort = DONE")
  
  #GAM WITH sample effort set to max
  GAM <- readRDS(file=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/GAMs/",s,".GAM",".rds"))
  gam.pred <- predict.gam(GAM,as.data.frame(all.preds.effort),type="response")
  print("Predict current GAM WITH effort = DONE")
  
  #PREDICT FUTURE
  if (future==T){
    brt.pred.future <- predict.gbm(BRT, as.data.frame(future.preds.no.effort), n.trees=BRT$gbm.call$best.trees, type="response")
    brte.pred.future <- predict.gbm(BRTe, as.data.frame(future.preds.effort),n.trees=BRTe$gbm.call$best.trees, type="response")
    gam.pred.future <- predict.gam(GAM,as.data.frame(future.preds.effort),type="response")
   
    #combine
    lst <- cbind(brt.pred,brt.pred.future,brte.pred,brte.pred.future,gam.pred,gam.pred.future)
    colnames(lst)<-c("BRT.Current","BRT.Future","BRTe.Current","BRTe.Future","GAM.Current","GAM.Future")
    
    #round to percentages 
    lst.a <- apply(lst,2,function(x) round(x,2)*100)
    
    #save as RDS
    saveRDS(lst.a,file=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/Predictions/",s,".1k",".rds"))
    
     
  }
  
  #OR DONT
  else {
    #combine
    lst <- cbind(brt.pred,brte.pred,gam.pred)
    colnames(lst)<-c("BRT.Current","BRTe.Current","GAM.Current")
    
    #round to percentages 
    lst.a <- apply(lst,2,function(x) round(x,2)*100)
    
    #save as RDS
    saveRDS(lst.a,file=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/Predictions",s,".1k",".rds"))
    
  }

  rm(BRT,BRTe,GAM,lst,lst.a,s,brt.pred,brt.pred.future,brte.pred,brte.pred.future,gam.pred,gam.pred.future)

beep(2)

} #this takes a LONG time (predicting at 1k)
predict.SDMs("Lupinus perennis",future=T)
  
#plot predictions 
plot.SDM<-function(species){

  #read in data
  data<-readRDS(paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/",species,".1k",".rds"))
  class(data)
  
  #make NAs 0
  data[is.na(data)]<-0
  
  #convert back into probabilities (0-1)
  data<-round(apply(data,2,function (x) x/100),2)
  
  #make vec from raster base
  basePixelNu <- dim(canusa)[1]*dim(canusa)[2]
  vec <- 1:basePixelNu
  
  #add data
  mat<-matrix(NA, nrow = length(vec), ncol = ncol(data))
  rownames(mat)<-vec
  mat[row.names(data),]<-data[]
  colnames(mat)<-colnames(data)
  
  #set breaks and colours
  require(RColorBrewer)
  breakpoints <- seq(0,1,length.out=100)
  colors <- colorRampPalette(brewer.pal(9,'YlGn'))(100)
  colors<-c("lightgrey",colors)
  arg <- list(at=breakpoints, labels=round(breakpoints, 1))
  
  #plot it!
  par(mfrow=c(2,3))
  for (i in 1:ncol(mat)){
  r <- canusa
  r[] <- mat[,i]
  r[is.na(canusa)] <- NA
  plot(r,breaks=c(breakpoints),col=c(colors),axes=F,box=F,main=colnames(mat)[i],axis.args=arg,legend=F)
  
  
  }
  
}
plot.SDM("Lupinus perennis")

#plot change between current and future
plot.change<-function(species,threshold){

    #read in data
  data<-readRDS(paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/Predictions/",species,".1k",".rds"))

  #make NAs 0
  data[is.na(data)]<-0
  
  #threshold to make maps nicer
  data[(data<threshold)]<-0
  
  
  #convert back into probabilities (0-1)
  #data<-round(apply(data,2,function (x) x/100),2)
  
  colnames(data)
  
  #calc change metric
  #order = "BRT.Current","BRT.Future","BRTe.Current","BRTe.Future","GAM.Current","GAM.Future"
  change<-matrix(data=NA,ncol=3,nrow=nrow(data))
  change[,1]=data[,2]-data[,1]
  change[,2]=data[,4]-data[,3]
  change[,3]=data[,6]-data[,5]
  colnames(change)=c("BRT.Change","BRTe.Change","GAM.Change")
  rownames(change)=rownames(data)
  
  #calc mx and mn
  mx.1<-max(change[,1],na.rm=T)
  mx.2<-max(change[,2],na.rm=T)
  mx.3<-max(change[,3],na.rm=T)
  
  maxes<-c(mx.1,mx.2,mx.3)
  
  mn.1<-min(change[,1],na.rm=T)
  mn.2<-min(change[,2],na.rm=T)
  mn.3<-min(change[,3],na.rm=T)
  
  mins<-c(mn.1,mn.2,mn.3)
  
  
  #add back in NAs but make them 2
  change[which(data[,1]==0 & data[,2]==0),1]<-200
  change[which(data[,3]==0 & data[,4]==0),2]<-200
  change[which(data[,5]==0 & data[,6]==0),3]<-200
  
  #make vec from raster base
  basePixelNu <- dim(canusa)[1]*dim(canusa)[2]
  vec <- 1:basePixelNu
  
  #add data
  mat<-matrix(NA, nrow = length(vec), ncol = ncol(change))
  rownames(mat)<-vec
  mat[row.names(change),]<-change[]
  colnames(mat)<-colnames(change)
  
  #plot it!
  par(mfrow=c(1,3))
  for (i in 1:ncol(mat)){

    #set breaks and colours
    require(RColorBrewer)
    breakpoints <- seq(mins[i],maxes[i],length.out=100)
    breakpoints <-c(breakpoints,199,200)
    colors <- colorRampPalette(brewer.pal(9,'RdYlGn'))(100)
    colors<-c(colors,"#CDCDCD")
    arg <- list(at=breakpoints, labels=round(breakpoints, 1))
    
    r <- canusa
    r[] <- mat[,i]
    r[is.na(canusa)] <- NA
    plot(r,breaks=c(breakpoints),col=c(colors),axes=F,box=F,main=colnames(mat)[i],axis.args=arg,legend=F)
    
  }
  
}
plot.change("Lupinus perennis",30)

#lets look at some model results
par(mfrow=c(1,1))
s="Lupinus perennis"
BRT <- readRDS(file=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/BRTs/",s,".BRT",".rds"))
BRTe <- readRDS(file=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/BRTs/",s,".BRTe",".rds"))

#relative importance of different variables
summary(BRT) #this tells you how important each variable was
summary(BRTe) #this tells you how important each variable was

#individual variable plots
gbm.plot(BRT, n.plots=11, plot.layout=c(4, 3), write.title = F)
gbm.plot(BRTe, n.plots=11, plot.layout=c(4, 3), write.title = F)


#save geotiff file
write.raster<-function(species){
  species="Lupinus perennis"
  
  #read in data
  data<-readRDS(paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/Predictions/",species,".1k",".rds"))
  class(data)
  
  #make NAs 0
  data[is.na(data)]<-0
  
  #convert back into probabilities (0-1)
  data<-round(apply(data,2,function (x) x/100),2)
  
for (i in 1:ncol(data)){
  i=1
  r <- (canusa)
  r[] <- data[,i]
  r[is.na(canusa)] <- NA
  r[(r==0)]<-NA
  
  rast(r)
  
  writeRaster(rast(r),filename=paste0("C:/Users/andre/OneDrive - McGill University/Borders_WG/SDM Tutorial/Lupine SDMs/Models/Projections/",species,".",colnames(data)[i],".1k",".tiff"),overwrite=T)
  
}
  
}





