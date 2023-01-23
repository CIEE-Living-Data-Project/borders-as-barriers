#get species list from IUCN polygons
library(sf)
library(sp)
library(terra)
library(raster, lib.loc = "/Library/Frameworks/R.framework/Versions/4.1-arm64/Resources/library")

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

#read in border buffer
buffer.1000<-read_sf("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Vectors/North America/Border Buffer/Border Shapefiles/Buffers/1000KM.Buffer.shp")
buffer.500<-read_sf("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Spatial Data/Vectors/North America/Border Buffer/Border Shapefiles/Buffers/500KM.Buffer.shp")

buffer.1000<-st_transform(buffer.1000,st_crs(mammals.polys))
buffer.500<-st_transform(buffer.500,st_crs(bird.polys))

#filter to just breeding range polys
bird.polys.breeding<-bird.polys[which(bird.polys$seasonal%in%c(1,2)),]
remove(bird.polys)

#recast birds as MP
bird.polys.breeding<-st_cast(bird.polys.breeding, "MULTIPOLYGON")

#find which polygons intersect using terra
amphibians.in.buffer<-amphibians.polys[relate(vect(amphibians.polys), vect(buffer.500), "intersects"),]
reptiles.in.buffer<-reptiles.polys[relate(vect(reptiles.polys), vect(buffer.500), "intersects"),]
mammals.in.buffer<-mammals.polys[relate(vect(mammals.polys), vect(buffer.500), "intersects"),]
birds.in.buffer<-bird.polys.breeding[relate(vect(bird.polys.breeding), vect(buffer.500), "intersects"),]
remove(bird.polys.breeding)

#remove species already in Canada
amphibians.in.buffer.US<-amphibians.in.buffer[-which(amphibians.in.buffer$binomial%in%gsub(".tiff","",list.files("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Zonation/SDM Inputs/Verts/BCOR/"))),]
reptiles.in.buffer.US<-reptiles.in.buffer[-which(reptiles.in.buffer$binomial%in%gsub(".tiff","",list.files("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Zonation/SDM Inputs/Verts/BCOR/"))),]
mammals.in.buffer.US<-mammals.in.buffer[-which(mammals.in.buffer$binomial%in%gsub(".tiff","",list.files("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Zonation/SDM Inputs/Verts/BCOR/"))),]
birds.in.buffer.US<-birds.in.buffer[-which(birds.in.buffer$sci_name%in%gsub(".tiff","",list.files("/Users/isaaceckert/Library/CloudStorage/OneDrive-McGillUniversity/Zonation/SDM Inputs/Verts/BCOR/"))),]


species.list=data.frame("Species"=c(amphibians.in.buffer.US$binomial,
                                    reptiles.in.buffer.US$binomial,
                                    mammals.in.buffer.US$binomial,
                                    birds.in.buffer.US$sci_name),
                        "Class"=c(rep("Amphibians",length(amphibians.in.buffer.US$binomial)),
                                  rep("Reptiles",length(reptiles.in.buffer.US$binomial)),
                                  rep("Mammals",length(mammals.in.buffer.US$binomial)),
                                  rep("Birds",length(birds.in.buffer.US$sci_name))))


write.csv(species.list[-which(duplicated(species.list)),],file = "/Users/isaaceckert/Desktop/species.list.csv")



