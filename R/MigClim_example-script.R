## trying out dispersal functions
## devtools::install_github("cran/MigClim")
library(MigClim)

?MigClim.genClust

## Arguments:
# hsMap
#  - name of raster file that contains suitability maps for each time step in ASCII grid 
#  - habitat suitability indicates the suitability of each cell to be colonized as a value between 0-1000
# barrier
#  - name of the raster file that contains barriers for each time step in ASCII grid 
#  - barrier files indicate whether there is a barrier to migration present, either 1 or 0
# nrClusters
#  - number of genetic clusters to use 
# nrIterations
#  - the number of iterations (ex. time steps)
# threshold
#  - number above which a cell is considered suitable 
# outFile
#  - name of output file
# initFile
#  - name of inpit file

## create name of folder with example data:
folder = "data-raw/migclim_example_data/MigClim_Example/"

## read in a raster file and look at it:
barrier1 <- raster(paste(folder, "barrier1.asc", sep = ""))
plot(barrier1)
barrier5 <- raster(paste(folder, "barrier5.asc", sep = ""))
plot(barrier5)

hsMap1 <- raster(paste(folder, "hsMap1.asc", sep = ""))
plot(hsMap1)
hsMap5 <- raster(paste(folder, "hsMap5.asc", sep = ""))
plot(hsMap5)

## try running the function
MigClim.genClust(hsMap = paste(folder, "hsMap", sep = ""),
                 barrier = paste(folder, "barrier", sep = ""), 
                 nrClusters=4,
                 nrIterations=5, 
                 threshold=445, 
                 outFile = paste(folder, "out", sep = ""), initFile="")


## read output script and look at at:
out1 <- raster(paste(folder, "out1.asc", sep = ""))
plot(out1)

out2 <- raster(paste(folder, "out2.asc", sep = ""))
plot(out2)

out5 <- raster(paste(folder, "out5.asc", sep = ""))
plot(out5)

