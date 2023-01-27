## trying out dispersal functions
require(devtools)
install_version("MigClim", version = "1.6.1", repos = "http://cran.us.r-project.org")
install_version("adehabitat", version = "1.8.2", repos = "http://cran.us.r-project.org")
install_version("SDMTools", version = "1.1", repos = "http://cran.us.r-project.org")
library(SDMTools)
library(MigClim)

?MigClim.migrate

## Arguments:

data(MigClim.testData)

### Run MigClim with a data frame type input.
n <- MigClim.migrate(iniDist = MigClim.testData[,1:3],
                    hsMap = MigClim.testData[,4:8],
                    rcThreshold = 500, 
                    envChgSteps = 5,
                    dispSteps = 5,
                    dispKernel = c(1.0,0.4,0.16,0.06,0.03),
                    barrier = MigClim.testData[,9],
                    barrierType = "strong", 
                    iniMatAge = 1, 
                    propaguleProd = c(0.01,0.08,0.5,0.92),
                    lddFreq = 0.1, 
                    lddMinDist = 6, 
                    lddMaxDist = 15, 
                    simulName = "MigClimTest", 
                    replicateNb = 1, 
                    overWrite = TRUE, 
                    testMode=FALSE, 
                    fullOutput=FALSE, 
                    keepTempFiles=FALSE)








## create name of folder with example data:
folder = "data-raw/migclim_example_data/MigClim_Example/"

## read in a raster file and look at it:
barrier1 <- raster(paste(folder, "barrier1.asc", sep = ""))
plot(barrier1)
barrier5 <- raster(paste(folder, "barrier5.asc", sep = ""))
plot(barrier5)

hsMap1 <- raster(paste(folder, "hsMap1.asc", sep = ""))
plot(hsMap1)
plot(hsMap1 > 445) ## visualize threshold 
hsMap5 <- raster(paste(folder, "hsMap5.asc", sep = ""))
plot(hsMap5)

## try running the function
MigClim.genClust(hsMap = paste(folder, "hsMap", sep = ""),
                 barrier = paste(folder, "barrier", sep = ""), 
                 nrClusters=1,
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

