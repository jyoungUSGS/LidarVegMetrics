library(rlas)
library(sp)
library(raster)
library(rgdal)
library(data.table)
library(gstat)
library(USGSlvm)
library(tools)
library(parallel)

setwd("C:/Temp/CONUS")

# retreive from config file?
outputRes <- 100
# will need to get this from metadata in project folder.
# CRS VA State Plane North
inputCRS <- "+proj=lcc +lat_1=39.2 +lat_2=38.03333333333333 +lat_0=37.66666666666666 +lon_0=-78.5 +x_0=3500000.0001016 +y_0=2000000.0001016 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

# initialize cluster
numClus <- detectCores()
cl <- makeCluster(numClus - 1)
clPackage <- clusterEvalQ(cl, {library(rlas); library(sp); library(raster);
                       library(rgdal); library(data.table); library(gstat);
                       library(USGSlvm); library(tools); library(parallel)})

clVars <- clusterExport(cl, c("lidarFiles", "outputRes", "inputCRS"))


projects <- list.dirs(, recursive = F)
for (p in projects){
  print(p)

  resPath <- list.dirs(p, recursive = F)
  print(resPath)

  lidarFiles <- tools::list_files_with_exts(resolutions[1], c("LAS", "las", "LAZ", "laz"))
  system.time(output10 <- parLapplyLB(cl, lidarFiles, lidarMetrics, resolution = 10, outputDir = resPath[2]))
  system.time(output25 <- parLapplyLB(cl, lidarFiles, lidarMetrics, resolution = 25, outputDir = resPath[3]))
  print(lidarFiles)
}



lidarMetrics <- function(x, CRS, resolution = 30, outputDir) {
  LasData <- readLidarData(x, inputCRS)
  dem <- createDEM(LasData, resolution)
  # dsm <- createDSM(LasData, resolution)
  # LasData <- normalizeByDEM(LasData, dem)
  # LasData <- classifyByHeight(LasData)
  # lasStatLayers <- calcPointStatistics(LasData, resolution)
  # percentileLayers <- calcHeightPercentiles(LasData, resolution)
  # vdRatioLayers <- calcVertDistRatio(LasData, resolution)
  # canCover <- calcCanopyCover(LasData, resolution)
  # canDensity <- calcCanopyDensity(LasData, resolution)
  # pointCountLayers <- calcHeightPointCounts(LasData, resolution)
  # pointPercentLayers <- calcHeightPointPercents(pointCountLayers, resolution)
  tileName <- basename(tools::file_path_sans_ext(x))
  outputName <- paste(tileName, resolution, "dem.tif", sep = "_")
  outputFile <- file.path(outputDir, "dem", outputName)

  writeRaster(dem, outputFile)
}


system.time(output <- parLapply(cl, lidarFiles, tileMetrics, inputCRS, outputRes))

parallel::stopCluster(cl)
