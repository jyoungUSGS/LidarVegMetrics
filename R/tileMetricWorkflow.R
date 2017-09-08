library(rlas)
library(sp)
library(raster)
library(rgdal)
library(data.table)
library(gstat)
library(USGSlvm)
library(tools)
library(parallel)

inputDir <- "C:/Temp"
setwd(inputDir)
lidarFiles <- tools::list_files_with_exts(getwd(), c("LAS", "las", "LAZ", "laz"))

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


tileMetrics <- function(x, CRS, resolution = 30) {
  LasData <- readLidarData(lidarFiles[2], inputCRS)
  dem <- createDEM(LasData, resolution)
  dsm <- createDSM(LasData, resolution)
  LasData <- normalizeByDEM(LasData, dem)
  LasData <- classifyByHeight(LasData)
  lasStatLayers <- calcPointStatistics(LasData, resolution)
  percentileLayers <- calcHeightPercentiles(LasData, resolution)
  vdRatioLayers <- calcVertDistRatio(LasData, resolution)
  canCover <- calcCanopyCover(LasData, resolution)
  canDensity <- calcCanopyDensity(LasData, resolution)
  pointCountLayers <- calcHeightPointCounts(LasData, resolution)
  pointPercentLayers <- calcHeightPointPercents(pointCountLayers, resolution)

  tileName <- basename(tools::file_path_sans_ext(lidarFiles[2]))

  writeRaster(dem, paste0(tileName, "_dem.tif"))
  writeRaster(dsm, paste0(tileName, "_dsm.tif"))
}


output <- parLapply(cl, lidarFiles, tileMetrics, inputCRS, outputRes)
