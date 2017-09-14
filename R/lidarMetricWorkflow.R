library(rlas)
library(sp)
library(raster)
library(rgdal)
library(data.table)
library(gstat)
library(USGSlvm)
library(tools)
library(parallel)



# Add option for config file to set all parameters
#     Config file holds:
#           CRS
#           output resolution(s?)
#           custom names?
# will need to get this from metadata in project folder.




setwd("C:/Temp/CONUS")
# CRS VA State Plane North
# inputCRS <- "+proj=lcc +lat_1=39.2 +lat_2=38.03333333333333 +lat_0=37.66666666666666 +lon_0=-78.5 +x_0=3500000.0001016 +y_0=2000000.0001016 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

# CRS UTM 17 N
inputCRS <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

numClus <- detectCores()
cl <- makeCluster(numClus - 1)
clPackage <- clusterEvalQ(cl, {library(rlas); library(sp); library(raster);
                       library(rgdal); library(data.table); library(gstat);
                       library(USGSlvm); library(tools); library(parallel)})

# get project files
projects <- list.dirs(, recursive = F)
for (p in projects){
  print(p)

# get output resolution folders
  resPath <- list.dirs(p, recursive = F)
  print(resPath)

  lidarFiles <- tools::list_files_with_exts(resPath[1], c("LAS", "las", "LAZ", "laz"))
  print(lidarFiles)

  clVars <- clusterExport(cl, c("lidarFiles", "inputCRS"))
  system.time(output10 <- parLapplyLB(cl, lidarFiles, lidarMetrics, resolution = 10, outputDir = resPath[2]))
  system.time(output25 <- parLapplyLB(cl, lidarFiles, lidarMetrics, resolution = 25, outputDir = resPath[3]))
}


# initialize cluster





lidarMetrics <- function(x, CRS, outputDir, resolution = 25){
  LasData <- readLidarData(x, inputCRS)
  # to account for pre-normalized lidar files
  LasData$Z_agl <- LasData$Z
  LasData <- classifyByHeight(LasData)
  lasStatLayers <- calcPointStatistics(LasData, resolution)
  percentileLayers <- calcHeightPercentiles(LasData, resolution)
  vdRatioLayers <- calcVertDistRatio(LasData, resolution)
  canCover <- calcCanopyCover(LasData, resolution)
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
