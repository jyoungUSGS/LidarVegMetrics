library(rlas)
library(sp)
library(raster)
library(rgdal)
library(data.table)
library(gstat)
library(USGSlvm)
library(tools)
library(parallel)

setwd("C:/Temp/CONUS/project1/laz")

# Add option for config file to set all parameters
#     Config file holds:
#           CRS
#           output resolution(s?)
#           custom names?
# will need to get this from metadata in project folder.

lidarMetrics <- function(x, CRS, outputDir, resolution = 25){

  # get file name
  tileName <- basename(tools::file_path_sans_ext(x))
  nameParts <- strsplit(tileName, "_")
  tileName <- paste0(unlist(nameParts[[1]][-6]), collapse = "_")
  tileName <- paste(tileName, resolution, sep = "_")

  # data load and prep
  lasData <- readLidarData(x, inputCRS)
  # Input z values are already normalized, rename Z to reflect
  names(lasData)[1] <- "Z_agl"
  lasData <- classifyByHeight(lasData)

  # metrics functions
  lasStatLayers <- calcPointStatistics(lasData, outputDir, tileName, resolution)
  percentileLayers <- calcHeightPercentiles(lasData, outputDir, tileName, resolution)
  vdRatioLayers <- calcVertDistRatio(lasData, outputDir, tileName, resolution)
  canCover <- calcCanopyCover(lasData, outputDir, tileName, resolution)
  canDensity <- calcCanopyDensity(lasData, outputDir, tileName, resolution)
  pointCountLayers <- calcHeightPointCounts(lasData, outputDir, tileName, resolution)
  pointPercentLayers <- calcHeightPointPercents(pointCountLayers, outputDir, tileName, resolution)
}


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

  clVars <- clusterExport(cl, c("lidarFiles", "inputCRS", "resPath"))
  system.time(output10 <- parLapplyLB(cl, lidarFiles, lidarMetrics, CRS = inputCRS, outputDir = resPath[2], resolution = 10))
  system.time(output25 <- parLapplyLB(cl, lidarFiles, lidarMetrics, CRS = inputCRS, outputDir = resPath[3], resolution = 25))
}








parallel::stopCluster(cl)
