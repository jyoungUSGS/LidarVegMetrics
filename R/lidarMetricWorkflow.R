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
# ALBERS EPSG 6350 FOR FINAL SCRIPT
inputCRS <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
dbh <<- 1.37
vegetationFloor <- dbh

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
  # vdRatioLayers <- calcVertDistRatio(lasData, outputDir, tileName, resolution)
  canCover <- calcCanopyCover(lasData, outputDir, tileName, resolution)
  canDensity <- calcCanopyDensity(lasData, outputDir, tileName, resolution)
  pointCountLayers <- calcHeightPointCounts(lasData, outputDir, tileName, resolution)
  pointPercentLayers <- calcHeightPointPercents(pointCountLayers, outputDir, tileName, resolution)
}


numClus <- detectCores()
cl <- makeCluster(numClus - 1)
clPackage <- clusterEvalQ(cl, {
                          library(rlas); library(sp); library(raster);
                          library(rgdal); library(data.table); library(gstat);
                          library(USGSlvm); library(tools); library(parallel)
                          })

# get project files
projects <- list.dirs(, recursive = F)
for (p in projects){
  print(p)

# get output resolution folders
  resPath <- list.dirs(p, recursive = F, full.names = T)
  print(resPath)

  lidarFiles <- tools::list_files_with_exts(resPath[1], c("LAS", "las", "LAZ", "laz"))
  print(lidarFiles)
  clVars <- clusterExport(cl, c("lidarFiles", "inputCRS", "resPath"))
  system.time(output10 <- parLapplyLB(cl, lidarFiles, lidarMetrics, CRS = inputCRS, outputDir = resPath[2], resolution = 10))
  system.time(output25 <- parLapplyLB(cl, lidarFiles, lidarMetrics, CRS = inputCRS, outputDir = resPath[3], resolution = 25))
}
parallel::stopCluster(cl)
