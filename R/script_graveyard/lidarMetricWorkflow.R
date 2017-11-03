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

numClus <- detectCores()
cl <- makeCluster(numClus - 1)
clPackage <- clusterEvalQ(cl, {
                          library(rlas); library(sp); library(raster);
                          library(rgdal); library(data.table); library(gstat);
                          library(USGSlvm); library(tools); library(parallel)
                          })

# ALBERS EPSG 6350 FOR FINAL SCRIPT
inputCRS <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
dbh <<- 1.37
vegetationFloor <- dbh

lidarMetrics <- function(x, CRS, outputDir, resolution = 25){

  format_tile_name <- function(x){
    tn <- basename(tools::file_path_sans_ext(x))
    np <- strsplit(tn, "_")
    tn <- paste0(unlist(np[[1]][-6]), collapse = "_")
    tn <- paste(tn, resolution, sep = "_")
    return(tn)
  }
  tileName <- format_tile_name(x)

  # data load and prep
  lasData <- USGSlvm::readLidarData(x, CRS)
  # Input z values are already normalized, rename Z to reflect
  names(lasData)[1] <- "Z_agl"
  lasData <- USGSlvm::classifyByHeight(lasData)

  # metrics functions
  lasStatLayers <- USGSlvm::calcPointStatistics(lasData, resolution)
  prod <- "hmin"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(lasStatLayers[1], outputFile)
  prod <- "hmax"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(lasStatLayers[2], outputFile)
  prod <- "havg"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(lasStatLayers[3], outputFile)
  prod <- "hstd"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
  raster::writeRaster(lasStatLayers[4], outputFile)
  prod <- "hske"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(lasStatLayers[5], outputFile)
  prod <- "hkur"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(lasStatLayers[6], outputFile)
  prod <- "hqav"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(lasStatLayers[7], outputFile)

  percentileLayers <- USGSlvm::calcHeightPercentiles(lasData, resolution)
  prod <- "hpct"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(percentileLayers, outputFile)

  vdRatioLayers <- USGSlvm::calcVertDistRatio(lasData, resolution)
  prod <- "vdr98"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(vdRatioLayers[1], outputFile)
  prod <- "vdr100"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(vdRatioLayers[1], outputFile)

  canCover <- USGSlvm::calcCanopyCover(lasData, outputDir, tileName, resolution)
  prod <- "ccov"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(canCover, outputFile)

  canDensity <- USGSlvm::calcCanopyDensity(lasData, resolution)
  prod <- "cdens"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(canDensity, outputFile)

  pointCountLayers <- USGSlvm::calcHeightPointCounts(lasData, resolution)
  prod <- "hcnt"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(pointCountLayers, outputFile)

  pointPercentLayers <- USGSlvm::calcHeightPointPercents(pointCountLayers,
                                                         resolution)
  prod <- "hdens"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod, ".tif",
                                                 sep = ""), sep = "_"))
  raster::writeRaster(pointPercentLayers, outputFile)

}

# get project files
projects <- list.dirs(, recursive = F)
for (p in projects){
  print(p)

# get output resolution folders
  resPath <- list.dirs(p, recursive = F, full.names = T)
  print(resPath)

  lidarFiles <- tools::list_files_with_exts(resPath[1], c("LAS", "las", "LAZ",
                                                          "laz"))
  print(lidarFiles)

  clusterExport(cl, c("lidarFiles", "inputCRS", "resPath"))
  system.time(output10 <- parLapplyLB(cl, lidarFiles, lidarMetrics,
                                      CRS = inputCRS, outputDir = resPath[2],
                                      resolution = 10))
  system.time(output25 <- parLapplyLB(cl, lidarFiles, lidarMetrics,
                                      CRS = inputCRS, outputDir = resPath[3],
                                      resolution = 25))
}
parallel::stopCluster(cl)
