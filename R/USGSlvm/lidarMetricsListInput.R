library(rlas)
library(sp)
library(raster)
library(rgdal)
library(data.table)
library(gstat)
library(USGSlvm)
library(tools)
library(parallel)


# USER PARAMETERS
# location of lidar files
setwd("P:/geospatial/Research/rkmeente/SOD/CAL_OR_border/Baselayers/Elevation")

# CRS of lidar files
inputCRS <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# directory for output (ALL rasters)
outputDir <- "./metrics"
# normalized points under this value in native units
# of lidar files will not be considered as vegetation
dbh <<- 1.37
vegetationFloor <- dbh
# normalized points above this value in native units
#  of lidar files will not be considered as vegetation
heightCeiling <- 200

# =============================================================================

lidarFiles <- tools::list_files_with_exts(basename("./Lidar"), c("LAS", "las",
                                                                 "LAZ", "laz"))

lidarMetrics <- function(x, CRS, outputDir, resolution){

   format_tile_name <- function(x){
     tn <- basename(tools::file_path_sans_ext(x))
     np <- strsplit(tn, "_")
     tn <- paste0(unlist(np[[1]][-6]), collapse = "_")
     tn <- paste(tn, resolution, sep = "_")
     return(tn)
   }
   tileName <- format_tile_name(x)

   # data load with CRS
   lasData <- USGSlvm::readLidarData(x, CRS)

   # Input z values are already normalized, rename Z to reflect OR...
  #  names(lasData)[1] <- "Z_agl"
   # normalize elevation values
   lasData <- normalizeByDEM(lasData, resolution = resolution)

   # if data does not fall within range, class as noise
   lasData$Classification[vegetationFloor > lasData$Z_agl & lasData$Z_agl > heightCeiling] <- 7

   # classify the points by height above ground
   lasData <- USGSlvm::classifyByHeight(lasData)

   # metrics functions
   lasStatLayers <- USGSlvm::calcPointStatistics(lasData, resolution)
   prod <- "hmin"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(lasStatLayers[1], outputFile)
   prod <- "hmax"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(lasStatLayers[2], outputFile)
   prod <- "havg"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(lasStatLayers[3], outputFile)
   prod <- "hstd"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                   sep = ""), sep = "_"))
   raster::writeRaster(lasStatLayers[4], outputFile)
   prod <- "hske"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(lasStatLayers[5], outputFile)
   prod <- "hkur"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(lasStatLayers[6], outputFile)
   prod <- "hqav"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(lasStatLayers[7], outputFile)

   percentileLayers <- USGSlvm::calcHeightPercentiles(lasData, resolution)
   prod <- "hpct"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(percentileLayers, outputFile)

   vdRatioLayers <- USGSlvm::calcVertDistRatio(lasData, resolution)
   prod <- "vdr98"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(vdRatioLayers[1], outputFile)
   prod <- "vdr100"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(vdRatioLayers[1], outputFile)

   canCover <- USGSlvm::calcCanopyCover(lasData, resolution)
   prod <- "ccov"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(canCover, outputFile)

   canDensity <- USGSlvm::calcCanopyDensity(lasData, resolution)
   prod <- "cdens"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(canDensity, outputFile)

   pointCountLayers <- USGSlvm::calcHeightPointCounts(lasData, resolution)
   prod <- "hcnt"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(pointCountLayers, outputFile)

   pointPercentLayers <- USGSlvm::calcHeightPointPercents(pointCountLayers,
                                                          resolution)
   prod <- "hdens"
   outputFile <- file.path(outputDir, paste(tileName, paste(prod, ".tif",
                                                  sep = ""), sep = "_"))
   raster::writeRaster(pointPercentLayers, outputFile)
}


numClus <- detectCores()
cl <- makeCluster(numClus - 1)
clPackage <- clusterEvalQ(cl, {
                          library(rlas); library(sp); library(raster);
                          library(rgdal); library(data.table); library(gstat);
                          library(USGSlvm); library(tools); library(parallel)
                          })
clusterExport(cl, c("vegetationFloor", "heightCeiling"))

system.time(output25 <- parLapplyLB(cl, lidarFiles, lidarMetrics,
                                    CRS = inputCRS, outputDir = outputDir,
                                    resolution = 25))
