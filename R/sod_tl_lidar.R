library(raster)
library(USGSlvm)
library(tools)
library(parallel)

# USER PARAMETERS
# location of PROJECT
setwd("D:/nfkruska")
setwd("P:/geospatial/Research/rkmeente/SOD/CAL_OR_border/Baselayers/Elevation")
# CRS of lidar files
# EPSG: 2992
inputCRS <- "+proj=lcc +lat_1=43 +lat_2=45.5 +lat_0=41.75 +lon_0=-120.5 +x_0=399999.9999984 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048 +no_defs"

# normalized points under this value in native units
# of lidar files will not be considered as vegetation
dbh <<- 1.37
vegetationFloor <- dbh
# normalized points above this value in native units
# of lidar files will not be considered as vegetation
heightCeiling <- 200

# =============================================================================

create_project_structure <- function(){
  dir.create("./Veg")

  dir.create("./Veg/10m")
  dir.create("./Veg/10m/canopy")
  dir.create("./Veg/10m/height")
  dir.create("./Veg/10m/stats")
  dir.create("./Veg/10m/ratios")

  dir.create("./Veg/25m")
  dir.create("./Veg/25m/canopy")
  dir.create("./Veg/25m/height")
  dir.create("./Veg/25m/stats")
  dir.create("./Veg/25m/ratios")
}
create_project_structure()

lidarFiles <- tools::list_files_with_exts("./LAZ",
                                          c("LAS", "las", "LAZ", "laz"))

outputDirs <- list.dirs("./Veg", recursive = F)

lidarMetrics <- function(x, CRS, outputDir, resolution, nrml = F){

  format_tile_name <- function(x){
     tn <- basename(tools::file_path_sans_ext(x))
     np <- strsplit(tn, "_")
     tn <- paste0(unlist(np[[1]][-6]), collapse = "_")
     tn <- paste(tn, resolution, sep = "_")
     return(tn)
   }
  tileName <- format_tile_name(x)

  save_output <- function(ouputDir, folder, tileName, product, file){
    outputPath <- file.path(outputDir, folder, paste(tileName, paste(product,
      ".tif", sep = ""), sep = "_"))
    raster::writeRaster(file, outputPath)
  }

   # data load with CRS
   lasData <- USGSlvm::readLidarData(x, CRS)

   if (nrml){
     names(lasData)[1] <- "Z_agl"
   } else {
     lasData <- USGSlvm::normalizeByDEM(lasData, resolution)
   }

   # if data does not fall within range, class as noise
   lasData$Classification[lasData$Classification == 1 &
                          vegetationFloor > lasData$Z_agl |
                          lasData$Z_agl > heightCeiling] <- 7

   # classify the points by height above ground
   lasData <- USGSlvm::classifyByHeight(lasData)

  #  # metrics functions
   lasStatLayers <- USGSlvm::calcPointStatistics(lasData, resolution)
   save_output(outputDir, "stats", tileName, "hmin", lasStatLayers$hmin)
   save_output(outputDir, "stats", tileName, "hmax", lasStatLayers$hmax)
   save_output(outputDir, "stats", tileName, "havg", lasStatLayers$havg)
   save_output(outputDir, "stats", tileName, "hstd", lasStatLayers$hstd)
   save_output(outputDir, "stats", tileName, "hske", lasStatLayers$hske)
   save_output(outputDir, "stats", tileName, "hkur", lasStatLayers$hkur)
   save_output(outputDir, "stats", tileName, "hqav", lasStatLayers$hqav)

   vdr <- USGSlvm::calcVertDistRatio(lasData, resolution)
   save_output(outputDir, "ratios", tileName, "vdr98", vdr$vdr98)
   save_output(outputDir, "ratios", tileName, "vdr100", vdr$vdr100)

   ccov <- USGSlvm::calcCanopyCover(lasData, resolution)
   save_output(outputDir, "canopy", tileName, "ccov", ccov)

   cdens <- USGSlvm::calcCanopyDensity(lasData, resolution)
   save_output(outputDir, "canopy", tileName, "cdens", cdens)

   hpct <- USGSlvm::calcHeightPercentiles(lasData, resolution)
   save_output(outputDir, "height", tileName, "hpct", hpct)

   hcnt <- USGSlvm::calcHeightPointCounts(lasData, resolution)
   save_output(outputDir, "height", tileName, "hcnt", hcnt)

   hdens <- USGSlvm::calcHeightPointPercents(hcnt, resolution)
   save_output(outputDir, "height", tileName, "hdens", hdens)
}

numClus <- detectCores()
cl <- makeCluster(numClus - 1)
clPackage <- clusterEvalQ(cl, {
                          library(rlas); library(sp); library(raster);
                          library(rgdal); library(data.table); library(USGSlvm);
                          library(tools); library(parallel)
                          })
clusterExport(cl, c("vegetationFloor", "heightCeiling"))

system.time(trash <- parLapplyLB(cl, lidarFiles, lidarMetrics,
                                    CRS = inputCRS, outputDir = outputDirs[1],
                                    resolution = 10, nrml = F))
system.time(trash <- parLapplyLB(cl, lidarFiles, lidarMetrics,
                                    CRS = inputCRS, outputDir = outputDirs[2],
                                    resolution = 25, nrml = T))

rm(trash)
stopCluster(cl)
