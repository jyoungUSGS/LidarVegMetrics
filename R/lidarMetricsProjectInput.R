library(raster)
library(USGSlvm)
library(tools)
library(parallel)

# Global variables
dbh <<- 1.37

# USER PARAMETERS
project_dir <- "C:/Temp/CONUS/Project1"
lidar_dir <- "D:/CDI2017/Lidar_collects/SHEN/NRCS_RockinghamCnty_2012/HAG/UNBuffered"
input_crs <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# points under this value will not be considered as vegetation
veg_floor <- dbh
# points above this value will not be considered as vegetation
veg_ceiling <- 50
# output raster resolution(s). Can be integer or list of integers
output_res <- c(10, 25)
# boolean: have the input points been normalized?
hag_nrml <- F
# raster format for output: "HFA" = .img
ras_fmt <- "HFA"
# =============================================================================
setwd(project_dir)
lidar_files <- tools::list_files_with_exts(lidar_dir,
                                          c("LAS", "las", "LAZ", "laz"))

create_veg_structure <- function(){
  dir.create("./Veg")
  dir.create("./stacks")

  dir.create("./Veg/10m")
  dir.create("./Veg/10m/canopy")
  dir.create("./Veg/10m/height_pct")
  dir.create("./Veg/10m/height_cnt")
  dir.create("./Veg/10m/height_den")
  dir.create("./Veg/10m/stats")
  dir.create("./Veg/10m/ratios")

  dir.create("./Veg/25m")
  dir.create("./Veg/25m/canopy")
  dir.create("./Veg/25m/height_pct")
  dir.create("./Veg/25m/height_cnt")
  dir.create("./Veg/25m/height_den")
  dir.create("./Veg/25m/stats")
  dir.create("./Veg/25m/ratios")
}
create_veg_structure()
output_dirs <- list.dirs("./Veg", recursive = F)

calc_metrics <- function(x, CRS, output_dir, resolution, nrml = F){

  format_tile_name <- function(x){
     tn <- basename(tools::file_path_sans_ext(x))
     np <- strsplit(tn, "_")
     tn <- paste0(unlist(np[[1]][1:5]), collapse = "_")
     tn <- paste(tn, resolution, sep = "_")
     return(tn)
   }
  tile_name <- format_tile_name(x)

  save_output <- function(output_dir, folder, tile_name, product, file){
    output_path <- file.path(output_dir, folder, paste(tile_name, product,
      sep = "_"))
    raster::writeRaster(file, output_path, ras_fmt)
  }

   # data load with CRS
   las_data <- USGSlvm::readLidarData(x, CRS)

   if (nrml){
     names(las_data)[1] <- "Z_agl"
   } else {
     las_data <- USGSlvm::normalizeByDEM(las_data, resolution)
   }

   # if data does not fall within range, class as noise
   las_data$Classification[las_data$Classification == 1 &
                          veg_floor > las_data$Z_agl |
                          las_data$Z_agl > veg_ceiling] <- 7

   # classify the points by height above ground
   las_data <- USGSlvm::classifyByHeight(las_data)

   # metrics functions
   stats <- USGSlvm::calcPointStatistics(las_data, resolution)
   save_output(output_dir, "stats", tile_name, "stat", stats)

   vdr <- USGSlvm::calcVertDistRatio(las_data, resolution)
   save_output(output_dir, "ratios", tile_name, "vdrs", vdr)

   create_canopy_stack <- function(x, res){
     ccov <- USGSlvm::calcCanopyCover(x, res)
     cdens <- USGSlvm::calcCanopyDensity(x, res)
     s <- raster::stack(ccov, cdens)
     names(s) <- c("ccov", "cdens")
     return(s)
   }
   cnpy <- create_canopy_stack(las_data, resolution)
   save_output(output_dir, "canopy", tile_name, "cnpy", cnpy)

   hpct <- USGSlvm::calcHeightPercentiles(las_data, resolution)
   save_output(output_dir, "height_pct", tile_name, "hpct", hpct)

   hcnt <- USGSlvm::calcHeightPointCounts(las_data, resolution)
   save_output(output_dir, "height_cnt", tile_name, "hcnt", hcnt)

   hdens <- USGSlvm::calcHeightPointPercents(hcnt, resolution)
   save_output(output_dir, "height_den", tile_name, "hden", hdens)
}

num_clus <- detectCores()
cl <- makeCluster(num_clus - 1)
clusterEvalQ(cl, {
  library(rlas); library(sp); library(raster);
  library(rgdal); library(data.table); library(gstat);
  library(USGSlvm); library(tools); library(parallel)
})
clusterExport(cl, c("veg_floor", "veg_ceiling", "ras_fmt"))

for (res in output_res){
  system.time(clusterApplyLB(cl, lidar_files, calc_metrics,
    CRS = input_crs, output_dir = output_dirs[1],
    resolution = res, nrml = hag_nrml))
}
stopCluster(cl)
