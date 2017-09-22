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
create_main_structure <- function(){
  dir.create("./veg")
  dir.create("./mosaics")
}
create_main_structure()
output_dirs <- list.dirs(recursive = F)

calc_metrics <- function(x, CRS, output_dir, resolution, nrml){

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
  res_output <- file.path("./veg", paste(res, "m", sep = ""))
  create_res_output <- function(folder){
    dir.create(res_output)
    dir.create(file.path(res_output, "canopy"))
    dir.create(file.path(res_output, "height_pct"))
    dir.create(file.path(res_output, "height_cnt"))
    dir.create(file.path(res_output, "height_den"))
    dir.create(file.path(res_output, "stats"))
    dir.create(file.path(res_output, "ratios"))
  }
  create_res_output(res_output)

  system.time(clusterApplyLB(cl, lidar_files, calc_metrics,
    CRS = input_crs, output_dir = res_output,
    resolution = res, nrml = hag_nrml))

  products <- list.dirs(res_output)
  products <- products[-1]

  create_mosaic <- function(p){
    f_list <- tools::list_files_with_exts(p, "img")
    r <- raster::stack(f_list[1])
    for (f in f_list){
      t <- raster::stack(f)
      r <- raster::merge(r, t, tolerance = 5)
    }
  }
  system.time(clusterApplyLB(cl, products, create_mosaic))
}
stopCluster(cl)
