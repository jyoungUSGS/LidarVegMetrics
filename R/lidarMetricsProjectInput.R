library(raster)
library(USGSlvm)
library(tools)
library(parallel)

# Global variables
dbh <<- 1.37

# USER PARAMETERS
project_dir <- "C:/Users/nfkruska/Documents/data/Project1"
lidar_dir <- "./LAZ"
input_crs <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# points under this value will not be considered as vegetation
veg_floor <- dbh
# points above this value will not be considered as vegetation
veg_ceiling <- 50
# output raster resolution(s). Can be integer or list of integers
output_res <- 10
# boolean: have the input points been normalized?
# NOT CURRENTLY SUPPORTED
# points must be normalized before processing
hag_nrml <- T
# raster format for output: "HFA" = .img
ras_fmt <- "HFA"
# =============================================================================
setwd(project_dir)
lidar_files <- tools::list_files_with_exts(lidar_dir,
                                          c("LAS", "las", "LAZ", "laz"))

folders <- c("./tiles", "./mosaics")
names(folders) <- c("tiles", "mosaics")

create_main_structure <- function(){
  dir.create(folders["tiles"])
  dir.create(folders["mosaics"])
}
create_main_structure()

calc_metrics <- function(x, CRS, output_dir, resolution, nrml = T){

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
    raster::writeRaster(file, output_path, ras_fmt, overwrite = T)
  }

   # data load with CRS
   las_data <- USGSlvm::readLidarData(x, CRS)
   # at this time, expecting normalized points
   names(las_data)[1] <- "Z_agl"

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

   return(TRUE)
}

create_mosaics <- function(p, output_dir){
  if (grepl("height_den", p) | grepl("height_cnt", p)){
    f_list <- tools::list_files_with_exts(p, "img")
    lc <- lapply(f_list, FUN = function(x){
      raster::nlayers(raster::stack(x))
    })
    lc <- max(unlist(lc))
    for (f in f_list){
        if (raster::nlayers(raster::stack(f)) < lc){
          print(f)
          s <- raster::stack(f)
          while (raster::nlayers(s) < lc){
            r <- raster::raster(s)
            r <- raster::setValues(r, 0)
            s <- raster::addLayer(s, r)
          }
          raster::writeRaster(s, f, overwrite = T)
        }
    }
  }
  f_list <- tools::list_files_with_exts(p, "img")
  s <- raster::stack(f_list[1])
  for (f in f_list){
    t <- raster::stack(f)
    s <- raster::merge(s, t, tolerance = 5)
  }
  np <- strsplit(basename(f), "_")
  m_name <- paste0(unlist(np[[1]][c(1:3, 6:7)]), collapse = "_")
  output_path <- file.path(output_dir, m_name)
  raster::writeRaster(s, output_path, overwrite = T)
}

create_res_output <- function(f){
  dir.create(f)
  dir.create(file.path(f, "canopy"))
  dir.create(file.path(f, "height_pct"))
  dir.create(file.path(f, "height_cnt"))
  dir.create(file.path(f, "height_den"))
  dir.create(file.path(f, "stats"))
  dir.create(file.path(f, "ratios"))
  p <- list.dirs(f)[-1]
  return(p)
}

num_clus <- detectCores()
cl <- makeCluster(num_clus - 1)
clusterEvalQ(cl, {
  library(rlas); library(sp); library(raster);
  library(rgdal); library(data.table); library(gstat);
  library(USGSlvm); library(tools); library(parallel)
})
clusterExport(cl, c("veg_floor", "veg_ceiling", "ras_fmt", "folders"))

for (res in output_res){
  res_output <- file.path(folders["tiles"], paste(res, "m", sep = ""))
  products <- create_res_output(res_output)

  system.time(tiles <- parLapply(cl, lidar_files, calc_metrics,
    CRS = input_crs, output_dir = res_output, resolution = res))
  # system.time(parLapply(cl, products, create_mosaics,
  #   output_dir = folders["mosaics"]))
}
stopCluster(cl)
