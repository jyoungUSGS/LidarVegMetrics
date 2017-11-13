library(raster)
library(USGSlvm)
library(tools)
library(parallel)
library(rgdal)

# Global dbh in meters
# must be changed if input unit is not meters
dbh <<- 1.37

# Absolute path to working directory for output files
project_dir <- "C:/Users/nfkruska/Documents/data/SHEN/lidar_projects/SHEN_2011"

# Relative or absolute path to location of lidar files
lidar_dir <- "./HAG"

# Input CRS of lidar files found by EPSG code
# NAD83 / UTM zone 17N: 26917
# NAD83(2011) / Conus Albers: 6350
epsg_code <- 26917

# points under this value will not be considered as vegetation
veg_floor <- dbh
# points above this value will not be considered as vegetation
veg_ceiling <- 50
# output raster resolution(s). Can be integer or list of integers
output_res <- c(10, 25)
# boolean: have the input points been normalized?
# NOT CURRENTLY SUPPORTED
# POINTS MUST ALREADY BE NORMALIZED
hag_nrml <- T
# raster format for output
# "HFA": .img
# "GTiff": .tif
ras_fmt <- "GTiff"

# =============================================================================
setwd(project_dir)

lidar_files <- tools::list_files_with_exts(lidar_dir,
                                          c("LAS", "las", "LAZ", "laz"))
if (length(lidar_files) == 0){
  stop("No lidar files found in lidar directory.")
}

epsg <- make_EPSG()
epsg <- epsg[! is.na(epsg$code), ]
input_crs <- epsg$prj4[epsg$code == epsg_code]
if (length(input_crs) == 0){
  stop("EPSG code was not found.")
}

folders <- c("./tiles", "./mosaics")
names(folders) <- c("tiles", "mosaics")
dir.create(folders["tiles"])
dir.create(folders["mosaics"])
# =============================================================================

calc_metrics <- function(x, CRS, output_dir, resolution, nrml = T){

  format_tile_name <- function(x){
     tn <- basename(tools::file_path_sans_ext(x))
     np <- strsplit(tn, "_")
     tn <- paste0(unlist(np[[1]][1:5]), collapse = "_")
     tn <- paste(tn, resolution, sep = "_")
     return(tn)
   }
  tile_name <- format_tile_name(x)
  print(tile_name)

  save_output <- function(output_dir, folder, tile_name, product, file){
    output_path <- file.path(output_dir, folder, paste(tile_name, product,
      sep = "_"))
    if (file.exists(output_path)){
      warning(paste0("File ", output_path,
        " already exists. It will be overwritten."))
    }

    if (product %in% c("hden", "hcnt", "hpct")){
      raster::writeRaster(file, output_path, ras_fmt, overwrite = T,
        options = "COMPRESS = DEFLATE")
    } else {
      raster::writeRaster(file, output_path, ras_fmt, overwrite = T,
        bylayer = T, suffix = names(file), options = "COMPRESS = DEFLATE")
    }
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
     names(s) <- c("ccov", "cden")
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
    f_list <- tools::list_files_with_exts(p, c("img", "tif", "grd", "nc",
      "envi", "bil"))
    lc <- lapply(f_list, FUN = function(x){
      raster::nlayers(raster::stack(x))
    })
    max_lc <- max(unlist(lc))
    for (f in f_list){
      if (raster::nlayers(raster::stack(f)) < max_lc){
        print(f)
        tmp <- paste(f, "tmp.img", sep = "_")
        file.copy(f, tmp)
        file.remove(f)
        s <- raster::stack(tmp)
        while (raster::nlayers(s) < max_lc){
          r <- raster::raster(s)
          r <- raster::setValues(r, NA)
          s <- raster::addLayer(s, r)
        }
        raster::writeRaster(s, f, overwrite = T)
        file.remove(tmp)
      }
    }
  }
  f_list <- tools::list_files_with_exts(p, c("img", "tif", "grd", "nc",
    "envi", "bil", "tiff"))
  if (length(f_list) == 0){
    warning(paste0("No raster tiles found in ", p, ". Product failed."))
    return(FALSE)
  }
  s <- do.call(merge, c(lapply(f_list, function(x){
    raster::stack(x)
  }), tolerance = 10))
  np <- strsplit(basename(f_list[1]), "_")
  m_name <- paste0(unlist(np[[1]][c(1:3, 6:7)]), collapse = "_")
  output_path <- file.path(output_dir, m_name)
  if (file.exists(output_path)){
    warning(paste0("File ", output_path,
      " already exists. It will be overwritten."))
  }
  raster::writeRaster(s, output_path, format = ras_fmt, overwrite = T)
  return(TRUE)
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

  tile_time <- system.time(tiles <- clusterApplyLB(cl, lidar_files,
    calc_metrics, CRS = input_crs, output_dir = res_output, resolution = res))
  # mosaic_time <- system.time(clusterApplyLB(cl, products, create_mosaics,
  #   output_dir = folders["mosaics"]))
}
stopCluster(cl)
