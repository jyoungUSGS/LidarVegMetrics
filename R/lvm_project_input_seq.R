library(raster)
library(USGSlvm)
library(tools)
library(parallel)
library(rgdal)

# Global dbh in meters
# must be changed if input unit is not meters
dbh <<- 1.37

# Absolute path to working directory for output files
project_dir <- "C:/Users/nfkruska/Documents/data/SHEN/va_shen_2015"

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
# dir.create(folders["mosaics"])
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
  las_data$Classification[las_data$Classification == 1 & veg_floor >
                          las_data$Z_agl | las_data$Z_agl > veg_ceiling] <- 7
  
  # classify the points by height above ground
  las_data <- USGSlvm::classifyByHeight(las_data)
  
  # metrics functions
  stats <- USGSlvm::calcPointStatistics(las_data, resolution)
  save_output(output_dir, "stats", tile_name, "stat", stats)
  rm(stats)
  
  vdr <- USGSlvm::calcVertDistRatio(las_data, resolution)
  save_output(output_dir, "ratios", tile_name, "vdrs", vdr)
  rm(vdr)
  
  create_canopy_stack <- function(x, res){
    ccov <- USGSlvm::calcCanopyCover(x, res)
    cdens <- USGSlvm::calcCanopyDensity(x, res)
    s <- raster::stack(ccov, cdens)
    names(s) <- c("ccov", "cden")
    return(s)
  }
  cnpy <- create_canopy_stack(las_data, resolution)
  save_output(output_dir, "canopy", tile_name, "cnpy", cnpy)
  rm(cnpy)
   
  hpct <- USGSlvm::calcHeightPercentiles(las_data, resolution)
  save_output(output_dir, "height_pct", tile_name, "hpct", hpct)
  rm(hpct)
  
  hcnt <- USGSlvm::calcHeightPointCounts(las_data, resolution)
  save_output(output_dir, "height_cnt", tile_name, "hcnt", hcnt)
  
  hdens <- USGSlvm::calcHeightPointPercents(hcnt, resolution)
  save_output(output_dir, "height_den", tile_name, "hden", hdens)
  rm(hcnt, hdens, las_data)
  gc()
  
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

for (res in output_res){
  res_output <- file.path(folders["tiles"], paste(res, "m", sep = ""))
  products <- create_res_output(res_output)

  tile_time <- system.time(tiles <- lapply(lidar_files, calc_metrics,
    CRS = input_crs, output_dir = res_output, resolution = res))
}
