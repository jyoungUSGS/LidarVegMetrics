library(raster)
library(USGSlvm)
library(tools)
library(parallel)
library(rgdal)

# PARAMETERS ==================================================================
# Absolute path to working directory for output files
project_dir <- "C:/Users/nfkruska/Documents/projects/SHEN/va_shen_2011"

# input lidar files dir
in_dir <- "./HAG"

# output metrics tiles dir
out_dir <- "./tiles"

# output raster resolution(s). Can be integer or list of integers
# will create dir for each resolution in out_dir
output_res <- c(25)
unit <- "m"

# Input CRS of lidar files found by EPSG code
# NAD83 / UTM zone 17N: 26917
# NAD83(2011) / Conus Albers: 6350
epsg_code <- 26917

# points above/below values will not be considered as vegetation
# dbh: 1.37
veg_floor <- 1.37
veg_ceiling <- 50

# boolean: have the input points been normalized?
# !!~NOT CURRENTLY SUPPORTED~!!
# !!~POINTS MUST ALREADY BE NORMALIZED~!!
hag_nrml <- T

# raster format for output
# .img: "HFA"
# .tif: "GTiff"
ras_fmt <- "GTiff"

# FUNCTIONS ===================================================================
save_output <- function(output_dir, folder, tile_name, product, file){
  output_path <- file.path(output_dir, folder, paste(tile_name, product,
                                                     sep = "_"))
  if (file.exists(output_path)){
    warning(paste0("File ", output_path,
                   " already exists. It will be overwritten."))
  }
  
  if (product %in% c("hden", "hcnt", "hpct")){
    raster::writeRaster(file, output_path, ras_fmt, overwrite = T,
                        NAflag = -9999, options = "COMPRESS = DEFLATE")
  } else {
    raster::writeRaster(file, output_path, ras_fmt, overwrite = T,
                        bylayer = T, suffix = names(file),
                        NAflag = -9999, options = "COMPRESS = DEFLATE")
  }
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

format_tile_name <- function(x, resolution){
  tn <- basename(tools::file_path_sans_ext(x))
  np <- strsplit(tn, "_")
  tn <- paste0(unlist(np[[1]][1:5]), collapse = "_")
  tn <- paste(tn, resolution, sep = "_")
  return(tn)
}

calc_metrics <- function(x, CRS, output_dir, resolution, nrml = T){
  # data load with CRS
  las_data <- USGSlvm::readLidarData(x, CRS)
  # at this time, expecting normalized points
  names(las_data)[1] <- "Z_agl"
  
  # if data does not fall within range, class as noise
  las_data$Classification[las_data$Classification == 1 & veg_floor > 
                            las_data$Z_agl | las_data$Z_agl > veg_ceiling] <- 7
  
  # classify the points by height above ground
  las_data <- USGSlvm::classifyByHeight(las_data)
  
  for (res in resolution){
    # set output dir
    res_output <- file.path(output_dir, paste(res, unit, sep = ""))
    
    # format tile name by project spec
    tile_name <- format_tile_name(x, res)
    print(tile_name)
    
    # metrics functions
    stats <- USGSlvm::calcPointStatistics(las_data, resolution)
    save_output(res_output, "stats", tile_name, "stat", stats)
    
    vdr <- USGSlvm::calcVertDistRatio(las_data, resolution)
    save_output(res_output, "ratios", tile_name, "vdrs", vdr)
    
    create_canopy_stack <- function(x, res){
      ccov <- USGSlvm::calcCanopyCover(x, res)
      cdens <- USGSlvm::calcCanopyDensity(x, res)
      s <- raster::stack(ccov, cdens)
      names(s) <- c("ccov", "cden")
      return(s)
    }
    cnpy <- create_canopy_stack(las_data, resolution)
    save_output(res_output, "canopy", tile_name, "cnpy", cnpy)
    
    hpct <- USGSlvm::calcHeightPercentiles(las_data, resolution)
    save_output(res_output, "height_pct", tile_name, "hpct", hpct)
    
    hcnt <- USGSlvm::calcHeightPointCounts(las_data, resolution)
    save_output(res_output, "height_cnt", tile_name, "hcnt", hcnt)
    
    hdens <- USGSlvm::calcHeightPointPercents(hcnt, resolution)
    save_output(res_output, "height_den", tile_name, "hden", hdens)
  }
  return(TRUE)
}

# MAIN ========================================================================
setwd(project_dir)

# get list of input lidar files
lidar_files <- tools::list_files_with_exts(in_dir,
                                          c("LAS", "las", "LAZ", "laz"))
if (length(lidar_files) == 0){
  stop("No lidar files found in lidar directory.")
}

# get proj.4 string from EPSG code
epsg <- make_EPSG()
epsg <- epsg[! is.na(epsg$code), ]
input_crs <- epsg$prj4[epsg$code == epsg_code]
if (length(input_crs) == 0){
  stop("EPSG code was not found.")
}
rm(epsg)

# create output, resolution, and product dirs in project dir
dir.create(out_dir)
for (res in output_res){
  res_output <- file.path(out_dir, paste(res, unit, sep = ""))
  products <- create_res_output(res_output)
}

# make cluster, export packages / objects / functions
cl <- makeCluster(detectCores() - 2)
clusterEvalQ(cl, {
  library(rlas); library(sp); library(raster);
  library(rgdal); library(data.table); library(gstat);
  library(USGSlvm); library(tools); library(parallel)
})
clusterExport(cl, c("veg_floor", "veg_ceiling", "ras_fmt", "create_res_output",
                    "format_tile_name", "save_output", "unit"))

# process tiles in parallel
tile_time <- system.time(tiles <- parLapply(cl, lidar_files,
  calc_metrics, CRS = input_crs, output_dir = out_dir,
  resolution = output_res, nrml = hag_nrml))

# stop cluster
stopCluster(cl)
