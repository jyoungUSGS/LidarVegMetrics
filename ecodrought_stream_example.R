library(gdalUtils)
library(raster)
library(USGSlvm)
library(tools)
library(parallel)
library(RColorBrewer)
library(rgdal)

pal_spec <- colorRampPalette(brewer.pal(11, "Spectral"))

setwd("C:/Users/nfkruska/Documents/projects/SHEN")
input_crs <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"
tile_shp <- rgdal::readOGR("./geo_layers/stream_example_tiles.shp", stringsAsFactors = F)
strm_buf <- rgdal::readOGR("./geo_layers/streams_25m_buffer.shp", stringsAsFactors = F)

# dbh <<- 1.37
veg_floor <- 1.37
veg_ceiling <- 50

lidar_files <- tile_shp$filename
o_dir <- "./stream_example"
o_res <- 10


cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, {
  library(rlas); library(sp); library(raster);
  library(rgdal); library(data.table); library(gstat);
  library(USGSlvm); library(tools); library(parallel)
})
clusterExport(cl, c("veg_floor", "veg_ceiling", "ras_fmt"))

create_res_output(o_dir)

tile_time <- system.time(tiles <- clusterApplyLB(cl, lidar_files3, calc_metrics,
                                                 CRS = input_crs,
                                                 output_dir = o_dir,
                                                 resolution = o_res))
stopCluster(cl)

mos <- list_files_with_exts("./stream_example/canopy", exts = "tif")
mos <- mos[grep("ccov", mos)]
gdalUtils::mosaic_rasters(mos, "./stream_example/ccov_10m_mosaic.tif")
r_ccov <- raster("./stream_example/ccov_10m_mosaic.tif")

mos <- list_files_with_exts("./stream_example/canopy", exts = "tif")
mos <- mos[grep("cden", mos)]
gdalUtils::mosaic_rasters(mos, "./stream_example/cden_10m_mosaic.tif")
r_cden <- raster("./stream_example/cden_10m_mosaic.tif")

mos <- list_files_with_exts("./stream_example/stats", exts = "tif")
mos <- mos[grep("hmax", mos)]
gdalUtils::mosaic_rasters(mos, "./stream_example/hmax_10m_mosaic.tif")
r_hmax <- raster("./stream_example/hmax_10m_mosaic.tif")

plot(r_hmax, col = pal_spec(100), axes = F)
plot(tile_shp, add = T)
plot(streams, col = "blue", lwd = 1.5, add = T)




h_vals <- getValues(r_hmax)
c_vals <- getValues(r_ccov)

dat <- as.data.frame(cbind(h_vals, c_vals))
smoothScatter(x = dat$h_vals, y = dat$c_vals)
mod <- lm(c_vals ~ h_vals, data = dat)