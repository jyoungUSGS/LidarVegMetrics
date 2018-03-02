library(data.table)
library(gstat)
library(leaps)
library(nnet)
library(parallel)
library(randomForest)
library(raster)
library(rgdal)
library(rlas)
library(sp)
library(tools)
library(USGSlvm)

# site dimension
site_dim <-24

# field site visit number
visit_num <- 4

# set desired output resolution
resolution <- 12

# data & paths
setwd("C:/Users/nfkruska/Documents/projects/SHEN")
out_path <- "./site_stats"
lid_path <- "D:/CDI2017/Lidar_collects/SHEN"

ss_pnts <- readOGR("./geo_layers", layer = "SHEN_FVM_2003-2015_plots")


tile_dirs <- c("ShenValley2011/HAG/UNBuffered",
               "NRCS_RockinghamCnty_2012/HAG/UNBuffered",
               "NRCS2013/CDa/HAG/UNBuffered",
               "NRCS2013/CDa/HAG/UNBuffered",
               "NRCS2013/CDb/HAG/UNBuffered",
               "NRCS2013/NDa/HAG/UNBuffered",
               "NRCS2013/NDb/HAG/UNBuffered",
               "Chesapeake_2015/CLASSIFIED_LAZ_VA_SP_NORTH_SNP/CDb/Tiled/HAG/UNBuffered",
               "Chesapeake_2015/CLASSIFIED_LAZ_VA_SP_NORTH_SNP/SDa/HAG/UNBuffered",
               "Chesapeake_2015/CLASSIFIED_LAZ_VA_SP_NORTH_SNP/SDb/HAG/UNBuffered",
               "Chesapeake_2015/CLASSIFIED_LAZ_VA_SP_SOUTH_SNP/SDa/HAG/UNBuffered",
               "Chesapeake_2015/CLASSIFIED_LAZ_VA_SP_SOUTH_SNP/SDb/HAG/UNBuffered")
tile_dirs <- sapply(tile_dirs, FUN = function(x){file.path(lid_path, x)})



# # make cluster for parallel computing
# cl <- makeCluster(detectCores() - 1)
# clusterEvalQ(cl, {library(raster); library(sp); library(USGSlvm);
#                   library(rgdal); library(lidR)})
# clusterExport(cl, c("field_sites"))
# 
# # for each site, find the lidar tile that covers it
# tc <- parSapply(cl, tile_dirs, FUN = function(x){
#   cat <- lidR::catalog(x)
#   xy <- cat[28:31]
#   tb <- data.frame(xy[, 2], xy[, 3], xy[, 1], xy[, 3], xy[, 1], xy[, 4],
#                    xy[, 2], xy[, 4], xy[, 2], xy[, 3])
#   names(tb) <- c("c1x", "c1y", "c2x", "c2y", "c3x", "c3y", "c4x", "c4y", "c1x",
#                  "c1y")
#   poly_list <- apply(tb, 1, FUN = function(x){Polygon(matrix(unlist(x),
#                                                              nrow = 5, ncol = 2,
#                                                              byrow = T))})
#   polys_list <- list()
#   for (i in 1:length(poly_list)){
#     polys_list[[i]] <- Polygons(poly_list[i], i)
#   }
#   multi_polys <- SpatialPolygons(polys_list, proj4string = field_sites@proj4string)
#   sp_df <- SpatialPolygonsDataFrame(multi_polys, as.data.frame(cat[34]))
#   return(sp_df)
# })
# 
# ts <- parLapply(cl, tc, fun = function(x){
#   as.character(over(field_sites, x)$filename)
# })
# 
# # merge tile dir to site polys
# field_sites$fn <- parApply(cl, data.frame(ts), 1, FUN = max, na.rm=T)
# 
# 
# # ERROR sites: 3L113-1, 3L558-1
# # The links for these sites must be corrected before 
# # calculating lidar metrics or else they will return NA.
# # WARNING: this is not a fool-proof method. It simply replaces the links
# # to tiles I know do not work with the next alternative link found.
# 
# # for known error sites, find alternative lidar tile link, replace known
# # bad ones.
# err_sites <- c("3L113-1", "3L558-1")
# for (e in err_sites){
#   alt <- list.files(lid_path, pattern = basename(field_sites$fn[field_sites$SiteID == e]), 
#                     recursive = T)
#   alt <- sapply(alt, FUN = function(x){file.path(lid_path, x)})
#   alt <- alt[alt != field_sites$fn[field_sites$SiteID == e]]
#   field_sites$fn[field_sites$SiteID == e] <- alt[1]
# }
# 
# clusterExport(cl, c("field_sites", "resolution", "out_path"))
# 
# # for each site, load the lidar tile, clip points, calculate metrics,
# # write output raster, add metric values to list
# lv <- parLapply(cl, 1:nrow(field_sites), fun = function(x){
#   sp <- field_sites[x, ]
#   ld <- USGSlvm::readLidarData(sp$fn, sp@proj4string@projargs)
#   
#   sp_ext <- extent(sp)
#   ld <- crop(ld, sp_ext)
#   if (is.null(ld)){
#     err <- c(err, field_sites$siteID[x])
#     v <- c(as.character(field_sites$siteID[x]), rep(NA, 21))
#     return(v)
#   } else {
#     names(ld)[1] <- "Z_agl"
#     
#     ld <- USGSlvm::classifyByHeight(ld)
#     stats <- USGSlvm::calcPointStatistics(ld, resolution)
#     vdr <- USGSlvm::calcVertDistRatio(ld, resolution)
#     ccov <- USGSlvm::calcCanopyCover(ld, resolution)
#     cdens <- USGSlvm::calcCanopyDensity(ld, resolution)
#     hpct <- USGSlvm::calcHeightPercentiles(ld, resolution)
#     hcnt <- USGSlvm::calcHeightPointCounts(ld, resolution)
#     hdens <- USGSlvm::calcHeightPointPercents(hcnt, resolution)
#     
#     s <- stack(stats, ccov, cdens, vdr, hpct)
#     v <- c(siteID = as.character(sp$SiteID), apply(getValues(s), 2, mean))
#     name <- paste0(out_path, "/site_", resolution, "m_", sp$SiteID, ".tif")
#     raster::writeRaster(s, name, overwrite = T)
#     return(v)
#   }
# })
# 
# # make output metrics into data frame
# lidar_stats <- data.frame(matrix(unlist(lv), ncol = 22, nrow = length(lv), byrow = T))
# names(lidar_stats) <- c("SiteID", "hmin", "hmax", "havg", "hstd", "hske", "hkur",
#                   "hqav", "ccov", "cdens", "vdr98", "vdr100", "x10", "x20",
#                   "x30", "x40", "x50", "x60", "x70", "x80", "x90", "x98")
# # save lidar metrics
# write.csv(lidar_stats, file.path(out_path, "site_stats.csv"))

# get metric values from output rasters in folder (alt method)
# t_list <- list_files_with_exts("out_path", "tif")
# lv <- parLapply(cl, t_list, fun = function(x){
#   v <- c(unlist(strsplit(unlist(strsplit(basename(x), "_"))[3], "[.]"))[1],
#          apply(getValues(stack(x)), 2, mean))
# })
# make output metrics into data frame
# lidar_stats <- data.frame(matrix(unlist(lv), ncol = 22, nrow = length(lv), byrow = T))
# names(lidar_stats) <- c("SiteID", "hmin", "hmax", "havg", "hstd", "hske", "hkur",
#                   "hqav", "ccov", "cdens", "vdr98", "vdr100", "x10", "x20",
#                   "x30", "x40", "x50", "x60", "x70", "x80", "x90", "x98")
# save lidar metrics
# write.csv(lidar_stats, file.path(out_path, "site_stats.csv"))

# find error sites
# comp <- parSapply(cl, list.files(out_path, pattern = "site_12m"),
#                   FUN = function(x){
#                     c(unlist(strsplit(unlist(strsplit(basename(x), "_"))[3],
#                                       "[.]"))[1])
#                   })
# undone <- field_sites[! field_sites$SiteID %in% comp, ]