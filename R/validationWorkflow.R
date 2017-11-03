library(rlas)
library(sp)
library(raster)
library(USGSlvm)
library(rgdal)
library(data.table)
library(gstat)

setwd("C:/Users/nfkruska/Documents/data/SHEN")

resolution <- 24

# load plot points
vp_pnts <- readOGR("./geo_layers", layer = "SHEN_FVM_2003-2015_plots")

# make plot Polygons
# plot point is NW corner
shen_points_2_plots <- function(x, dim){
  vp_xy <- x@data[, 10:11]
  vp_xy[, 3] <- vp_xy[, 1] + 24
  vp_xy[, 4] <- vp_xy[, 2]
  vp_xy[, 5] <- vp_xy[, 1] + 24
  vp_xy[, 6] <- vp_xy[, 2] - 24
  vp_xy[, 7] <- vp_xy[, 1]
  vp_xy[, 8] <- vp_xy[, 2] - 24
  vp_xy[, 9] <- vp_xy[, 1]
  vp_xy[, 10] <- vp_xy[, 2]
  names(vp_xy) <- c("c1x", "c1y", "c2x", "c2y", "c3x", "c3y", "c4x",
    "c4y", "c1x", "c1y")
  poly_list <- apply(vp_xy, 1, FUN = function(x){Polygon(matrix(unlist(x),
    nrow = 5, ncol = 2, byrow = T))})
  polys_list <- list()
  for (i in 1:length(poly_list)){
    polys_list[[i]] <- Polygons(poly_list[i], i)
  }
  multi_polys <- SpatialPolygons(polys_list, proj4string = x@proj4string)
  plot_polys <- SpatialPolygonsDataFrame(multi_polys, x@data)
  return(plot_polys)
}

# load field information
tree <- fread("./field_data/tree_data.csv")
shrb <- fread("./field_data/shrub_data.csv")
seed <- fread("./field_data/seedling_data.csv")

# values for latest survey of all sites,Visit # 4, ~ 2013-2015
tree_v4 <- tree[Visit_Number == 4]
shrb_v4 <- shrb[Visit_Number == 4]
seed_v4 <- seed[Visit_Number == 4]

# field plots
vp_poly <- shen_points_2_plots(vp_pnts, 24)

# calc field stats
field_stats <- tree_v4[, .(mean(DBHcm, na.rm = T)), by = .(SiteID)]
setnames(field_stats, "V1", "tree_mean_dbh_cm")

field_stats <- merge(field_stats, tree_v4[, .(sum(TreeBA_m2, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "tree_sum_ba_m2")
field_stats$ba_per_ha <- field_stats$tree_sum_ba * (10000 / (24*24))

field_stats <- merge(field_stats, tree_v4[, .(max(Tree_ht_m_calc, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "tree_max_ht_m")
field_stats$tree_max_ht_m[field_stats$tree_max_ht_m == -Inf] <- NA

field_stats <- merge(field_stats, tree_v4[, .(mean(Tree_ht_m_calc, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "tree_mean_ht_m")

field_stats <- merge(field_stats, tree_v4[, .(min(Tree_ht_m_calc, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "tree_min_ht_m")
field_stats$tree_min_ht_m[field_stats$tree_min_ht_m == Inf] <- NA

field_stats <- merge(field_stats, shrb_v4[, .(mean(Calc_DBH, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "shrb_mean_dbh_cm")

field_stats <- merge(field_stats, shrb_v4[, .(sum(Stem_count, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "shrb_stm_cnt")

field_stats <- merge(field_stats, seed_v4[, .(sum(Stem_count, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "seed_stm_cnt")

field_stats <- merge(field_stats, tree_v4[, .N,
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "N", "tree_stm_cnt")

# merge field stats with plot polys
vp_poly <- merge(vp_poly, field_stats, by = "SiteID", all.x=TRUE)

get_tile_dir <- function(x, crs){
  cat <- lidR::catalog(x)
  xy <- cat[28:31]
  tb <- data.frame(xy[, 2], xy[, 3], xy[, 1], xy[, 3], xy[, 1], xy[, 4],
    xy[, 2], xy[, 4], xy[, 2], xy[, 3])
  names(tb) <- c("c1x", "c1y", "c2x", "c2y", "c3x", "c3y", "c4x", "c4y", "c1x",
    "c1y")
  poly_list <- apply(tb, 1, FUN = function(x){Polygon(matrix(unlist(x),
    nrow = 5, ncol = 2, byrow = T))})
  polys_list <- list()
  for (i in 1:length(poly_list)){
    polys_list[[i]] <- Polygons(poly_list[i], i)
  }
  multi_polys <- SpatialPolygons(polys_list, proj4string = crs)
  sp_df <- SpatialPolygonsDataFrame(multi_polys, as.data.frame(cat[34]))
  return(sp_df)
}

# td_2011 <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/ShenValley2011/HAG/UNBuffered",
#   vp_poly@proj4string)
# td_2012 <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/NRCS_RockinghamCnty_2012/HAG/UNBuffered",
#   vp_poly@proj4string)
# td_2013_cda <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/NRCS2013/CDa/HAG/UNBuffered",
#   vp_poly@proj4string)
# td_2013_cdb <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/NRCS2013/CDb/HAG/UNBuffered",
#   vp_poly@proj4string)
# td_2013_nda <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/NRCS2013/NDa/HAG/UNBuffered",
#   vp_poly@proj4string)
# td_2013_ndb <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/NRCS2013/NDb/HAG/UNBuffered",
#   vp_poly@proj4string)
# td_2015n_cdb <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/Chesapeake_2015/CLASSIFIED_LAZ_VA_SP_NORTH_SNP/CDb/Tiled/HAG/UNBuffered",
#   vp_poly@proj4string)
# td_2015n_sda <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/Chesapeake_2015/CLASSIFIED_LAZ_VA_SP_NORTH_SNP/SDa/HAG/UNBuffered",
#   vp_poly@proj4string)
# td_2015n_sdb <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/Chesapeake_2015/CLASSIFIED_LAZ_VA_SP_NORTH_SNP/SDb/HAG/UNBuffered",
#   vp_poly@proj4string)
# td_2015s_sda <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/Chesapeake_2015/CLASSIFIED_LAZ_VA_SP_SOUTH_SNP/SDa/HAG/UNBuffered",
#   vp_poly@proj4string)
# td_2015s_sdb <- get_tile_dir("D:/CDI2017/Lidar_collects/SHEN/Chesapeake_2015/CLASSIFIED_LAZ_VA_SP_SOUTH_SNP/SDb/HAG/UNBuffered",
#   vp_poly@proj4string)
#
# writeOGR(td_2011, "./geo_layers/tile_dirs/td_2011.shp", "td_2011", driver = "ESRI Shapefile")
# writeOGR(td_2012, "./geo_layers/tile_dirs/td_2012.shp", "td_2012", driver = "ESRI Shapefile")
# writeOGR(td_2013_cda, "./geo_layers/tile_dirs/td_2013_cda.shp", "td_2013_cda", driver = "ESRI Shapefile")
# writeOGR(td_2013_cdb, "./geo_layers/tile_dirs/td_2013_cdb.shp", "td_2013_cdb", driver = "ESRI Shapefile")
# writeOGR(td_2013_nda, "./geo_layers/tile_dirs/td_2013_nda.shp", "td_2013_nda", driver = "ESRI Shapefile")
# writeOGR(td_2013_ndb, "./geo_layers/tile_dirs/td_2013_ndb.shp", "td_2013_ndb", driver = "ESRI Shapefile")
# writeOGR(td_2015n_cdb, "./geo_layers/tile_dirs/td_2015n_cdb.shp", "td_2015n_cdb", driver = "ESRI Shapefile")
# writeOGR(td_2015n_sda, "./geo_layers/tile_dirs/td_2015n_sda.shp", "td_2015n_sda", driver = "ESRI Shapefile")
# writeOGR(td_2015n_sdb, "./geo_layers/tile_dirs/td_2015n_sdb.shp", "td_2015n_sdb", driver = "ESRI Shapefile")
# writeOGR(td_2015s_sda, "./geo_layers/tile_dirs/td_2015s_sda.shp", "td_2015s_sda", driver = "ESRI Shapefile")
# writeOGR(td_2015s_sdb, "./geo_layers/tile_dirs/td_2015s_sdb.shp", "td_2015s_sdb", driver = "ESRI Shapefile")

# Use ArcGIS to merge all las directory tile indexes together because
# R just can't do it that easily

sld <- readOGR("./geo_layers", layer = "shen_las_dir")
vp_poly$fn <- as.character(over(vp_poly, sld)$filename)

# shen_albers_tiles <- readOGR("./geo_layers", layer = "shen_albers_tiles")
# tile_index_name <- paste(paste0("e", substr(shen_albers_tiles@data$TileID, 2, 5)),
#   paste0("n", substr(shen_albers_tiles@data$TileID, 7, 10)), sep = "_")
#
# shen_albers_tiles$tile_name <- paste(paste0("e",
#   substr(shen_albers_tiles@data$TileID, 2, 5)), paste0("n",
#   substr(shen_albers_tiles@data$TileID, 7, 10)), sep = "_")
#
# shen_albers_tiles <- spTransform(shen_albers_tiles, vp_poly@proj4string)
# vp_poly$tile_index <- over(vp_poly, shen_albers_tiles)$tile_name
#
# laz_hag_list <- list.files("D:/CDI2017/Lidar_collects/SHEN",
#   pattern = "_HAG.laz", recursive = T)


# this should be a function written in parallel. Can that be done to write data
# back to object? Or return a list of values and merge later?

library(parallel)


for (i in 1:nrow(vp_poly)){
  ld <- readLidarData(vp_poly$fn[i], vp_poly@proj4string@projargs)
  pld_ext <- extent(vp_poly[i, ])
  ld <- crop(ld, pld_ext)

  names(ld)[1] <- "Z_agl"

  ld <- USGSlvm::classifyByHeight(ld)
  stats <- USGSlvm::calcPointStatistics(ld, resolution)
  vdr <- USGSlvm::calcVertDistRatio(ld, resolution)
  ccov <- USGSlvm::calcCanopyCover(ld, resolution)
  cdens <- USGSlvm::calcCanopyDensity(ld, resolution)
  hpct <- USGSlvm::calcHeightPercentiles(ld, resolution)
  hcnt <- USGSlvm::calcHeightPointCounts(ld, resolution)
  hdens <- USGSlvm::calcHeightPointPercents(hcnt, resolution)

  s <- stack(stats, ccov, cdens, vdr, hpct, hcnt, hdens)

  name <- paste0("./plotstats/plot_", vp_poly$SiteID[i], ".tif")
  writeRaster(s, name)
}

# ERROR PLOTS
# 2L126-1
# 2L511-1
# 2L578-1
# 3L113-1
# 3L558-1

# PSEUDO CODE FOR EXTRACTING LIDAR METRICS BY SITE
#
# extract tile index ID to veg poly
# Recursively search through all shenandoah data folders to find a matching
# LAZ file by tile index ID, extract full path of LAZ to veg poly
# For each Veg poly
#   load intersecting lidar file
#   crop point data to veg poly extent
#   calculate all lidar metrics
#   extract lidar metric values to veg poly
#   save lidar metrics by veg poly name
