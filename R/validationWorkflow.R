library(rlas)
library(sp)
library(raster)
library(USGSlvm)
library(rgdal)
library(data.table)
library(gstat)
library(tools)
library(parallel)


cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, {library(raster); library(USGSlvm); library(lidR)})

resolution <- 12

# data
setwd("C:/Users/nfkruska/Documents/data/SHEN")
vp_pnts <- readOGR("./geo_layers", layer = "SHEN_FVM_2003-2015_plots")

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

tile_dirs <- sapply(tile_dirs, FUN = function(x){file.path("D:/CDI2017/Lidar_collects/SHEN", x)})

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
vp_poly <- shen_points_2_plots(vp_pnts, 24)

# values for latest survey of all sites,Visit # 4, ~ 2013-2015
tree_v4 <- tree[Visit_Number == 4]
shrb_v4 <- shrb[Visit_Number == 4]
seed_v4 <- seed[Visit_Number == 4]

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

clusterExport(cl, c("vp_pnts", "vp_poly"))

tc <- parSapply(cl, tile_dirs, FUN = function(x){
  cat <- lidR::catalog(x)
  xy <- cat[28:31]
  tb <- data.frame(xy[, 2], xy[, 3], xy[, 1], xy[, 3], xy[, 1], xy[, 4],
                   xy[, 2], xy[, 4], xy[, 2], xy[, 3])
  names(tb) <- c("c1x", "c1y", "c2x", "c2y", "c3x", "c3y", "c4x", "c4y", "c1x",
                 "c1y")
  poly_list <- apply(tb, 1, FUN = function(x){Polygon(matrix(unlist(x),
                                                             nrow = 5, ncol = 2,
                                                             byrow = T))})
  polys_list <- list()
  for (i in 1:length(poly_list)){
    polys_list[[i]] <- Polygons(poly_list[i], i)
  }
  multi_polys <- SpatialPolygons(polys_list, proj4string = vp_pnts@proj4string)
  sp_df <- SpatialPolygonsDataFrame(multi_polys, as.data.frame(cat[34]))
  return(sp_df)
})

tile_select <- parLapply(cl, tc, fun = function(x){
  as.character(over(vp_poly, x)$filename)
})

vp_poly$fn <- apply(data.frame(tile_select), 1, max, na.rm=T)

# ERROR PLOTS
# 2L126-1
# 2L511-1
# 2L578-1
# 3L113-1
# 3L558-1

clusterExport(cl, c("vp_pnts", "vp_poly", "resolution"))

err <- list()
lv <- parLapply(cl, 1:3, fun = function(x){
  tryCatch({
      sp <- vp_poly[x, ]
      ld <- USGSlvm::readLidarData(sp$fn, sp@proj4string@projargs)
      
      sp_ext <- extent(sp)
      ld <- crop(ld, sp_ext)
      
      names(ld)[1] <- "Z_agl"
      
      ld <- USGSlvm::classifyByHeight(ld)
      stats <- USGSlvm::calcPointStatistics(ld, resolution)
      vdr <- USGSlvm::calcVertDistRatio(ld, resolution)
      ccov <- USGSlvm::calcCanopyCover(ld, resolution)
      cdens <- USGSlvm::calcCanopyDensity(ld, resolution)
      hpct <- USGSlvm::calcHeightPercentiles(ld, resolution)
      hcnt <- USGSlvm::calcHeightPointCounts(ld, resolution)
      hdens <- USGSlvm::calcHeightPointPercents(hcnt, resolution)
      
      s <- stack(stats, ccov, cdens, vdr, hpct)
      v <- c(siteID = as.character(sp$siteID), apply(getValues(s), 2, mean))
      return(v)
  }, error = function(e){
      err <- c(err, vp_poly$siteID[x])
      v <- c(as.character(vp_poly$siteID[x]), rep(NA, 21))
      return(v)
  }, finally = {
      name <- paste0("./plotstats/plot_", resolution, "m_", sp$siteID, ".tif")
      writeRaster(s, name, overwrite = T)
  })
})

lv_df <- data.frame(matrix(unlist(lv), ncol = 22, nrow = length(lv), byrow = T))
names(lv_df) <- c("plot", "hmin", "hmax", "havg", "hstd", "hske",
                       "hkur", "hqav", "ccov", "cdens", "vdr98", "vdr100",
                       "x10", "x20", "x30", "x40", "x50", "x60", "x70",
                       "x80", "x90", "x98")
write.csv(lv_df, "shen_plot_lidar_mets.csv")



# comp <- parSapply(cl, list.files("./plotstats", pattern = "plot_12m"),
#                   FUN = function(x){
#                     c(unlist(strsplit(unlist(strsplit(basename(x), "_"))[3],
#                                       "[.]"))[1])
# })
# undone <- vp_poly$siteID[!vp_poly$siteID %in% comp]

# l_cnt <- parLapply(cl, p_list, fun = function(x){
#   nlayers(stack(x))
# })
# df_col <- max(unlist(l_cnt))

# v_list <- parLapply(cl, p_list, fun = function(x){
#   v <- c(unlist(strsplit(unlist(strsplit(basename(x), "_"))[2], "[.]"))[1],
#          getValues(stack(x))[1:21])
#   return(v)
# })
# plot_stats <- data.frame(matrix(unlist(v_list), nrow = 160, ncol = 22,
#                                 byrow = T))
# names(plot_stats) <- c("plot", "hmin", "hmax", "havg", "hstd", "hske",
#                        "hkur", "hqav", "ccov", "cdens", "vdr98", "vdr100",
#                        "x10", "x20", "x30", "x40", "x50", "x60", "x70",
#                        "x80", "x90", "x98")
# write.csv(plot_stats, "./plotstats/plot_stats.csv")
# 
# names(vp_poly)[1] <- names(plot_stats)[1] <- "siteID"
# vp_stats <- merge(vp_poly, plot_stats)
# cols = c(22:42)   
# vp_stats@data[,cols] = apply(vp_stats@data[,cols], 2, function(x)
#   {as.numeric(as.character(x))
# })

r_table <- data.frame(matrix(ncol = 10, nrow = 21),
                      row.names = names(vp_stats@data[22:42]))
for (i in 22:42){
  for (j in 12:21){
    tmp <- lm(vp_stats@data[, j] ~ vp_stats@data[, i])
    plot(vp_stats@data[, j], vp_stats@data[, i], ylab = names(vp_stats)[i],
         xlab = names(vp_stats)[j])
    abline(tmp)
    
    names(r_table)[(j-11)] <- names(vp_stats)[j]
    r_table[(i-21), (j-11)] <- summary(tmp)$adj.r.squared  
  }
}
write.csv(r_table, "./plotstats/shen_plots_adjrsq_lidar.csv")







