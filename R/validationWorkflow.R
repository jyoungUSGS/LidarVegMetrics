library(rlas)
library(sp)
library(raster)
library(USGSlvm)
library(rgdal)
library(data.table)
library(gstat)

setwd("C:/Users/nfkruska/Documents/data/SHEN")

# load plot points
veg_plot_pnts <- readOGR("./geo_layers", layer = "SHEN_FVM_2003-2015_plots")

# make plot Polygons
# plot point is NW corner
shen_points_2_plots <- function(x, dim){
  veg_plot_xy <- x@data[, 10:11]
  veg_plot_xy[, 3] <- veg_plot_xy[, 1] + 24
  veg_plot_xy[, 4] <- veg_plot_xy[, 2]
  veg_plot_xy[, 5] <- veg_plot_xy[, 1] + 24
  veg_plot_xy[, 6] <- veg_plot_xy[, 2] - 24
  veg_plot_xy[, 7] <- veg_plot_xy[, 1]
  veg_plot_xy[, 8] <- veg_plot_xy[, 2] - 24
  veg_plot_xy[, 9] <- veg_plot_xy[, 1]
  veg_plot_xy[, 10] <- veg_plot_xy[, 2]
  names(veg_plot_xy) <- c("c1x", "c1y", "c2x", "c2y", "c3x", "c3y", "c4x",
    "c4y", "c1x", "c1y")
  poly_list <- apply(veg_plot_xy, 1, FUN = function(x){Polygon(matrix(unlist(x),
    nrow = 5, ncol = 2, byrow = T))})
  polys_list <- list()
  for (i in 1:length(poly_list)){
    polys_list[[i]] <- Polygons(poly_list[i], i)
  }
  multi_polys <- SpatialPolygons(polys_list, proj4string = x@proj4string)
  plot_polys <- SpatialPolygonsDataFrame(multi_polys, x@data)
  return(plot_polys)
}
veg_plot_poly <- shen_points_2_plots(veg_plot_pnts, 24)

# load field information
tree <- fread("./field_data/tree_data.csv")
shrb <- fread("./field_data/shrub_data.csv")
seed <- fread("./field_data/seedling_data.csv")

# values for latest survey of all sites,Visit # 4, ~ 2013-2015
tree_v4 <- tree[Visit_Number == 4]
shrb_v4 <- shrb[Visit_Number == 4]
seed_v4 <- seed[Visit_Number == 4]

# calc field stats
field_stats <- tree_v4[, .(mean(DBHcm, na.rm = T)), by = .(SiteID)]
names(field_stats)[2] <- "tree_mean_dbh"
field_stats <- merge(field_stats, tree_v4[, .(sum(TreeBA_m2, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
names(field_stats)[3] <- "tree_sum_ba"
field_stats <- merge(field_stats, tree_v4[, .(max(Tree_ht_m_calc, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
names(field_stats)[4] <- "tree_max_ht"
field_stats <- merge(field_stats, tree_v4[, .(mean(Tree_ht_m_calc, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
names(field_stats)[5] <- "tree_mean_ht"
field_stats <- merge(field_stats, tree_v4[, .(min(Tree_ht_m_calc, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
names(field_stats)[6] <- "tree_min_ht"
field_stats$tree_max_ht[field_stats$tree_max_ht == -Inf] <- NA
field_stats <- merge(field_stats, shrb_v4[, .(mean(Calc_DBH, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
names(field_stats)[7] <- "shrb_mean_dbh"
field_stats <- merge(field_stats, shrb_v4[, .(sum(Stem_count, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
names(field_stats)[8] <- "shrb_stm_cnt"
field_stats <- merge(field_stats, seed_v4[, .(sum(Stem_count, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
names(field_stats)[9] <- "seed_stm_cnt"
field_stats <- merge(field_stats, tree_v4[, .N,
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
  names(field_stats)[10] <- "tree_stm_cnt"

# merge field stats with plot polys
veg_plot_poly@data <- merge(veg_plot_poly@data, field_stats, by = "SiteID")

# PSEUDO CODE FOR EXTRACTING LIDAR METRICS BY SITE
#
# Load albers tile index
# extract tile index ID to veg poly
# Recursively search through all shenandoah data folders to find a matching
# LAZ file by tile index ID, extract full path of LAZ to veg poly
# For each Veg poly
#   load intersecting lidar file
#   crop point data to veg poly extent
#   calculate all lidar metrics
#   extract lidar metric values to veg poly
#   save lidar metrics by veg poly name
