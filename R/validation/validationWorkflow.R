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
site_dim <- 24

# field site visit number
visit_num <- 4

# data & paths
setwd("C:/Users/nfkruska/Documents/projects/SHEN")

# load lidar stats
lidar_stats <- fread("./site_stats/site_stats.csv")
setnames(field_stats, "SLOPE_%", "SLOPE_perc")
lidar_stats$V1 <- NULL
lidar_stats[, 2:22] <- as.data.frame(sapply(lidar_stats[, 2:22], as.numeric))

# load survey data
tree <- fread("./field_data/tree_data.csv")
shrb <- fread("./field_data/shrub_data.csv")
seed <- fread("./field_data/seedling_data.csv")

# load site information, make into site polygons
# func assumes site point is NW corner
field_sites <- fread("./field_data/SHEN_FVM_2003-2015.csv")
setnames(field_sites, "SLOPE_%", "SLOPE_perc")
field_sites$ELU_ID <- as.factor(field_sites$ELU_ID)
coordinates(field_sites) <- ~ xUTMz17NAD83 + yUTMz17NAD83 
proj4string(field_sites) <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
shen_pnt_2_poly <- function(x, dim){
  vp_xy <- matrix(nrow = nrow(x), ncol= 10)
  vp_xy[, 1:2] <- x@coords
  vp_xy[, 3] <- vp_xy[, 1] + dim
  vp_xy[, 4] <- vp_xy[, 2]
  vp_xy[, 5] <- vp_xy[, 1] + dim
  vp_xy[, 6] <- vp_xy[, 2] - dim
  vp_xy[, 7] <- vp_xy[, 1]
  vp_xy[, 8] <- vp_xy[, 2] - dim
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
  site_polys <- SpatialPolygonsDataFrame(multi_polys, x@data)
  return(site_polys)
}
field_sites <- shen_pnt_2_poly(field_sites, site_dim)

# select survey visit #.
# Visit # 4 ~ 2013-2015
tree_sub <- tree[Visit_Number == visit_num]
shrb_sub <- shrb[Visit_Number == visit_num]
seed_sub <- seed[Visit_Number == visit_num]

# calculate field stats: mean dbh, basal area per site, max canopy height,
# mean canopy height, min canopy height, tree stem count.
field_stats <- tree_sub[, .(mean(DBHcm, na.rm = T)), by = .(SiteID)]
setnames(field_stats, "V1", "tree_mean_dbh_cm")

field_stats <- merge(field_stats, tree_sub[, .(sum(TreeBA_m2, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "tree_sum_ba_m2")

field_stats <- merge(field_stats, tree_sub[, .(max(Tree_ht_m_calc, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "tree_max_ht_m")
field_stats$tree_max_ht_m[field_stats$tree_max_ht_m == -Inf] <- NA

field_stats <- merge(field_stats, tree_sub[, .(mean(Tree_ht_m_calc, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "tree_mean_ht_m")

field_stats <- merge(field_stats, tree_sub[, .(min(Tree_ht_m_calc, na.rm = T)),
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "V1", "tree_min_ht_m")
field_stats$tree_min_ht_m[field_stats$tree_min_ht_m == Inf] <- NA

field_stats <- merge(field_stats, tree_sub[, .N,
  by = .(SiteID)], by = "SiteID", all.x=TRUE)
setnames(field_stats, "N", "tree_stm_cnt")

# merge field and lidar stats with polygons
field_sites <- merge(field_sites, field_stats, by = "SiteID", all.x=TRUE)
field_sites <- merge(field_sites, lidar_stats, by = "SiteID", all.x=TRUE)

# extract merged field data
dat <- field_sites@data
# remove any rows with missing data
dat <- dat[complete.cases(dat), ]
dat <- as.data.frame(dat)

# adjr2 table for all metrics with all field data
r_table <- data.frame(matrix(nrow = 21, ncol = 6))
for (l in 16:36){
  for (f in 10:15){
    mod <- lm(dat[, f] ~ dat[, l])
    plot(dat[, f] ~ dat[, l], xlab = names(dat)[l], ylab = names(dat)[f])
    abline(mod)
    r_table[(l-15), (f-9)] <- summary(mod)$adj.r.squared
  }
}
rownames(r_table) <- names(dat[, 16:36])
colnames(r_table) <- names(dat[, 10:15])

# random forest classification and regression
rf_mean_dbh <- randomForest(tree_mean_dbh_cm~., data = dat[, c(3:6, 10, 16:36)])
rf_sum_ba <- randomForest(tree_sum_ba_m2 ~., data = dat[, c(3:6, 11, 16:36)])
rf_max_ht <- randomForest(tree_max_ht_m~., data = dat[, c(3:6, 12, 16:36)])
rf_mean_ht <- randomForest(tree_mean_ht_m~., data = dat[, c(3:6, 13, 16:36)])
rf_min_ht <- randomForest(tree_min_ht_m~., data = dat[, c(3:6, 14, 16:36)])
rf_stem_cnt <- randomForest(tree_stm_cnt~., data = dat[, c(3:6, 15, 16:36)])

# stratify by ELU_ID
strats <- unique(dat$ELU_ID)
dat_sub <- dat[dat$ELU_ID == strats[3], ]
# find best subset of variables
lps_sub <- regsubsets(x = dat_sub[, 16:36], y = dat_sub[, 10], nbest = 3, nvmax = 3)
