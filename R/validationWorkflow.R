library(rlas)
library(sp)
library(raster)
library(USGSlvm)
library(rgdal)
library(data.table)
library(gstat)

setwd("C:/Temp/CONUS")

# create veg plot polygons
veg_plot_pnts <- readOGR(inputDir, layer = "SNP_FORVEG_all_plots")
outputCRS <- veg_plot_pnts@proj4string
veg_plot_xy <- veg_plot_pnts@data[,10:11]
veg_plot_xy[,3] <- veg_plot_xy[,1] + 24
veg_plot_xy[,4] <- veg_plot_xy[,2]
veg_plot_xy[,5] <- veg_plot_xy[,1] + 24
veg_plot_xy[,6] <- veg_plot_xy[,2] - 24
veg_plot_xy[,7] <- veg_plot_xy[,1]
veg_plot_xy[,8] <- veg_plot_xy[,2] - 24
veg_plot_xy[,9] <- veg_plot_xy[,1]
veg_plot_xy[,10] <- veg_plot_xy[,2]
names(veg_plot_xy) <- c("c1x", "c1y", "c2x", 'c2y', 'c3x', 'c3y', 'c4x', 'c4y', 'c1x', 'c1y')
poly_list <- apply(veg_plot_xy, 1, FUN = function(x){Polygon(matrix(unlist(x), nrow = 5, ncol = 2, byrow = T))})
polys_list <- list()
for (i in 1:length(poly_list)){
  polys_list[[i]] <- Polygons(poly_list[i], i)
}
shen_polys <- SpatialPolygons(polys_list, proj4string = outputCRS)
veg_plot_poly <- SpatialPolygonsDataFrame(shen_polys, veg_plot_pnts@data)
plot(veg_plot_poly)

# load site information
fData_tree <- as.data.table(read.csv("shen_data_trees.csv"))
fData_shrub <- as.data.table(read.csv("shen_data_shrubs.csv"))
fData_seedling <- as.data.table(read.csv("shen_data_seedlings.csv"))

# calc stats: average dbh, total basal area
# values for latest survey of all sites,Visit # 4, ~ 2013-2015
fData_tree_v4 <- fData_tree[Visit_Number==4]
fStats_tree_v4 <- fData_tree_v4[,.(mean(DBHcm, na.rm=T)), by=.(SiteID)]
setnames(fStats_tree_v4, "V1", "avgDBH")
fStats_tree_v4$sumBA <- fData_tree_v4[,.(sum(TreeBA_m2, na.rm=T)), by=.(SiteID)]$V1
veg_plot_poly@data <- merge(veg_plot_poly@data, fStats_tree_v4, by = "SiteID")

# save data as shapefile
writeOGR(veg_plot_poly, dsn = inputDir, layer = "shen_veg_plot_poly", driver = "ESRI Shapefile", overwrite_layer = T)
