library(rlas)
library(sp)
library(raster)
library(lidarpro)
library(rgdal)
library(data.table)

input_dir <- "C:/Temp"
input_file <- "LAS_N16_4759_20.laz"
# CRS VA State Plane North
input_CRS <- "+proj=lcc +lat_1=39.2 +lat_2=38.03333333333333 +lat_0=37.66666666666666 +lon_0=-78.5 +x_0=3500000.0001016 +y_0=2000000.0001016 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
output_CRS <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
output_res <- 24
setwd(input_dir)

# create veg plot polygons
veg_plot_pnts <- readOGR(input_dir, layer = "SNP_FORVEG_all_plots")
output_CRS <- veg_plot_pnts@proj4string
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
shen_polys <- SpatialPolygons(polys_list, proj4string = output_CRS)
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
writeOGR(veg_plot_poly, dsn = input_dir, layer = "shen_veg_plot_poly", driver = "ESRI Shapefile", overwrite_layer = T)

# load LAS data
lasData <- rlas::readlasdata(input_file)

sp::coordinates(lasData) <- c("X", "Y" ,"Z")
sp::proj4string(lasData) <- sp::CRS(input_CRS)
lasData <- sp::spTransform(lasData, output_CRS)
lasData <- flagNoiseKnn(lasData)
lasData <- lasData[lasData$Classification %in% c(0, 1, 2), ]

# lidar metrics
rast_template <- raster(lasData, resolution = output_res)
dem <- rasterize(lasData@coords[lasData$Classification==2, 1:2 ], rast_template, lasData$Z[lasData$Classification==2], fun=mean)
dem <- focal(dem, w=matrix(1, 7, 7), fun=mean, na.rm=TRUE, NAonly=TRUE)
plot(dem, main = "Digital Elevation Model")
frs <- rasterize(lasData@coords[lasData$ReturnNumber==1, 1:2 ], rast_template, lasData$Z[lasData$ReturnNumber==1], fun=max)
frs <- focal(frs, w=matrix(1, 7, 7), fun=mean, na.rm=TRUE, NAonly=TRUE)
plot(frs, main = "First Return Surface")
chm <- frs - dem
plot(chm, main = "Canopy Height Model")
lasData <- normalize(lasData)
lasData <- classifyVegByHeight(lasData)
lasStatLayers <- pointStatistics(lasData, output_res)
percentileLayers <- heightPercentiles(lasData, output_res)
vdRatioLayers <- vertDistRatio(lasData, output_res)
canCover <- canopyCover(lasData, output_res)
canDensity <- canopyDensity(lasData, output_res)
pointCountLayers <- heightPointCounts(lasData, output_res)
pointPercentLayers <- heightPointPercents(pointCountLayers, output_res)
