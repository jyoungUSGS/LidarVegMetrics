library(lidarpro)
library(sp)
library(raster)

input_dir <- "C:/Temp"
input_file <- "USGS_LPC_VA_Shenandoah_2013_DO_N16_4873_20_LAS_2015.las"
input_CRS <- "+proj=lcc +lat_1=39.2 +lat_2=38.03333333333333 +lat_0=37.66666666666666 +lon_0=-78.5 +x_0=3500000.0001016 +y_0=2000000.0001016 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
output_CRS <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
output_res <- 10
output_dir <- ""

setwd(input_dir)
lasheader <- readLASheader(input_file)
lasData <- readLASdata(input_file, FALSE, input_CRS)
lasData <- spTransform(lasData, output_CRS)
lasData <- flagNoiseKnn(lasData)
lasData <- lasData[lasData$Classification %in% c(0, 1, 2) & lasData$classFlagWithheld==0 & lasData$classFlagOverlap==1, ]

rast_template <- raster(lasData, resolution = output_res)
dem <- rasterize(lasData@coords[lasData$Classification==2, 1:2 ], rast_template, lasData$Z[lasData$Classification==2], fun=mean)
dem <- focal(dem, w=matrix(1, 7, 7), fun=mean, na.rm=TRUE, NAonly=TRUE)
plot(dem, main = "Digital Elevation Model")
frs <- rasterize(lasData@coords[lasData$ReturnNumber==1, 1:2 ], rast_template, lasData$Z[lasData$ReturnNumber==1], fun=max)
frs <- focal(frs, w=matrix(1, 7, 7), fun=mean, na.rm=TRUE, NAonly=TRUE)
plot(frs, main = "First Return Surface")
chm <- frs - dem
plot(chm, main = "Canopy Height Model")

lasData <- normalize(lasData, dem = dem)
lasData <- classifyVegByHeight(lasData)

lasStatLayers <- pointStatistics(lasData, output_res)
percentileLayers <- heightPercentiles(lasData, output_res)
vdRatioLayers <- vertDistRatio(lasData, output_res)
canCover <- canopyCover(lasData, output_res)
canDensity <- canopyDensity(lasData, output_res)
pointCountLayers <- heightPointCounts(lasData, output_res)
pointPercentLayers <- heightPointPercents(pointCountLayers, output_res)
