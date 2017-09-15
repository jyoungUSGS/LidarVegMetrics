#'Calculate basic statistics from lidar point data
#'
#'@description This function calculates statistics from lidar point data, including: maximum, mean, standard deviation, skewness, kurtosis, and quadratic mean.
#'
#'@usage calcPointStatistics(x, resolution = 30, pointClasses = c(3,4,5))
#'
#'@param x Spatial Points Data Frame containing X, Y, Z coordinates and Classification data.
#'@param resolution Number specifying the grid cell resolution of the output raster.
#'@param pointClasses List of numbers specifying the point classifications to subset the lidar data.
#'@return returns a raster stack where each layer is a metric.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@export
#'@importFrom raster raster rasterize
#'@importFrom moments skewness kurtosis

calcPointStatistics <- function(x, outputDir, tileName, resolution = 30,
                                pointClasses = c(100:200)){
  r <- raster::raster(x, resolution = resolution)
  x <- x[x$Classification %in% pointClasses,]

  qMean <- function(x,...){
      sqrt(mean(x^2))
  }

  minRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun=min)
  prod <- "hmin"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod,".tif", sep = ""), sep = "_"))
  raster::writeRaster(minRast, outputFile)

  maxRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun=max)
  prod <- "hmax"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod,".tif", sep = ""), sep = "_"))
  raster::writeRaster(maxRast, outputFile)

  meanRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun=mean)
  prod <- "havg"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod,".tif", sep = ""), sep = "_"))
  raster::writeRaster(meanRast, outputFile)

  sdRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun=sd)
  prod <- "hstd"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod,".tif", sep = ""), sep = "_"))
  raster::writeRaster(sdRast, outputFile)

  skewRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun=moments::skewness)
  prod <- "hske"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod,".tif", sep = ""), sep = "_"))
  raster::writeRaster(skewRast, outputFile)

  kurtRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun=moments::kurtosis)
  prod <- "hkur"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod,".tif", sep = ""), sep = "_"))
  raster::writeRaster(kurtRast, outputFile)

  qMeanRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun=qMean)
  prod <- "hqav"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod,".tif", sep = ""), sep = "_"))
  raster::writeRaster(qMeanRast, outputFile)


  layerList <- list(minRast, maxRast, meanRast, sdRast, skewRast, kurtRast, qMeanRast)
  names(layerList) <- c("min", "max", "mean", "stdev", "skewness", "kurtosis", "quad mean")
  return(layerList)
}
