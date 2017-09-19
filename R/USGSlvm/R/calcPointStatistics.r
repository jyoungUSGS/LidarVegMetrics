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

calcPointStatistics <- function(x, resolution = 30, pointClasses = c(100:200)){
  r <- raster::raster(x, resolution = resolution)
  x <- x[x$Classification %in% pointClasses, ]

  qMean <- function(x, ...){
      sqrt(mean(x ^ 2))
  }

  minRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun = min)
  maxRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun = max)
  meanRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun = mean)
  sdRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun = sd)
  skewRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun = moments::skewness)
  kurtRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun = moments::kurtosis)
  qMeanRast <- raster::rasterize(x@coords, r, field = x$Z_agl, fun = qMean)

  layerList <- list(minRast, maxRast, meanRast, sdRast, skewRast, kurtRast, qMeanRast)
  names(layerList) <- c("hmin", "hmax", "havg", "hstd", "hske", "hkur", "hqav")
  return(layerList)
}
