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

calcPointStatistics <- function(x, resolution = 30, pointClasses = c(3,4,5)){
  tile_raster <- raster::raster(x, resolution = resolution)
  x <- x[x$Classification %in% pointClasses,]

  qmean <- function(x,...){
      sqrt((sum(x^2)/length(x)))
  }

  max_rast <- raster::rasterize(x@coords[ , 1:2], tile_raster, field = x$Z_agl, fun=max)
  mean_rast <- raster::rasterize(x@coords[ , 1:2], tile_raster, field = x$Z_agl, fun=mean)
  sd_rast <- raster::rasterize(x@coords[ , 1:2], tile_raster, field = x$Z_agl, fun=sd)
  skew_rast <- raster::rasterize(x@coords[ , 1:2], tile_raster, field = x$Z_agl, fun=moments::skewness)
  kurt_rast <- raster::rasterize(x@coords[ , 1:2], tile_raster, field = x$Z_agl, fun=moments::kurtosis)
  qmean_rast <- raster::rasterize(x@coords[ , 1:2], tile_raster, field = x$Z_agl, fun=qmean)

  layer_list <- list(max_rast, mean_rast, sd_rast, skew_rast, kurt_rast, qmean_rast)
  names(layer_list) <- c("max", "mean", "stdev", "skewness", "kurtosis", "quad mean")
  return(layer_list)
}
