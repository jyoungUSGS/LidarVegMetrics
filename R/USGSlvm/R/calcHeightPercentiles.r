#'Calculate Height Percentiles
#'
#'@description This function calculates percentiles of lidar points.
#'
#'@usage calcHeightPercentiles(x, resolution = 30, percentiles = c(.10, .20, .30, .40, .50, .60, .70, .80, .90, .98), pointClasses = c(3,4,5))
#'
#'@param x Spatial Points Data Frame containing X, Y, Z coordinates and Classification data.
#'@param resolution Number specifying the grid cell resolution of the output raster.
#'@param percentiles a list of numbers specifying the percentiles to be calculated.
#'@param pointClasses List of numbers specifying the point classifications to subset the lidar data.
#'@return returns a raster stack where each layer is a percentile.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@export
#'@importFrom raster raster rasterize addLayer

calcHeightPercentiles <- function(x, resolution = 30, percentiles = c(.10, .20, .30, .40, .50, .60, .70, .80, .90, .98), pointClasses = c(100:200)){
  tile_raster <- raster::raster(x, resolution = resolution)
  perc_stack <- raster::raster(x, resolution = resolution)
  x <- x[x$Classification %in% pointClasses, ]
  for (i in percentiles){
    perc_rast <- raster::rasterize(x@coords[ , 1:2], tile_raster, field = x$Z_agl,
      fun=function(x,...){quantile(x, i, na.rm = T)})
    perc_stack <- raster::addLayer(perc_stack, perc_rast)
  }
  perc_names <- as.character(percentiles*100)
  names(perc_stack) <- perc_names
  return(perc_stack)
}
