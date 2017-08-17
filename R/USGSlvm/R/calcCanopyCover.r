#'Calculate Canopy Cover
#'
#'@description This function calculates the canopy cover of an area from lidar data. The canopy cover is calculated as the percentage of vegetation first returns to all first returns.
#'
#'@usage calcCanopyCover(x, resolution = 30, pointClasses = c(3,4,5))
#'
#'@param x Spatial Points Data Frame containing X, Y, Z coordinates and Classification data.
#'@param resolution Number specifying the grid cell resolution of the output raster.
#'@param pointClasses List of numbers specifying the point classifications to subset the lidar data.
#'@return a raster layer
#'@author Nicholas Kruskamp
#'@examples
#'
#' canopyCoverRaster <- calcCanopyCover(lasdata)
#'
#'@export
#'@importFrom raster raster rasterize

calcCanopyCover <- function(x, resolution = 30, pointClasses = c(3,4,5)) {
  rast_template <- raster::raster(x, resolution = resolution)
  x <- x[x$ReturnNumber==1, ]
  all_rast <- raster::rasterize(x@coords[, c("X","Y")], rast_template, x$Z, fun='count')
  x <- x[x$Classification %in% pointClasses, ]
  veg_rast <- raster::rasterize(x@coords[, c("X","Y")], rast_template, x$Z, fun='count')
  canCov_rast <- veg_rast / all_rast * 100
  return(canCov_rast)
}
