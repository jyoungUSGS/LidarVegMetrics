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

calcCanopyCover <- function(x, resolution = 30, pointClasses = c(100:200)) {
  r <- raster::raster(x, resolution = resolution)
  x <- x[x$ReturnNumber==1, ]
  allRast <- raster::rasterize(x@coords, r, x$Z, fun='count')
  x <- x[x$Classification %in% pointClasses, ]
  vegRast <- raster::rasterize(x@coords, r, x$Z, fun='count')
  canCovRast <- vegRast / allRast * 100
  return(canCovRast)
}
