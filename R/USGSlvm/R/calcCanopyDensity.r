#'Calculate Canopy Density
#'
#'@description This function calculates the canopy density of an area from lidar data. The canopy cover is calculated as the percentage of vegetation returns to all returns.
#'
#'@usage calcCanopyDensity(x, resolution = 30, pointClasses = c(3,4,5))
#'
#'@param x Spatial Points Data Frame containing X, Y, Z coordinates and Classification data.
#'@param resolution Number specifying the grid cell resolution of the output raster.
#'@param pointClasses List of numbers specifying the point classifications to subset the lidar data.
#'@return a raster layer
#'@author Nicholas Kruskamp
#'@examples
#'
#' canopyDensityRaster <- calcCanopyDensity(lasdata)
#'
#'@export
#'@importFrom raster raster rasterize

calcCanopyDensity <- function(x, resolution = 30, pointClasses = c(100:200)) {
  r <- raster::raster(x, resolution = resolution)
  allRast <- raster::rasterize(x, r, x$Z_agl, fun = "count")
  x <- x[x$Classification %in% pointClasses, ]
  vegRast <- raster::rasterize(x, r, x$Z_agl, fun = "count")
  canDenRast <- vegRast / allRast * 100

  return(canDenRast)
}
