#'normalize values by height above group from a digital elevation model.
#'
#'@description This function finds the difference between a non-ground point Z elevation and the coincident cell value Z in X Y space.
#'
#'@usage normalizeByDEM(x, y)
#'
#'@param A SpatialPointsDataFrame containing lidar data, at minimum X, Y, and Z values.
#'@param A raster dem.
#'@return Returns the SpatialPointsDataFrame with an additional vector of normalized Z values.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@importFrom raster raster xyFromCell ncell setValues
#'@importFrom sp coordinates
#'@importFrom gstat variogram fit.variogram krige

normalizeByDEM <- function(x, y){
  Z_g <- extract(y, x@coords[,1:2])
  x$Z_agl <- x$Z - Z_g
  return(x)
}
