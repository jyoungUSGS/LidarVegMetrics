#'normalize values by height above group from a digital elevation model.
#'
#'@description This is a hasty function to find the difference between a non-ground point Z elevation and the coincident cell value Z in X Y space. This quickly computes a dem on the fly be finding the mean ground value by cell. It may contain NA values. It is recommended to use your own DEM for this function.
#'
#'@usage normalizeByDEM(x, y, resolution)
#'
#'@param A SpatialPointsDataFrame containing lidar data, at minimum X, Y, and Z values.
#'@param A raster dem.
#'@param Resolution of DEM if calculated on the fly
#'@return Returns the SpatialPointsDataFrame with an additional vector of normalized Z values.
#'@author Nicholas Kruskamp
#'@examples
#'
#' @export
#'@importFrom raster raster rasterize focal extract

normalizeByDEM <- function(x, y = NA, resolution = 30){
  if (class(y) != "RasterLayer"){
    gp <- x[x$Classification == 2, 1]

    r <- raster::raster(x, resolution = resolution)

    y <- raster::rasterize(gp, r, gp$Z, fun = mean)
    y <- raster::focal(y, w = matrix(1, 7, 7), fun = mean, na.rm = T, pad = T,
      NAonly = T)
  }

    z_g <- raster::extract(y, x)
    x$Z_agl <- x$Z - z_g
    return(x)
}
