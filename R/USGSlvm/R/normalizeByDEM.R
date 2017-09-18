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
#'@importFrom raster raster rasterize focalWeight focal extract

normalizeByDEM <- function(x, y = NA, resolution = 30){
  if (is.na(y)){
    gp <- x[x$Classification == 2, 1]
    r <- raster::raster(x, resolution = resolution)
    r_xy <- as.data.frame(raster::xyFromCell(r, 1:raster::ncell(r)))
    names(r_xy) <- c("X", "Y")
    sp::coordinates(r_xy) <- ~ X + Y
    sp::proj4string(r_xy) <- gp@proj4string
    v <- gstat::idw.spatial(Z~1, gp, r_xy)
    y <- raster::rasterize(v, r, v$v1.pred)
  }

    Z_g <- raster::extract(y, x)
    x$Z_agl <- x$Z - Z_g
    return(x)
}
