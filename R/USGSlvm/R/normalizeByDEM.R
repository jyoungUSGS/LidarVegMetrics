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
  if (class(y) != "RasterLayer"){
    gp <- x[x$Classification == 2, 1]

    r <- raster::raster(x, resolution = resolution)

    dem_mean <- raster::rasterize(gp, r, gp$Z, fun = mean)
    mean_vals <- as.data.frame(raster::xyFromCell(dem_mean,
      1:raster::ncell(dem_mean)))
    names(mean_vals) <- c("x", "y")
    mean_vals$z <- raster::getValues(dem_mean)
    mean_vals <- mean_vals[!(is.na(mean_vals$z)), ]
    sp::coordinates(mean_vals) <- ~ x + y
    sp::proj4string(mean_vals) <- gp@proj4string

    pred_vals <- as.data.frame(raster::xyFromCell(r, 1:raster::ncell(r)))
    names(pred_vals) <- c("x", "y")
    sp::coordinates(pred_vals) <- ~ x + y
    sp::proj4string(pred_vals) <- gp@proj4string

    pred <- gstat::idw(Z~1, gp, pred_vals)
    y <- raster::rasterize(pred, r, pred$var1.pred)
  }

    z_g <- raster::extract(y, x)
    x$Z_agl <- x$Z - z_g
    return(x)
}
