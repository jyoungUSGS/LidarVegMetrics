#'create bare eath digital elevation model (DEM)
#'
#'@description This function reads and returns values associated with the LAS file format. The LAS file is a public file format for the interchange of LiDAR 3-dimensional point cloud data (American Society of Photogrammetry and Remote Sensing - ASPRS)
#'
#'@usage createDSM (SpatialPointsDataFrame, res = 10)
#'
#'@param A SpatialPointsDataFrame containing lidar data, at minimum X, Y, and Z values.
#'@param resolution Number specifying the grid cell resolution of the output raster.
#'@return Returns a raster of the dem elevation values.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@export
#'@importFrom raster raster xyFromCell ncell setValues
#'@importFrom sp coordinates
#'@importFrom gstat variogram fit.variogram krige

createDEM <- function(x, res = 10){
  r <- raster::raster(x, resolution = res)
  lp <- x[x$Classification==2, 1]
  lp_s <- lp[sample(nrow(lp), 5000),]
  r_vgm <- gstat::variogram(Z~1, lp_s)
  r_fit <- gstat::fit.variogram(r_vgm, model=vgm("Gau"))
  plot(r_vgm, r_fit)
  r_xyz <- data.frame(raster::xyFromCell(r, 1:raster::ncell(r)))
  r_xyz$Z <- NA
  sp::coordinates(r_xyz) <- ~x+y
  sp::proj4string(r_xyz) <- x@proj4string
  r_krig <- gstat::krige(Z~1, lp_s, r_xyz, model=r_fit)
  r <- raster::setValues(r, r_krig$var1.pred)
  return(r)
}
