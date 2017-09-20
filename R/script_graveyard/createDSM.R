#'create first return digital surface model (DSM)
#'
#'@description This function performs krige interpolation on all lidar first return to produce a digital surface model.
#'
#'@usage createDSM (SpatialPointsDataFrame, res = 10)
#'
#'@param A SpatialPointsDataFrame containing lidar data, at minimum X, Y, and Z values.
#'@param resolution Number specifying the grid cell resolution of the output raster.
#'@return Returns a raster of the dsm elevation values.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@export
#'@importFrom raster raster xyFromCell ncell setValues
#'@importFrom sp coordinates
#'@importFrom gstat variogram fit.variogram krige

createDSM <- function(x, res = 10){
  r <- raster::raster(x, resolution = res)
  lp <- x[x$ReturnNumber==1, 1]
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
