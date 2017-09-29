#'Calculate a canopy height model
#'
#'@description This function calculates a canopy height model as the difference between the first return surface and a bare earth dem.
#'
#'@usage canopyHeightModel(x, resolution = 30, pointClasses = c(3,4,5))
#'
#'@param x Spatial Points Data Frame containing X, Y, Z coordinates and Classification data.
#'@param resolution Number specifying the grid cell resolution of the output raster.
#'@param pointClasses List of numbers specifying the point classifications to subset the lidar data.
#'@return returns a raster of the canopy height model
#'@author Nicholas Kruskamp
#'@examples
#'
#'@importFrom raster raster rasterize focal

canopyHeightModel <- function(x, resolution = 30){
  tile_raster <- raster(x, resolution = output_res)
  dem <- rasterize(x[x$Classification==2, 1:2 ], tile_raster, x$Z[x$Classification==2], fun=mean)
  dem <- focal(dem, w=matrix(1, 7, 7), fun=mean, na.rm=TRUE, NAonly=TRUE)
  frs <- rasterize(x[x$ReturnNumber==1, 1:2 ], tile_raster, x$Z[x$ReturnNumber==1], fun=max)
  frs <- focal(frs, w=matrix(1, 7, 7), fun=mean, na.rm=TRUE, NAonly=TRUE)
  chm <- frs - dem
  return(chm)

}
