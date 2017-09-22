#'Count Lidar Points in Height Layers
#'
#'@description This function counts the number of points above ground level from lidar data. It will divide the canopy into even height bins, or by number of bins.
#'
#'@usage calcHeightPointCounts(x, resolution = 30, binHeight = NA, binCount = 3, pointClasses = c(3,4,5))
#'
#'@param x Spatial Points Data Frame containing X, Y, Z coordinates and Classification data.
#'@param resolution Number specifying the grid cell resolution of the output raster.
#'@param BinHeight value to set a fixed height for count layer. If not set, binCount is used.
#'@param BinCount value to evenly divide the canopy into count layers. Default is 3 layers.
#'@param pointClasses List of numbers specifying the point classifications to subset the lidar data.
#'@return a raster stack where each layer is represents the point counts at that height bin.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@export
#'@importFrom raster raster rasterize addLayer

calcHeightPointCounts <- function(x, resolution = 30, binHeight = 5,
                                  binCount = NA, pointClasses = c(100:200)){
  r <- raster::raster(x, resolution = resolution)
  pcStack <- raster::raster(x, resolution = resolution)
  x <- x[x$Classification %in% pointClasses, ]
  maxAgl <- max(x$Z_agl, na.rm = TRUE)
  if (is.na(binHeight)) {
    binHeight <- maxAgl / binCount
  } else {
    binCount <- ceiling(maxAgl / binHeight)
  }

  bottom <- 0
  for (i in 1:binCount){
    bottom <- bottom
    top <- bottom + binHeight
    t <- x[bottom <= x$Z & x$Z < top, "Z_agl"]
    if (nrow(t) == 0 ){
      countRast <- raster::setValues(r, 0)
    } else {
      countRast <- raster::rasterize(t, r, field = t$Z_agl, fun = "count")
    }
    pcStack <- raster::addLayer(pcStack, countRast)
    bottom <- top
  }

  return(pcStack)
}
