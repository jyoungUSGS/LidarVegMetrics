#'Calculate Lidar Points percentages in Height Layers
#'
#'@description This function the point percentages above ground level from lidar data. The value is calculated as the number of points within a cell divided by the sum of all cells in the raster stack column.
#'
#'@usage calcHeightPointPercents(x, resolution = 30, binHeight = NA, binCount = 3, pointClasses = c(3,4,5))
#'
#'@param x Spatial Points Data Frame containing X, Y, Z coordinates and Classification data or Raster Stack containing point counts.
#'@param resolution Number specifying the grid cell resolution of the output raster.
#'@param BinHeight value to set a fixed height for count layer. If not set, binCount is used.
#'@param BinCount value to evenly divide the canopy into count layers. Default is 3 layers.
#'@param pointClasses List of numbers specifying the point classifications to subset the lidar data.
#'@return a raster stack where each layer represents the point percentages at that height bin.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@export

calcHeightPointPercents <- function(x, outputDir, tileName, resolution = 30, binHeight = NA, binCount = 3, pointClasses = c(100:200)){
  if (class(x) == "SpatialPointsDataFrame"){
    pcLayer <- heightPointCounts(x, resolution = resolution, binHeight, binCount, pointClasses)
  } else {
    pcLayer <- x
  }
  pcSumLayer <- sum(pcLayer, na.rm=T)
  percentLayer <- pcLayer / pcSumLayer * 100

  prod <- "hcntpct"
  outputFile <- file.path(outputDir, prod, paste(tileName, paste(prod,".tif", sep = ""), sep = "_"))
  raster::writeRaster(percentLayer, outputFile)
  return(percentLayer)
}
