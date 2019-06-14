#'Read LiDAR point data
#'
#'@description This function reads and returns values associated with the LAS file format. The LAS file is a public file format for the interchange of LiDAR 3-dimensional point cloud data (American Society of Photogrammetry and Remote Sensing - ASPRS)
#'
#'@usage readLidarData (LASfile, inputCRS, maxHAG = 70, returnAll=FALSE)
#'
#'@param LASfile A standard LAS or LAZ data file (ASPRS)
#'@param proj4string to be interpreted as a CRS by SP pacakge.
#'@param Logical. Default FALSE. By default, only returns Classification 0, 1, & 2.
#'@return Returns a Spatial Points class of the point information stored in the LAS file.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@export
#'@importFrom rlas read.las
#'@importFrom sp coordinates proj4string CRS

readLidarData <- function(x, inputCRS, returnAll = FALSE){

  f <- rlas::read.las(x, select = "xyzicr")

  if (returnAll==F) {
    f <- f[f$Classification %in% c(1, 2, 3, 4, 5, 8, 19), ]
  }

  sp::coordinates(f) <- ~X+Y
  sp::proj4string(f) <- sp::CRS(inputCRS)
  return(f)
}
