#'Read LiDAR point data
#'
#'@description This function reads and returns values associated with the LAS file format. The LAS file is a public file format for the interchange of LiDAR 3-dimensional point cloud data (American Society of Photogrammetry and Remote Sensing - ASPRS)
#'
#'@usage readLidarData (LASfile, inputCRS, maxHAG = 70, returnAll=FALSE)
#'
#'@param LASfile A standard LAS data file (ASPRS)
#'@param proj4string to be interpreted as a CRS by SP pacakge.
#'@param Logical. Default FALSE. By default, only returns Classification 0, 1, & 2.
#'@return Returns a Spatial Points class of the point information stored in the LAS file.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@export
#'@importFrom rlas readlasdata
#'@importFrom sp coordinates proj4string CRS

readLidarData <- function(x, inputCRS, maxHAG = 70, returnAll = FALSE){

  x <- rlas::readlasdata(x, F, T, T, F, F, T, T, F, F, F)

  if (returnAll==F) {
    x <- x[x$Classification %in% c(1, 2, 3, 4, 5, 8, 19), ]
  }

  x <- x[x$Z < maxHAG, ]
  
  sp::coordinates(x) <- ~X+Y
  sp::proj4string(x) <- sp::CRS(inputCRS)
  return(x)
}
