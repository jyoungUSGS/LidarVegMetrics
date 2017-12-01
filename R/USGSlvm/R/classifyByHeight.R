#'Classify lidar points by height above ground level.
#'
#'@description This function will take normalized Z elevation values and reassign the classification.
#'
#'@usage classifyByHeight(x, binHeight = NA, binCount = 3)
#'
#'@param x Spatial Points Data Frame containing X, Y, Z coordinates and Classification data.
#'@param BinHeight value to set a fixed height for classification. If not set, binCount is used.
#'@param BinCount value to evenly divide the canopy into classes. Default is 3 layers/classes.
#'@return returns the spatial points data frame with appended classification column.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@export

classifyByHeight <- function(x, binHeight = 5, binCount = NA){

    maxAgl <- max(x$Z_agl[x$Classification == 1], na.rm=TRUE)

    if (is.na(binHeight)) {
      binHeight <- maxAgl / binCount
    } else {
      binCount <- ceiling(maxAgl / binHeight)
    }

    vegClass <- 101

    bottom <- 0
    for (i in 1:binCount){
      bottom <- bottom
      top <- bottom + binHeight
      x$Classification[bottom <= x$Z_agl & x$Z_agl < top & x$Classification == 1] <- vegClass

      bottom <- top
      vegClass <- vegClass + 1
    }
    return(x)
  }
