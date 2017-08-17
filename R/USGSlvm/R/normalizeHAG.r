#'Normalize Z Values
#'
#'@description This function returns the height above ground level for each non ground point. The function finds the nearest ground classified point in X,Y space, and computes the Z different for each non ground point.
#'
#'@usage normalizeHAG (lasdata)
#'
#'@param lasdata, a Spatial Points Data Frame containing X, Y, Z coordinates and Classification data.
#'@return returns the spatial points data frame with additional column "Z above ground level (Z_agl)" value.
#'@author Nicholas Kruskamp
#'@examples
#'
#' # dem surface raster is interpolated using mean of ground classified values, with default grid cell of 30 units.
#' lasdata <- normalize(lasdata)
#'
#'@export
#'@importFrom FNN knnx.index

normalizeHAG <- function(x) {
  g_knn <- FNN::knnx.index(x[x$Classification==2,1:2], x[x$Classification %in% c(0,1),1:2], k=1)
  ground_z <- x$Z[g_knn]
  x$Z_agl[x$Classification %in% c(0,1)] <- x$Z[x$Classification %in% c(0,1)] - ground_z
  return(x)
}
