#'Flags noise points in lidar data.
#'
#'@description This function identifies potential outlier/noise points using a k-nearest neighbor approach, where if a given point does not have "nn" neighbors within "threshold" distance, then it is an outlier/noise.
#'
#'@usage knnOutlier(x, threshold = 3, nn=7)
#'
#'@param x Spatial Points Data Frame containing X, Y, Z coordinates and Classification data.
#'@param threshold Number specifying the euclidian distance from a given point in which to identify neighborss
#'@param pointClasses Number of neighbors a point should have to not be considered an outlier/noise.
#'@return in-place processing of the Spatial Points Data Frame, adding or appending a 'classFlagWithheld' column.
#'@author Nicholas Kruskamp
#'@examples
#'
#'@export
#'@importFrom FNN knn.dist

flagNoiseKnn <- function(x, threshold = 3, nn=7){
  knndist <- knn.dist(x@coords, nn)
  nn_list <- apply(knndist, 1, function(x,y=threshold,...){length(which(x<y))})
  x$Classification[nn_list<nn] <- 7
  x$nn <- NULL
  return(x)
}
