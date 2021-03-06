% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcHeightPointPercents.r
\name{calcHeightPointPercents}
\alias{calcHeightPointPercents}
\title{Calculate Lidar Points percentages in Height Layers}
\usage{
calcHeightPointPercents(x, resolution = 30, binHeight = NA, binCount = 3, pointClasses = c(3,4,5))
}
\arguments{
\item{x}{Spatial Points Data Frame containing X, Y, Z coordinates and Classification data or Raster Stack containing point counts.}

\item{resolution}{Number specifying the grid cell resolution of the output raster.}

\item{pointClasses}{List of numbers specifying the point classifications to subset the lidar data.}

\item{BinHeight}{value to set a fixed height for count layer. If not set, binCount is used.}

\item{BinCount}{value to evenly divide the canopy into count layers. Default is 3 layers.}
}
\value{
a raster stack where each layer represents the point percentages at that height bin.
}
\description{
This function the point percentages above ground level from lidar data. The value is calculated as the number of points within a cell divided by the sum of all cells in the raster stack column.
}
\examples{

}
\author{
Nicholas Kruskamp
}
