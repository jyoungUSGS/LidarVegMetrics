library(devtools)
library(roxygen2)


clear_update <- function(){
  document("C:/Users/nfkruska/Documents/GitHub/LidarVegMetrics/R/USGSlvm")
  install("C:/Users/nfkruska/Documents/GitHub/LidarVegMetrics/R/USGSlvm")
  delete_results()
}


delete_results <-function(){
  file.remove(list.files(pattern = ".tif", recursive = T))
}

# EPSG 6350 CONUS ALBERS METERS

devtools::use_package("raster", pkg = "C:/Users/nfkruska/Documents/R/win-library/packages/raster")
devtools::use_package("rgdal", pkg = "C:/Users/nfkruska/Documents/R/win-library/packages/rgdal")
devtools::use_package("rlas", pkg = "C:/Users/nfkruska/Documents/R/win-library/packages/rlas")
devtools::use_package("sp", pkg = "C:/Users/nfkruska/Documents/R/win-library/packages/sp")
devtools::use_package("tools", pkg = "C:/Program Files/Microsoft/R Open/R-3.4.1/library/tools")