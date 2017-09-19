library(devtools)
library(roxygen2)


document("C:/Users/nfkruska/Documents/GitHub/LidarVegMetrics/R/USGSlvm")
install("C:/Users/nfkruska/Documents/GitHub/LidarVegMetrics/R/USGSlvm")



delete_results <-function(){
  file.remove(list.files(pattern = ".tif", recursive = T))
}

# EPSG 6350 CONUS ALBERS METERS