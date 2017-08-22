identifyFlightPolygons <- function(x){
  
  fl_ids <- unique(x$PointSourceID)
  
  poly_list <- list()
  i <- 1
  for (f in fl_ids){
    print(f)
    fl <- lasData[PointSourceID==f]
    fl_hull <- chull(fl)
    fl_hull <- fl[fl_hull, 1:2]
    poly_list[i] <- Polygon(fl_hull)
    i <- i + 1
  }

  polys_list <- list()
  for (i in 1:length(poly_list)){
    polys_list[[i]] <- Polygons(poly_list[i], i)
  }
  
  fl_polys <- SpatialPolygons(polys_list, proj4string = sp::CRS(input_CRS))
  return(fl_polys)

}