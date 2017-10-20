mosaicList <- function(rast_path, overlapping = "mean"){

# rast_path = the full path and name with file extension of all the rasters we want to mosaic
# overlapping param indicates what function to use for the cells in overlapping areas, in case 
# there is one: The functions accepted are: mean and sum for now .... other functions
# can be added later if necessary

require(raster)

raster.list <- list() # initialise the list of rasters

    for (i in 1:(length(rast_path))){ 
       # rast_path contains all full path and the names of the rasters
      raster.list[[i]] <- raster(rast_path[i])
	  NAvalue(raster.list[[i]]) <- 0
	  }
	  
  # edit settings of the raster list for use in do.call and mosaic
  names(raster.list) <- NULL
  #####This function deals with overlapping areas
  
  if (overlapping == "mean") raster.list$fun <- mean else raster.list$fun <- sum
    raster.list$na.rm <- TRUE
  
  #run do call to implement mosaic over the list of raster objects.
  mos <- do.call(mosaic, raster.list)

  #set crs of output : in a normal world the input crs should be good so the output 
  # should be good too, but in our case it is not
  crs(mos) <- crs(raster.list[[1]])
  return(mos)
}

