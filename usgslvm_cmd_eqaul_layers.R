library(raster)
library(tools)

# get file path to files to be merge
file_list <- grep("tif$", list.files("./tiles/25m", "hcnt", full.names = T,
                                     recursive = T), value = T)

# get the maximum number of bands
# this could also be an integer (e.g. 10 bands @ 5 m bins = 50 m)
bnd_cnt <- max(unlist(lapply(file_list, FUN = function(x){nlayers(stack(x))})))

# for each tile, if the number of bands is less than bnd_cnt, add empty layers
# until number equals bnd_cnt
for (f in file_list){
  if (nlayers(stack(f)) < bnd_cnt){
    s <- readAll(stack(f))
    while (raster::nlayers(s) < bnd_cnt){
      r <- raster::raster(s)
      r <- raster::setValues(r, NA)
      s <- raster::addLayer(s, r)
    }
    raster::writeRaster(s, f, overwrite = T)
  }
}

# verify all tiles have same layer count.
# bnd_list <- lapply(file_list, FUN = function(x){nlayers(stack(x))})
# bnd_list




