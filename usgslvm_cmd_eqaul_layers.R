#------------------------------------------------------------------------------
# Name:       usgslvm_cmd_equal_layers.R
# Purpose:    make all input rasters have same number of layers.
# Arguments:  1) text file containing paths to raster files to be altered
#             Each path on new line. 2) number of desired layers. If left blank
#             number of layers set to maximum from input raster list.
# Notes:      File paths can be relative or absolute dependings on your command
#             line directory location.

library(raster)
library(tools)

args <- commandArgs(trailingOnly = T)

# get list of files text file
file_list <- scan(args[1], what = character(), quiet = T)

# if band count arg was left blank, find highest number of layers from files
# else, set to argument
if (is.na(args[2])){
  bnd_cnt <- max(unlist(lapply(file_list, FUN = function(x){nlayers(stack(x))})))
} else {
  bnd_cnt <- as.numeric(args[2])
}

# for each tile, if the number of bands is less than bnd_cnt, add empty layers
# until number equals bnd_cnt
for (f in file_list){
  if (nlayers(stack(f)) < bnd_cnt){
    s <- readAll(stack(f))
    while (nlayers(s) < bnd_cnt){
      s <- addLayer(s, raster(s, vals = NA))
    }
    writeRaster(s, f, overwrite = T)
  }
}
print("complete.")
