#------------------------------------------------------------------------------
# Name: usgslvm_cmd_merge.R
# Purpose: to merge all files from text list into single raster output
# Usage: 2 arguments
#        1) input text file: text file containing paths to raster files
#           to be merged
#        2) path and name of output file with extension.
# 
# Notes: file paths can be relative or absolute dependings on your command line
#        directory location.


library(raster)
library(tools)

args <- commandArgs(trailingOnly = T)

# get file path to files to be merge
file_list <- scan(args[1], what = character(), quiet = T)

# merge files into one mosaic, this could be unwieldy 
# with high number of tiles and layers.
# there is a do.call() that should be prettier than this,
# but I can't get it to work...
print("merging files from list...")
m <- stack(file_list[1])
for (f in file_list){
  r <- stack(f)
  m <- merge(m, r, tolerance = 1)
}
writeRaster(m, args[2], overwrite = T)
print("done.")
