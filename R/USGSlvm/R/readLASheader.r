#'Read LiDAR header
#'
#'@description This function reads and returns values associated with the LAS file format header. The LAS file is a public file format for the interchange of LiDAR 3-dimensional point cloud data (American Society of Photogrammetry and Remote Sensing - ASPRS)
#'
#'@usage readLASheader (LASfile)
#'
#'@param LASfile A standard LAS data file (ASPRS)
#'@return Returns a list of the information stored in the LAS file header.
#'@author Original Script: rLiDAR package by Michael Sumner and Carlos Alberto Silva. https://cran.r-project.org/package=rLiDAR Modified by Nicholas Kruskamp to read and output the header information.
#'@examples
#'
#'@export
readLASheader<- function(LASfile) {

  con <- file(LASfile, open = "rb")
  isLASFbytes <- readBin(con, "raw", size = 1, n = 4, endian = "little")
  isLASFbytes <- rawToChar(isLASFbytes)

  if (isLASFbytes != "LASF") {
    stop("The LASfile input is not a valid LAS file")
  }

  seek(con, where = 94)

  publicHeaderDescription <- function() {
    hd <- structure(list(Item = c("File Signature (\"LASF\")",
                                  "File Source ID", "Global Encoding",
                                  "Project ID - GUID data 1", "Project ID - GUID data 2",
                                  "Project ID - GUID data 3", "Project ID - GUID data 4",
                                  "Version Major", "Version Minor", "System Identifier",
                                  "Generating Software", "File Creation Day of Year",
                                  "File Creation Year", "Header Size", "Offset to point data",
                                  "Number of variable length records",
                                  "Point Data Record Format", "Point Data Record Length",
                                  "Legacy Number of point records", "Legacy Number of points by return",
                                  "X scale factor", "Y scale factor", "Z scale factor", "X offset",
                                  "Y offset", "Z offset", "Max X", "Min X", "Max Y", "Min Y", "Max Z",
                                  "Min Z", "Start of Waveform Data Packet Record", "Start of first EVLR",
                                  "Number of EVLR", "Number of point records", "Number of points by return"),
                        Format = c("char[4]", "unsigned short", "unsigned short",
                                   "unsigned long", "unsigned short", "unsigned short",
                                   "unsigned char [8]", "unsigned char", "unsigned char",
                                   "char [32]", "char [32]", "unsigned short",
                                   "unsigned short", "unsigned short", "unsigned long",
                                   "unsigned long", "unsigned char", "unsigned short",
                                   "unsigned long", "unsigned long [5]", "double",
                                   "double", "double", "double",
                                   "double", "double", "double",
                                   "double", "double", "double",
                                   "double", "double", "unsigned long long",
                                   "unsigned long long", "unsigned long", "unsigned long",
                                   "unsigned long long [15]"),
                      Size = c("4 bytes", "2 bytes", "2 bytes", "4 bytes", "2 byte",
                               "2 byte", "8 bytes", "1 byte", "1 byte", "32 bytes",
                               "32 bytes", "2 bytes", "2 bytes", "2 bytes", "4 bytes",
                               "4 bytes", "1 byte", "2 bytes", "4 bytes", "20 bytes",
                               "8 bytes", "8 bytes", "8 bytes", "8 bytes", "8 bytes",
                               "8 bytes", "8 bytes", "8 bytes", "8 bytes", "8 bytes",
                               "8 bytes", "8 bytes", "8 bytes", "8 bytes", "4 bytes",
                               "8 bytes", "120 bytes"),
                        Required = c("*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*",
                                   "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*", "*",
                                   "*", "*", "*", "*", "*", "*", "*", "*", "*", "*")),
                         .Names = c("Item", "Format", "Size", "Required"), row.names = 1:37, class = "data.frame")
    hd$what <- ""
    hd$what[grep("unsigned", hd$Format)] <- "integer"
    hd$what[grep("char", hd$Format)] <- "raw"
    hd$what[grep("short", hd$Format)] <- "integer"
    hd$what[grep("long", hd$Format)] <- "integer"
    hd$what[grep("double", hd$Format)] <- "numeric"
    hd$signed <- TRUE
    hd$signed[grep("unsigned", hd$Format)] <- FALSE
    hd$n <- as.numeric(gsub("[[:alpha:][:punct:]]", "", hd$Format))
    hd$n[hd$what == "character"] <- 1
    hd$n[is.na(hd$n)] <- 1
    hd$Hsize <- as.numeric(gsub("[[:alpha:]]", "", hd$Size))
    hd$Rsize <- hd$Hsize / hd$n
    hd$Rsize[hd$what == "raw"] <- 1
    hd$n[hd$what == "raw"] <- hd$Hsize[hd$what == "raw"]
    hd
  }

  hd <- publicHeaderDescription()
  pheader <- vector("list", nrow(hd))
  names(pheader) <- hd$Item

  seek(con, where = 94)
  headerSize <- readBin(con, what = hd$what[14], signed = hd$signed, size = hd$Rsize[14], endian = "little", n = hd$n[14])

  seek(con, where = 0)

  for (i in 1:nrow(hd)) {
    if (seek(con) >= headerSize){
      break
    }
    pheader[[hd$Item[i]]] <- readBin(con, what = hd$what[i], signed = hd$signed, size = hd$Rsize[i], endian = "little", n = hd$n[i])
  }


  for (i in c(1, 10, 11)){
    pheader[[hd$Item[i]]] <- rawToChar(pheader[[hd$Item[i]]])
  }

  for (i in c(8, 9, 17)){
    pheader[[hd$Item[i]]] <- as.integer(pheader[[hd$Item[i]]])
  }

  close(con)
  return(pheader)
}
