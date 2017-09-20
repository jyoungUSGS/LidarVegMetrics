#'Read LiDAR vlr
#'
#'@description This function reads and returns values associated with the LAS file Variable Length Records (vlr). The LAS file is a public file format for the interchange of LiDAR 3-dimensional point cloud data (American Society of Photogrammetry and Remote Sensing - ASPRS). This tool does not currently support Extended vlr.
#'
#'@usage readLASvlr (LASfile)
#'
#'@param LASfile A standard LAS data file (ASPRS)
#'@return Returns a list of lists with vlr information
#'@author Original Script: rLiDAR package by Michael Sumner and Carlos Alberto Silva. https://cran.r-project.org/package=rLiDAR Modified by Nicholas Kruskamp to read vlr.
#'@examples
#'

readLASvlr_bin<- function(LASfile) {

  pheader <- readLASheader(LASfile)

  vd <- structure(list(Item = c("Reserved", "User ID", "Record ID",
                                "Record Lenth After Header", "Description", "Record"),
                      Format = c("unsigned short", "char [16]", "unsigned short",
                                 "unsigned short", "char [32]", "char"),
                    Size = c("2 bytes", "16 bytes", "2 bytes", "2 bytes", "32 byte", "Variable"),
                      Required = c(" ", "*", "*", "*", " ", "*")),
                       .Names = c("Item", "Format", "Size", "Required"), row.names = 1:6, class = "data.frame")
  vd$what <- ""
  vd$what[grep("unsigned", vd$Format)] <- "integer"
  vd$what[grep("char", vd$Format)] <- "raw"
  vd$what[grep("short", vd$Format)] <- "integer"
  vd$what[grep("long", vd$Format)] <- "integer"
  vd$what[grep("double", vd$Format)] <- "numeric"
  vd$signed <- TRUE
  vd$signed[grep("unsigned", vd$Format)] <- FALSE
  vd$n <- as.numeric(gsub("[[:alpha:][:punct:]]", "", vd$Format))
  vd$n[vd$what == "character"] <- 1
  vd$n[is.na(vd$n)] <- 1
  vd$Hsize <- as.numeric(gsub("[[:alpha:]]", "", vd$Size))
  vd$Rsize <- vd$Hsize / vd$n
  vd$Rsize[vd$what == "raw"] <- 1
  vd$n[vd$what == "raw"] <- vd$Hsize[vd$what == "raw"]
  vd

  vlr <- vector("list", nrow(vd))
  names(vlr) <- vd$Item

  headerSize <-pheader[["Header Size"]]
  numVLR <- pheader[["Number of variable length records"]]
  startEVLR <- pheader[["Start of first EVLR"]]
  numEVLR <- pheader[["Number of EVLR"]]
  totVLRS <- sum(numVLR, numEVLR, na.rm = T)
  vlrs <- rep(list(vlr), totVLRS)

  con <- file(LASfile, open = "rb")

  if (numVLR > 0) {
    seek(con, where = headerSize)
    for (j in 1:numVLR) {
      for (i in 1:nrow(vd)) {
        if (i %in% c(2,5)) {
          vlrs[[j]][[vd$Item[i]]] <- readBin(con, what = vd$what[i], signed = vd$signed, size = vd$Rsize[i], endian = "little", n = vd$n[i])
          vlrs[[j]][[vd$Item[i]]] <- rawToChar(vlrs[[j]][[vd$Item[i]]])
        } else if (i == 6) {
          vlrs[[j]][[vd$Item[i]]] <- readBin(con, what = "raw", signed = vd$signed, size = 1, endian = "little", n = vlrs[[j]][[vd$Item[4]]])
          try(vlrs[[j]][[vd$Item[i]]] <- rawToChar(vlrs[[j]][[vd$Item[i]]]), silent = TRUE)
        } else {
          vlrs[[j]][[vd$Item[i]]] <- readBin(con, what = vd$what[i], signed = vd$signed, size = vd$Rsize[i], endian = "little", n = vd$n[i])
        }
      }
    }
  }

  close(con)
  return(vlrs)
}
