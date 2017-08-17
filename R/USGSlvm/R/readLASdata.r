#'Read LiDAR point data
#'
#'@description This function reads and returns values associated with the LAS file format. The LAS file is a public file format for the interchange of LiDAR 3-dimensional point cloud data (American Society of Photogrammetry and Remote Sensing - ASPRS)
#'
#'@usage readLASdata (LASfile, short=TRUE)
#'
#'@param LASfile A standard LAS data file (ASPRS)
#'@param short Logical, if TRUE it will return only a 5-column matrix with information on the returned point x, y, z locations, point intensity and the number of return within an individual discrete-return system laser pulse.
#'@param String contains the exact proj4string of the input data to project the resulting spatial points data frame.
#'@return Returns a Spatial Points class of the point information stored in the LAS file.
#'@author Original Script: rLiDAR package by Michael Sumner and Carlos Alberto Silva. https://cran.r-project.org/package=rLiDAR Modified by Nicholas Kruskamp to meet (most of) LAS 1.4 spec and output a Spatial Points class.
#'@examples
#'
#'@export
#'@importFrom bitops bitAnd bitShiftR
#'@importFrom sp coordinates proj4string CRS
readLASdata<- function(LASfile, short=TRUE, projection=NA) {

  if (class(short)!="logical") {
    stop("The short parameter is invalid. It must to be a TRUE or FALSE logical statement")
  }

  pheader <- readLASheader(LASfile)

  if (pheader[["Point Data Record Format"]] > 06) {
    print(paste0("point data record format: ", pheader[["Point Data Record Format"]]))
    stop("This poing data record format is not currently supported.")
  }

  LegacyNumberPointRecords <- pheader[["Legacy Number of point records"]]
  if (LegacyNumberPointRecords < 1){
    numberPointRecords <- pheader[["Number of point records"]]
  } else{
    numberPointRecords <- LegacyNumberPointRecords
  }
  if (numberPointRecords < 1) {
    stop("No point data found in file.")
  }

  offsetToPointData <- pheader[["Offset to point data"]]
  pointDataRecordLength <-pheader[["Point Data Record Length"]]
  xyzScaleOffset <- cbind(unlist(pheader[c("X scale factor", "Y scale factor", "Z scale factor")]),
                          unlist(pheader[c("X offset", "Y offset", "Z offset")]))

  con <- file(LASfile, open = "rb")
  seek(con, where = offsetToPointData)

  allbytes <- matrix(readBin(con, "raw", n = pointDataRecordLength * numberPointRecords,
                             size = 1, endian = "little"), ncol= pointDataRecordLength,
                             nrow = numberPointRecords, byrow = TRUE)

  close(con)

  xyz <- matrix(readBin(t(allbytes[,1:(3*4)]), "integer", size = 4, n = 3 * numberPointRecords, endian = "little"), ncol = 3, byrow = TRUE)
  xyz[,1] <- xyz[ ,1] * xyzScaleOffset[1,1] + xyzScaleOffset[1, 2]
  xyz[,2] <- xyz[ ,2] * xyzScaleOffset[2,1] + xyzScaleOffset[2, 2]
  xyz[,3] <- xyz[ ,3] * xyzScaleOffset[3,1] + xyzScaleOffset[3, 2]
  colnames(xyz) <- c("X", "Y", "Z")

  Intensity <- readBin(t(allbytes[, 13:14]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

  if (short==TRUE) {
    if(pheader[17][[1]] <= 05) {
      ReturnNumber <- bitops::bitAnd(7, allbytes[,15])
      data <- data.frame(cbind(xyz, Intensity, ReturnNumber))
    } else {
      ReturnNumber <- bitops::bitAnd(15, allbytes[,15])
      data <- data.frame(cbind(xyz, Intensity, ReturnNumber))
    }
  } else {
      if (pheader[17][[1]]==00) {
        ReturnNumber <- bitops::bitAnd(7, allbytes[,15])
        NumberOfReturns <- bitops::bitShiftR(bitops::bitAnd(56, allbytes[,15]), 3)
        ScanDirectionFlag <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 64), 6)
        EdgeofFlightLine <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 128), 7)
        Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        ScanAngleRank <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
        UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

        data <- data.frame(cbind(xyz, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,
                     EdgeofFlightLine,Classification,ScanAngleRank,UserData,PointSourceID))
      }

      if (pheader[17][[1]]==01) {
        ReturnNumber <- bitops::bitAnd(7, allbytes[,15])
        NumberOfReturns <- bitops::bitShiftR(bitops::bitAnd(56, allbytes[,15]), 3)
        ScanDirectionFlag <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 64), 6)
        EdgeofFlightLine <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 128), 7)
        Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        ScanAngleRank <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
        UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
        gpstime <- NULL

        if (ncol(allbytes) == 28){
          gpstime <- readBin(t(allbytes[ , 21:28]), "numeric", size = 8, n = numberPointRecords, endian = "little")
        }

        data <- data.frame(cbind(xyz, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,
                    EdgeofFlightLine,Classification,ScanAngleRank,UserData,PointSourceID,
                    gpstime))
        }

      if (pheader[17][[1]]==02) {
        ReturnNumber <- bitops::bitAnd(7, allbytes[,15])
        NumberOfReturns <- bitops::bitShiftR(bitops::bitAnd(56, allbytes[,15]), 3)
        ScanDirectionFlag <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 64), 6)
        EdgeofFlightLine <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 128), 7)
        Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")

        ScanAngleRank <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
        UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

        R <- readBin(t(allbytes[, 21:22]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
        G <- readBin(t(allbytes[, 23:24]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
        B <- readBin(t(allbytes[, 25:26]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

        data <- data.frame(cbind(xyz, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,
                     EdgeofFlightLine,Classification,ScanAngleRank,UserData,PointSourceID,
                     R,G,B))
      }

      if (pheader[17][[1]]==03) {

        ReturnNumber <- bitops::bitAnd(7, allbytes[,15])
        NumberOfReturns <- bitops::bitShiftR(bitops::bitAnd(56, allbytes[,15]), 3)
        ScanDirectionFlag <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 64), 6)
        EdgeofFlightLine <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 128), 7)
        Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")

        ScanAngleRank <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
        UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

        gpstime <- NULL
        gpstime <- readBin(t(allbytes[ , 21:28]), "numeric", size = 8, n = numberPointRecords, endian = "little")

        R <- readBin(t(allbytes[, 29:30]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
        G <- readBin(t(allbytes[, 31:32]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
        B <- readBin(t(allbytes[, 33:34]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

        data <- data.frame(cbind(xyz, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,
                     EdgeofFlightLine,Classification,ScanAngleRank,UserData,PointSourceID,
                     R,G,B))
      }

      if (pheader[17][[1]]==04) {
        ReturnNumber <- bitops::bitAnd(7, allbytes[,15])
        NumberOfReturns <- bitops::bitShiftR(bitops::bitAnd(56, allbytes[,15]), 3)
        ScanDirectionFlag <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 64), 6)
        EdgeofFlightLine <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 128), 7)
        Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")

        ScanAngleRank <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
        UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

        gpstime <- NULL
        gpstime <- readBin(t(allbytes[ , 21:28]), "numeric", size = 8, n = numberPointRecords, endian = "little")

        WavePacket_Descriptor_Index <- readBin(t(allbytes[, 29]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        Byte_offset_to_waveform_data <- readBin(t(allbytes[, 30:37]), "integer", size = 8, n = numberPointRecords, signed = FALSE, endian = "little")
        Waveform_packet_size_in_bytes <- readBin(t(allbytes[, 38:41]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
        Return_Point_Waveform_Location<- readBin(t(allbytes[, 42:45]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
        X.t<- readBin(t(allbytes[, 46:49]), "intege", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
        Y.t<- readBin(t(allbytes[, 50:53]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
        Z.t<- readBin(t(allbytes[, 54:57]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")

        data <- data.frame(cbind(xyz, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,
                     EdgeofFlightLine, Classification, ScanAngleRank, UserData, PointSourceID,
                     gpstime, WavePacket_Descriptor_Index, Byte_offset_to_waveform_data,
                     Waveform_packet_size_in_bytes, Return_Point_Waveform_Location,
                     X.t, Y.t, Z.t))
      }

      if (pheader[17][[1]]==05) {
        ReturnNumber <- bitops::bitAnd(7, allbytes[,15])
        NumberOfReturns <- bitops::bitShiftR(bitops::bitAnd(56, allbytes[,15]), 3)
        ScanDirectionFlag <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 64), 6)
        EdgeofFlightLine <- bitops::bitShiftR(bitops::bitAnd(allbytes[,15], 128), 7)
        Classification <- readBin(t(allbytes[, 16]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")

        ScanAngleRank <-readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = TRUE, endian = "little")
        UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        PointSourceID <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

        gpstime <- NULL
        gpstime <- readBin(t(allbytes[ , 21:28]), "numeric", size = 8, n = numberPointRecords, endian = "little")

        R <- readBin(t(allbytes[, 29:30]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
        G <- readBin(t(allbytes[, 31:32]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
        B <- readBin(t(allbytes[, 33:34]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")

        WavePacket_Descriptor_Index <- readBin(t(allbytes[, 35]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        Byte_offset_to_waveform_data <- readBin(t(allbytes[, 36:43]), "integer", size = 8, n = numberPointRecords, signed = FALSE, endian = "little")
        Waveform_packet_size_in_bytes <- readBin(t(allbytes[, 44:47]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
        Return_Point_Waveform_Location<- readBin(t(allbytes[, 48:51]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
        X.t<- readBin(t(allbytes[, 52:55]), "intege", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
        Y.t<- readBin(t(allbytes[, 56:59]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")
        Z.t<- readBin(t(allbytes[, 60:63]), "integer", size = 4, n = numberPointRecords, signed = FALSE, endian = "little")

        data <- data.frame(cbind(xyz, Intensity, ReturnNumber,NumberOfReturns,ScanDirectionFlag,
                     EdgeofFlightLine,Classification,ScanAngleRank,UserData,PointSourceID,
                     gpstime, R, G, B, WavePacket_Descriptor_Index,Byte_offset_to_waveform_data,
                     Waveform_packet_size_in_bytes, Return_Point_Waveform_Location,X.t,Y.t,Z.t))
      }

      if (pheader[17][[1]]==06) {

        ReturnNumber <- bitops::bitAnd(15, allbytes[,15])
        NumberOfReturns <- bitops::bitShiftR(bitops::bitAnd(240, allbytes[,15]), 4)
        classFlagsSynthetic <- bitops::bitAnd(1, allbytes[,16])
        classFlagKeyPoint <-bitops::bitShiftR(bitops::bitAnd(2, allbytes[,16]), 1)
        classFlagWithheld <-bitops::bitShiftR(bitops::bitAnd(4, allbytes[,16]), 2)
        classFlagOverlap <- bitops::bitShiftR(bitops::bitAnd(8, allbytes[,16]), 3)
        ScannerChannel <- bitops::bitShiftR(bitops::bitAnd(48, allbytes[,16]), 4)
        ScanDirectionFlag <- bitops::bitShiftR(bitops::bitAnd(64, allbytes[,16]), 6)
        EdgeofFlightLine <- bitops::bitShiftR(bitops::bitAnd(128, allbytes[,16]), 7)
        Classification <- readBin(t(allbytes[, 17]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        UserData <-readBin(t(allbytes[, 18]), "integer", size = 1, n = numberPointRecords, signed = FALSE, endian = "little")
        ScanAngle <-readBin(t(allbytes[, 19:20]), "integer", size = 2, n = numberPointRecords, signed = TRUE, endian = "little")
        # need to correct for backwards compatibility?
        # see spec explanation of scan angle rank
        scanConversion <- 0.006
        ScanAngleRank <- round(ScanAngle * scanConversion)

        PointSourceID <-readBin(t(allbytes[, 21:22]), "integer", size = 2, n = numberPointRecords, signed = FALSE, endian = "little")
        gpstime <- readBin(t(allbytes[ , 23:30]), "numeric", size = 8, n = numberPointRecords, endian = "little")

        data <- data.frame(cbind(xyz, Intensity, ReturnNumber,NumberOfReturns, classFlagsSynthetic,
                       classFlagKeyPoint, classFlagWithheld, classFlagOverlap,
                       ScannerChannel, ScanDirectionFlag, EdgeofFlightLine, Classification,
                       UserData, ScanAngleRank, PointSourceID, gpstime))
      }
    }
  sp::coordinates(data) <- c("X", "Y" ,"Z")
  if (!is.na(projection)){
    sp::proj4string(data) <- sp::CRS(projection)
  }

  # Return a slightly modified spdf with the functionality of a data.table
  # data@data <- as.data.table(data@data)
  return(data)
}
