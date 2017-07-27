# -------------------------------------------------------------------------
"""Print basic information of a las file."""
# Name:         lidar_info.py
# Purpose:      Print basic information of a las file.
#
# Author:       Nicholas Kruskamp
#               nfkruska@ncsu.edu
#               Center for Geospatial Analytics
#               North Carolina State University
#
# Ex Input:     C:/Temp/myFile.las True True
# Updated:      2017.06.06
# Licence:      Open Source or something.
# -------------------------------------------------------------------------

import os
import sys

import laspy
import numpy


# Classificaiton dictionaries for ASPRS LAS 1.4 and 1.2 spec.
# If the version is 1.0 - 1.3, it will use the 1.2 spec.
# spec: http://www.asprs.org/wp-content/uploads/2010/12/LAS_1_4_r13.pdf
# LAS 1.2: table 9, LAS 1.4: table 17
# Often used informal classications:
# 10 (water edge), 17 (low overlap),
# 18 (high overlap), 25 (water edge overlap)
globClassDict12 = {0: '(0) Created, never classified', 1: '(1) Unclassified',
                   2: '(2) Ground', 3: '(3) Low Vegetation',
                   4: '(4) Medium Vegetation', 5: '(5) High Vegetation',
                   6: '(6) Building', 7: '(7) Low Point',
                   8: '(8) Model Key-point', 9: '(9) Water',
                   10: '(10) Reserved', 11: '(11) Reserved',
                   12: '(12) Overlap Points'}
globClassDict14 = {0: '(0) Created, never classified', 1: '(1) Unclassified',
                   2: '(2) Ground', 3: '(3) Low Vegetation',
                   4: '(4) Medium Vegetation', 5: '(5) High Vegetation',
                   6: '(6) Building', 7: '(7) Low Point', 8: '(8) Reserved',
                   9: '(9) Water', 10: '(10) Rail', 11: '(11) Road Surface',
                   12: '(12) Reserved', 13: '(13) Wire - Guard',
                   14: '(14) Wire - Conductor', 15: '(15) Transmission Tower',
                   16: '(16) Wire-structure Connector', 17: '(17) Bridge Deck',
                   18: '(18) High Noise'}
for x in range(13, 32):
    globClassDict12[x] = '({}) Reserved'.format(x)
for x in range(19, 256):
    if x < 64:
        globClassDict14[x] = '({}) Reserved'.format(x)
    else:
        globClassDict14[x] = '({}) User definable'.format(x)


def lasFileInfo(inFilePath, writeToFile=False, printToStdOpt=True):
    """Print out LAS header and basic point record info."""
    print "\nGetting data from LAS file..."

    # open las file for read only
    lasFile = laspy.file.File(inFilePath, mode="r")

    # create a header object
    hdr = lasFile.header

    # create array of raw classification values
    rawClasses = lasFile.classification

    # get point format to access specs & names
    pointFormat = lasFile.point_format

    # set return number counters
    onlyReturns = 0
    firstReturns = 0
    intReturns = 0
    lastReturns = 0

    # create array from return number and number of returns
    returnStack = numpy.vstack((lasFile.return_num, lasFile.num_returns))

    # count the number of points for each return type
    for i in range(1, 5):
        for j in range(1, 5):
            if j < i:
                continue
            elif i == 1 and j == 1:
                # pep8/flake8 flags these as errors but numpy still
                # requires this syntax for where type clauses.
                returns = numpy.logical_and(returnStack[0] == i,
                                            returnStack[1] == j)
                onlyReturns += returns[returns == True].size
            elif i == 1 and j > i:
                returns = numpy.logical_and(returnStack[0] == i,
                                            returnStack[1] == j)
                firstReturns += returns[returns == True].size
            elif i > 1 and i == j:
                returns = numpy.logical_and(returnStack[0] == i,
                                            returnStack[1] == j)
                lastReturns += returns[returns == True].size
            else:
                returns = numpy.logical_and(returnStack[0] == i,
                                            returnStack[1] == j)
                intReturns += returns[returns == True].size

    # adjust values to match accepted standards
    firstReturns += onlyReturns
    lastReturns += onlyReturns

    # get the count of unique raw classifications
    rawClassUnique = numpy.unique(rawClasses, return_index=False,
                                  return_inverse=False, return_counts=True)

    # split the input file path for directory for output text file
    fileName = os.path.splitext(os.path.basename(inFilePath))

    # print header information
    hdrStr = "LAS FILE HEADER\n\n{:30}{}\n{:30}{}\n{:30}{}\n{:30}{}\n{:30}{} \
            \n{:30}{}\n{:30}{}\n{:30}{}\n{:30}{}\n{:30}{}\n{:30}{}\n{:30}{} \
            \n{:30}{}\n{:30}{}\n{:30}{}\n{:30}{}\n{:30}{}\n{:30}{}\n{:30}{} \
            \n{:30}{}".format("Input LAS File", fileName[0],
                              "File signature", hdr.file_signature,
                              "File source ID", hdr.filesource_id,
                              "Global encoding", hdr.global_encoding,
                              "Project ID GUID data 1-4", hdr.project_id,
                              "Version major.minor", hdr.version,
                              "System identifier", hdr.system_id,
                              "Generating software", hdr.software_id,
                              "File creation date", hdr.date,
                              "Header size", hdr.header_size,
                              "Offset to point data", hdr.data_offset,
                              "Number var. length records", len(hdr.vlrs),
                              "Point data format", hdr.dataformat_id,
                              "Point data record length",
                              hdr.data_record_length,
                              "Number of point records",
                              "{:,}".format(hdr.count),
                              "Number of points by return",
                              " ".join("{:,}".format(x) for x
                                       in hdr.point_return_count),
                              "Scale factor (x y z)",
                              " ".join("{:,}".format(x) for x in hdr.scale),
                              "Offset (x y z)",
                              " ".join("{:,}".format(x) for x in hdr.offset),
                              "Max values (X Y Z)",
                              " ".join("{:,}".format(x) for x in hdr.max),
                              "Min values (X Y Z)",
                              " ".join("{:,}".format(x) for x in hdr.min))

    # print point summary
    hdrStr += "\n\nPOINT RECORD SUMMARY"

    # calculate the point density. Currently cannot fetch unit from
    # LAS file. Using the 'unit' squared until I can get that figured
    # out.
    fileArea = (hdr.max[0] - hdr.min[0]) * (hdr.max[1] - hdr.min[1])
    pulseDen = firstReturns / fileArea
    pointDen = hdr.count / fileArea
    hdrStr += "\n\n{:30}{:.2f}".format("File Area", fileArea)
    hdrStr += "\n{:30}{:.2f} p/unit^2".format("Point Density", pointDen)
    hdrStr += "\n{:30}{:.2f} p/unit^2".format("Pulse Density", pulseDen)

    # print the names of all included record fields
    hdrStr += "\n\n{:30}".format("Point record fields")

    for count, spec in enumerate(pointFormat):
        if count == 0:
            hdrStr += spec.name
        else:
            hdrStr += "\n{:30}{} ".format(" ", spec.name)

    # print return types
    hdrStr += "\n\n{:30}{:30}{:,}\n{:30}{:30}{:,}\n{:30}{:30}{:,} \
    \n{:30}{:30}{:,}\n\n{:30}".format("Return Types", "First returns",
                                      firstReturns, " ", "Int returns",
                                      intReturns, " ", "Last returns",
                                      lastReturns, " ", "Single returns",
                                      onlyReturns, "Point counts by class")

    # try to match LAS version to ASPRS spec. Not fully developed to match
    # all possible spec versions. Assumes anything not 1.4 to be 1.2.
    if hdr.version == '1.4':
        for count, rc in enumerate(rawClassUnique[0]):
            if count == 0:
                hdrStr += "{:30}{:,}".format(globClassDict14[rc],
                                             rawClassUnique[1][count])
            else:
                hdrStr += \
                         "\n{:30}{:30}{:,}".format(" ",
                                                   globClassDict14[rc],
                                                   rawClassUnique[1][count])
    else:
        for count, rc in enumerate(rawClassUnique[0]):
            if count == 0:
                hdrStr += \
                         "{:30}{:,}".format(globClassDict12[rc],
                                            rawClassUnique[1][count])
            else:
                hdrStr += \
                         "\n{:30}{:30}{:,}".format(" ",
                                                   globClassDict12[rc],
                                                   rawClassUnique[1][count])

    hdrStr += "\n\nVARIABLE LENGTH RECORDS"

    for count, x in enumerate(hdr.vlrs):
        count += 1
        hdrStr += "\n\nVLR # {:,}".format(count)
        # print x.reserved
        hdrStr += "\n{}".format(x.user_id)
        # print x.record_id
        # print x.rec_len_after_header
        hdrStr += "\n{}".format(x.description)
        hdrStr += "\n{}".format(x.VLR_body)

    if printToStdOpt:
        print hdrStr

    if writeToFile:
        print "\n\nprinting to file {}".format(fileName[0] + "_info.txt")
        dirPath = os.path.dirname(inFilePath)

        hdrFilePath = os.path.join(dirPath, fileName[0] + "_info.txt")
        hdrFile = open(hdrFilePath, "w")
        hdrFile.write(hdrStr)
        hdrFile.close()

    lasFile.close()


def str2Bool(s):
    """Convert strings to booleans."""
    if type(s) is bool:
        return s
    elif s.lower() == "false":
        return False
    elif s.lower() == "true":
        return True


if __name__ == '__main__':
    """Print LAS file information."""

    try:
        inLASPath = sys.argv[1]
        print "{:30}{}".format("Input LAS File:", inLASPath)
    except:
        print "please provide full file path for .LAS file."
        sys.exit()
    try:
        write2file = str2Bool(sys.argv[2])
        print "{:30}{}".format("print to text file:", write2file)
    except:
        write2file = False
        print "{:30}{} (default value)".format("print to text file:",
                                               write2file)
    try:
        print2StdOpt = str2Bool(sys.argv[3])
        print "{:30}{}".format("print to stdOut:", print2StdOpt)
    except:
        print2StdOpt = True
        print "{:30}{} (default value)".format("print to stdOut:",
                                               print2StdOpt)

    lasFileInfo(inLASPath, write2file, print2StdOpt)
