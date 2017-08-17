# -------------------------------------------------------------------------
"""Perform lidar processing to create derived forest products."""
# Name:         lidarMets.py
# Purpose:      Utilities to process lidar las files.
#
# Author:       Nicholas Kruskamp
# UnityID:      nfkruska
#
# Ex input:     ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Elevation/
# LPC/Projects/USGS_LPC_VA_Shenandoah_2013_LAS_2015/las/tiled/"
# "5800_10;5800_20;5800_30;5800_40" "C:/Temp/data" "C:/Temp/LidarMets.mxd"
# 10 False True "3;4;5" "30;60;150" True True True True True
#
# Last Edited:  2017.05.01 @ 22:00 EDT
# Copyright:    (c) nfkruska 2017
# Licence:      Open Source or something.
# -------------------------------------------------------------------------


import os
import sys
import time
import urllib2
import zipfile

import arcpy


arcpy.env.overwriteOutput = True

# Classificaiton dictionaries for ASPRS LAS 1.4 and 1.2 spec.
# If the version is 1.0 - 1.3, it will use the 1.2 spec.
# spec: http://www.asprs.org/wp-content/uploads/2010/12/LAS_1_4_r13.pdf
# LAS 1.2: table 9, LAS 1.4: table 17
# ESRI only implements LAS 1.4, will mis-classify pre 1.4 files.
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

# Stable subset of data to count as "all points".
# This avoids the formal & informal classications for:
# 7 (low noise), 9 (water), 10 (water edge), 17 (low overlap),
# 18 (high overlap), 25 (water edge overlap)
globAllPntClass = [0, 1, 2, 3, 4, 5, 6]

# Maximum memory chunk size.
# I have not tinkered with it yet enough to find the upper limit.
# Solution to chunk data found here:
# https://stackoverflow.com/questions/1517616/
# stream-large-binary-files-with-urllib2-to-file
CHUNK = 50 * 1024

# save the sript directory where reference files are located.
scriptPath = os.path.realpath(__file__)
globScriptDir = os.path.dirname(scriptPath)


class arcLASD(object):
    """Class which holds the arcpy methods for lidar processing."""

    def __init__(self, LASFile, lasd, cellSize, wDir):
        """Initialize the arcLASD class."""
        self.LASFile = os.path.join(wDir, LASFile)
        self.lasd = lasd
        self.cellSize = cellSize
        if not os.path.exists(wDir):
            os.makedirs(wDir)
        arcpy.env.workspace = wDir
        self.createLASD()

    def createLASD(self):
        """Create a ESRI las dataset if one does not already exist."""
        start = time.time()
        try:
            if not arcpy.Exists(self.lasd):
                arcpy.CheckOutExtension('3D')
                printArc("\n\tCreating LasDataset {}.".format(self.lasd),)
                arcpy.management.CreateLasDataset(self.LASFile, self.lasd, '#',
                                                  '#', '#', 'COMPUTE_STATS')
                printArc("\tDone. Complete in {:.2f} seconds."
                         .format(time.time() - start))
            elif arcpy.Describe(self.lasd).dataType != "LasDataset":
                printArc("\tInput file is not an ESRI LAS Dataset")
                sys.exit()
            arcpy.CheckInExtension('3D')

        except arcpy.ExecuteError:
            printArc(arcpy.GetMessages())

    def reclassify(self, classList, destClass=0):
        """Reclassify lidar points."""
        printArc("\n\tExecuting reclassification.",)
        start = time.time()
        try:
            arcpy.CheckOutExtension('3D')
            if classList[0] == '#':
                printArc("{:30}{}".format("Reclassifying value(s)", "ALL"))
                printArc("{:30}{}".format("To this value", destClass))
                classList = [[0, destClass]]
                for x in range(1, 32):
                    classList.append([x, destClass])
            else:
                oldValues = []
                newValues = []
                for i in range(0, len(classList)):
                    oldValues.append(classList[i][0])
                    newValues.append(classList[i][1])
                printArc("{:30}{}".format("Old value(s)", oldValues))
                printArc("{:30}{}".format("New value(s)", newValues))
            arcpy.ChangeLasClassCodes_3d(self.lasd, classList, 'COMPUTE_STATS')
            arcpy.CheckInExtension('3D')
            printArc("\tDone. Complete in {:.2f} seconds."
                     .format(time.time() - start))

        except arcpy.ExecuteError:
            printArc(arcpy.GetMessages())

    def groundClassify(self, demName="beDem", classPoints=False, makeDEM=True):
        """Classify the ground surface points with two iterations."""
        printArc("\n\tExecuting ground processing...",)
        start = time.time()
        try:
            arcpy.CheckOutExtension('3D')
            if classPoints:
                printArc("\tClassifying ground points...")
                # two step method from ESRI example 2:
                # http://desktop.arcgis.com/en/arcmap/latest/tools/
                # 3d-analyst-toolbox/classify-las-ground.htm
                arcpy.ddd.ClassifyLasGround(self.lasd, method="Conservative")
                arcpy.ddd.ClassifyLasGround(self.lasd, method="Aggressive",
                                            reuse_ground="REUSE_GROUND")
            if makeDEM:
                groundClass = 2
                temp_lasd = self.createTempLASD(groundClass, '#')
                printArc("\tCreating {}...".format(demName),)
                arcpy.LasDatasetToRaster_conversion(temp_lasd, demName, '#',
                                                    '#', '#', 'CELLSIZE',
                                                    self.cellSize, '#')
            arcpy.CheckInExtension('3D')
            printArc("\tDone. Complete in {:.2f} seconds."
                     .format(time.time() - start))

        except arcpy.ExecuteError:
            printArc(arcpy.GetMessages())

    def buildingClassify(self, height, area):  # Only for arc 1.5.x
        """Classify buildings."""
        # only works with ArcMap 1.5.x
        printArc("\n\tExcecuting building classification...",)

        start = time.time()
        try:
            arcpy.CheckOutExtension('3D')
            arcpy.ClassifyLasBuilding_3d(self.lasd, minHeight=height,
                                         minArea=area, compute_stats=True)
            arcpy.CheckInExtension('3D')
            printArc("\tDone. Complete in {:.2f} seconds."
                     .format(time.time() - start))

        except arcpy.ExecuteError:
            printArc(arcpy.GetMessages())

    def heightClassify(self, heightList):
        """Classify lidar points by height from list."""
        printArc("\n\tExcecuting height classification...",)
        start = time.time()
        try:
            arcpy.CheckOutExtension('3D')
            arcpy.ddd.ClassifyLasByHeight(self.lasd, ground_source='GROUND',
                                          height_classification=heightList,
                                          noise='ALL_NOISE',
                                          compute_stats="COMPUTE_STATS")
            arcpy.CheckInExtension('3D')
            printArc("\tDone. Complete in {:.2f} seconds."
                     .format(time.time() - start))

        except arcpy.ExecuteError:
            printArc(arcpy.GetMessages())

    def firstRetSurface(self, rastName="frs", classList=globAllPntClass):
        """Create a first return surface raster."""
        printArc("\n\tExcecuting first return surface raster...",)
        start = time.time()
        try:
            temp_lasd = self.createTempLASD('#', '1')
            printArc("\tCreating {}...".format(rastName),)
            arcpy.LasDatasetToRaster_conversion(temp_lasd, rastName, '#',
                                                '#', '#', 'CELLSIZE',
                                                self.cellSize, '#')
            printArc("\tDone. Complete in {:.2f} seconds."
                     .format(time.time() - start))

        except arcpy.ExecuteError:
            printArc(arcpy.GetMessages())

    def canCovDen(self, classList, canDenName="canDen", canCovName="canCov"):
        """Caculate the canopy cover and density."""
        printArc("\n\tExcecuting canopy cover and density rasters...",)
        start = time.time()
        try:
            arcpy.CheckOutExtension('Spatial')
            # create point count raster sums for all and first return.
            allSum = self.pntCntToRast(globAllPntClass)
            all1R = self.pntCntToRast(globAllPntClass, 1)
            rastList = []
            rastList1R = []
            # create a piont count raster for each value in class list.
            for x in classList:
                pntCntRast = self.pntCntToRast(x)
                rastList.append(pntCntRast * 1.0)
                pntCntRast1R = self.pntCntToRast(x, 1)
                rastList1R.append(pntCntRast1R * 1.0)
            # sum only vegetation points.
            vegPCSum = sum(rastList)
            veg1RSum = sum(rastList1R)
            # calculate the density and cover surfaces.
            canDen = (vegPCSum * 1.0) / (allSum * 1.0) * 100.0
            canCov = (veg1RSum * 1.0) / (all1R * 1.0) * 100.0
            # save the results.
            printArc("\tCreating {}...".format(canDenName),)
            canDen.save(canDenName)
            printArc("\tCreating {}...".format(canCovName),)
            canCov.save(canCovName)
            arcpy.CheckInExtension('Spatial')
            printArc("\tDone. Complete in {:.2f} seconds."
                     .format(time.time() - start))

        except arcpy.ExecuteError:
            printArc(arcpy.GetMessages())

    def canHM(self, chmName="chm", classList=globAllPntClass):
        """Calculate the canopy height model."""
        printArc("\n\tExcecuting canopy height model...",)
        start = time.time()
        try:
            arcpy.CheckOutExtension('Spatial')
            # create first return raster
            firstRast = 'in_memory/frs'
            temp_lasd = self.createTempLASD('#', '1')
            arcpy.LasDatasetToRaster_conversion(temp_lasd, firstRast, '#',
                                                '#', '#', 'CELLSIZE',
                                                self.cellSize, '#')
            # create bare earth dem raster
            groundRast = "in_memory/grnd"
            groundClass = 2
            temp_lasd = self.createTempLASD(groundClass)
            arcpy.LasDatasetToRaster_conversion(temp_lasd, groundRast, '#',
                                                '#', '#', 'CELLSIZE',
                                                self.cellSize, '#')
            # calculate canopy height model
            chm = arcpy.Raster(firstRast) - arcpy.Raster(groundRast)
            # set values less than 0 to 0
            chma = arcpy.sa.Con(chm < 0, 0, chm)
            printArc("\tCreating {}...".format(chmName),)
            chma.save(chmName)
            arcpy.CheckInExtension('Spatial')
            printArc("\tDone. Complete in {:.2f} seconds."
                     .format(time.time() - start))

        except arcpy.ExecuteError:
            printArc(arcpy.GetMessages())

    def pntCntToCompRast(self, classList, percRast="perc",
                         rwctRast="raw"):
        """Calculate point counts/ratios and create Composite raster."""
        printArc("\n\tExcecuting Point class count to composite raster...",)
        start = time.time()
        rastList = []
        percList = []
        try:
            arcpy.CheckOutExtension('Spatial')
            # create raster for each class value.
            for x in classList:
                pntCntRast = self.pntCntToRast(x)
                rastList.append(pntCntRast * 1.0)
            # create a total sum raster of all classes.
            vegPCSum = sum(rastList)
            # calculate the point class percentages.
            for rast in rastList:
                perc = ((rast * 1.0) / vegPCSum * 1.0) * 100.0
                percList.append(perc)
            # check sum to make sure all percentages are adding to 100%.
            chkSum = sum(percList)
            # add check sum and total sum as last band for outputs.
            percList.append(chkSum * 1.0)
            rastList.append(vegPCSum * 1.0)
            # Create the percentages composite raster.
            printArc("\tCreating {}...".format(percRast),)
            arcpy.CompositeBands_management(percList, percRast)
            # Create raw count compisite raster.
            printArc("\tCreating {}...".format(rwctRast),)
            arcpy.CompositeBands_management(rastList, rwctRast)
            arcpy.CheckInExtension('Spatial')
        except arcpy.ExecuteError:
            printArc(arcpy.GetMessages())
        printArc("\tDone. Complete in {:.2f} seconds."
                 .format(time.time() - start))

    def pntCntToRast(self, retClass='#', retNum='#'):
        """Create a subset of las points and output to raster."""
        """This will create subsets by point classification or return number.
        If both args are left blank, return raster will count
        all points within a cell."""
        try:
            arcpy.CheckOutExtension('Spatial')
            temp_lasd = self.createTempLASD(retClass, retNum)
            pcrName = 'in_memory/pcrast{}{}'.format(retClass, retNum)
            arcpy.management.LasPointStatsAsRaster(temp_lasd, pcrName,
                                                   "POINT_COUNT",
                                                   "CELLSIZE", self.cellSize)
            pcrName = arcpy.Raster(pcrName)
            pcrNameA = arcpy.sa.Con(arcpy.sa.IsNull(pcrName), 0, pcrName)
            return pcrNameA
        except arcpy.ExecuteError:
            printArc(arcpy.GetMessages())

    def createTempLASD(self, retClass='#', retNum='#'):
        """Create a temporary las dataset by class or return number."""
        temp_lasd = 'temp_lasd.lasd'
        arcpy.management.MakeLasDatasetLayer(self.lasd, temp_lasd,
                                             retClass, retNum)
        return temp_lasd

    def delIfExist(self, fileName):
        """Delete a file if it already exists."""
        if arcpy.Exists(fileName):
            arcpy.Delete_management(fileName)


def saveBinary(localDir, url, lasList):
    """Fetch all binary files from url and save to local directory."""
    """Adapted from Dr. Laura Tateosian, GIS 540, class excercize:
    WakeURL.py. It has been altered to bulk download zip files from
    a given URL."""
    """This tool was specifically designed to work with the USGS 3DEP
    FTP site. Example: ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/
    Elevation/LPC/Projects/USGS_LPC_VA_Shenandoah_2013_LAS_2015/las/tiled/"""
    # read html from url
    response = urllib2.urlopen(url)
    html = response.read()
    response.close()
    zipList = []
    sizeList = []
    totalSize = 0
    lineList = html.split('\n')
    # for each line in html, find the zip file link. Add it to the zipList.
    for line in lineList:
        values = line.split(' ')
        try:
            totalSize += float(values[16])
            sizeList.append(float(values[16]))
        except:
            "skipping file. somethin's screwy."
        fileName = values[-1].strip('\r')
        zipList.append(fileName)
    totalSize = totalSize/1073741824
    printArc("\nDownloading zip files from: {}".format(url))
    printArc("{} zip files found. Approximate total size: {:.2f} GB\n"
             .format(len(zipList), totalSize))

    # for each file in the zipList, see if it matches desired download file.
    for count, z in enumerate(zipList):
        if any(uid in z for uid in lasList):
            binPath = os.path.join(url, z)
            basename = os.path.basename(binPath)
            fullOutName = os.path.join(localDir, basename)
            printArc("\tdownloading #{} {}...".format(count, basename))
            printArc("\t\tapproximate file size: {}".format(sizeList[count]))
            if os.path.exists(fullOutName):
                printArc("\t\tSkipping, file already exists.")
                continue
            response = urllib2.urlopen(binPath)
            # save the file if it is in the download list.
            with open(fullOutName, "wb") as f:
                while True:
                    chunk = response.read(CHUNK)
                    if not chunk:
                        break
                    f.write(chunk)
            response.close()
    printArc("Downloads complete.\n")


def unzip(zipDir, destDir):
    """Unzip all compressed filesvinto the destination directory."""
    """Adapted from Dr. Laura Tateosian, GIS 540, class excercize:
    WakeURL.py. It has been altered to unzip all zip files from
    a given directory."""
    printArc("Starting extraction of LAS files.\n")
    zipList = [z for z in os.listdir(zipDir) if z.endswith('.zip')]
    for z in zipList:
        zPath = os.path.join(zipDir, z)
        with zipfile.ZipFile(zPath, 'r') as myZip:
            for filename in myZip.namelist():
                if filename.endswith('.las'):
                    printArc("\tExtracting {}...".format(filename))
                    fileDir = os.path.join(destDir, filename[:-4])
                    filePath = os.path.join(fileDir, filename)
                    with myZip.open(filename) as zf:
                        if os.path.exists(fileDir):
                            if os.path.exists(filePath):
                                printArc("\t\tSkipping, file already exists.")
                                continue
                        else:
                            os.makedirs(fileDir)
                        with open(os.path.join(fileDir, filename), 'wb') as f:
                            while True:
                                chunk = zf.read(CHUNK)
                                if not chunk:
                                    break
                                f.write(chunk)
    printArc("Extractions complete.\n")


def printArc(message):
    """Print a message to StdOut and arcmap."""
    """Adapted from Dr. Laura Tateosian, GIS 540, class slides:
    arcScriptTools.pptx"""
    print message
    arcpy.AddMessage(message)


def addToMap(wDir, mapName, layerDir):
    """Add layers to a map by name/type."""
    symDenCov = os.path.join(globScriptDir, 'DenCov.lyr')
    symChmFrsDem = os.path.join(globScriptDir, 'DemFrsChm.lyr')
    symComps = os.path.join(globScriptDir, 'comps.lyr')
    printArc("\n\tAdding data to {}.".format(mapName))
    mxd = arcpy.mapping.MapDocument(mapName)
    mxd.relativePaths = True
    # get a list of all data frames, get the first frame.
    dfs = arcpy.mapping.ListDataFrames(mxd)
    df = dfs[0]
    # create a list of all rasters to be added to map doc.
    rastList = [r for r in os.listdir(layerDir) if arcpy.Describe(
        os.path.join(layerDir, r)).dataType == 'RasterDataset']
    # create a list of all group layers.
    targetLyrsList = [l for l in arcpy.mapping.ListLayers(
        mxd) if l.isGroupLayer is True]
    # create a list of all non-group layers
    existLyrList = [l.dataSource for l in arcpy.mapping.ListLayers(
        mxd) if l.isGroupLayer is False]

    for r in rastList:
        rPath = os.path.join(layerDir, r)
        layerObj = arcpy.mapping.Layer(rPath)
        # if the raster is already in map doc, skip it.
        if layerObj.dataSource in existLyrList:
            continue
        # identify rasters by name, add to the appropriate group layer.
        if 'pc' in layerObj.name and len(layerObj.name) < 10:
            # Unable to find way to changer renderer from single band stretch
            # to RGB composite.
            arcpy.ApplySymbologyFromLayer_management(layerObj, symComps)
            arcpy.mapping.AddLayerToGroup(df, targetLyrsList[0], layerObj)
        elif 'rc' in layerObj.name and len(layerObj.name) < 10:
            arcpy.ApplySymbologyFromLayer_management(layerObj, symComps)
            arcpy.mapping.AddLayerToGroup(df, targetLyrsList[1], layerObj)
        elif 'cov' in layerObj.name:
            arcpy.ApplySymbologyFromLayer_management(layerObj, symDenCov)
            arcpy.mapping.AddLayerToGroup(df, targetLyrsList[2], layerObj)
        elif 'den' in layerObj.name:
            arcpy.ApplySymbologyFromLayer_management(layerObj, symDenCov)
            arcpy.mapping.AddLayerToGroup(df, targetLyrsList[3], layerObj)
        elif 'chm' in layerObj.name:
            arcpy.ApplySymbologyFromLayer_management(layerObj, symChmFrsDem)
            arcpy.mapping.AddLayerToGroup(df, targetLyrsList[4], layerObj)
        elif 'frs' in layerObj.name:
            arcpy.ApplySymbologyFromLayer_management(layerObj, symChmFrsDem)
            arcpy.mapping.AddLayerToGroup(df, targetLyrsList[5], layerObj)
        elif 'dem' in layerObj.name:
            arcpy.ApplySymbologyFromLayer_management(layerObj, symChmFrsDem)
            arcpy.mapping.AddLayerToGroup(df, targetLyrsList[6], layerObj)

    # The below should zoom to full extent according to this web page.
    # http://desktop.arcgis.com/en/arcmap/10.3/analyze/arcpy-mapping/
    # dataframe-class.htm. "...if no features are selected,
    # it will zoom to the full extent of all layers."
    df.zoomToSelectedFeatures()

    # save the map doc and delete object to release.
    mxd.save()
    del mxd


def removeAllLayers(wDir, mapName):
    """Remove all layers from an existing map document."""
    mapName = os.path.join(wDir, mapName)
    mxd = arcpy.mapping.MapDocument(mapName)
    mxd.relativePaths = True
    dfs = arcpy.mapping.ListDataFrames(mxd)
    for df in dfs:
        lyrList = arcpy.mapping.ListLayers(mxd, "", df)
        for l in lyrList:
            if l.isGroupLayer:
                continue
            arcpy.mapping.RemoveLayer(df, l)
    mxd.save()
    del mxd


def str2Bool(s):
    """Convert strings to booleans."""
    if type(s) is bool:
        return s
    elif s.lower() == "false":
        return False
    elif s.lower() == "true":
        return True


if __name__ == '__main__':
    """Procss lidar data with utilites from Python and ESRI."""
    printArc("Lidar Processing with Arcpy.\n")
    start = time.time()

    '''USER INPUTS'''
    try:
        inUrl = sys.argv[1]
        inDwnList = sys.argv[2]
        inZipDir = sys.argv[3]
        inMapDocName = sys.argv[4]
        inCellSize = float(sys.argv[5])
        inClassGround = str2Bool(sys.argv[6])
        inClassVeg = str2Bool(sys.argv[7])
        inVegClassList = sys.argv[8]
        inHeightList = sys.argv[9]
        inCreateDEM = str2Bool(sys.argv[10])
        inCreateFRS = str2Bool(sys.argv[11])
        inCreateCHM = str2Bool(sys.argv[12])
        inCreateRGB = str2Bool(sys.argv[13])
        inCreateDenCov = str2Bool(sys.argv[14])

        inDemName = 'dem'
        inFrsName = 'frs'
        inPercRastName = 'pc'
        inRcRastName = 'rc'
        inDenName = 'den'
        inCovName = 'cov'
        inChmName = 'chm'

    except IndexError:
        printArc("Using default values.")

        # "ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged/Elevation/LPC/
        # Projects/USGS_LPC_VA_Shenandoah_2013_LAS_2015/las/tiled/"

        inUrl = 'ftp://rockyftp.cr.usgs.gov/vdelivery/Datasets/Staged' + \
            '/Elevation/LPC/Projects/USGS_LPC_VA_Shenandoah_2013_LAS_2015/' + \
            'las/tiled/'
        inDwnList = "5800_10;5800_20;5800_30;5800_40"
        inZipDir = 'C:/Temp/data'
        inMapDocName = 'C:/Temp/LidarMets.mxd'
        inCellSize = 10
        inClassGround = False
        inClassVeg = True
        inVegClassList = '3;4;5'
        inHeightList = '15;30;45'
        inCreateDEM = True
        inCreateFRS = True
        inCreateCHM = True
        inCreateRGB = True
        inCreateDenCov = True
        inDemName = 'dem'
        inFrsName = 'frs'
        inPercRastName = 'pc'
        inRcRastName = 'rc'
        inDenName = 'den'
        inCovName = 'cov'
        inChmName = 'chm'

    '''INPUT PROCESSING'''

    vegClassList = [int(i) for i in inVegClassList.split(';')]
    heightList = [int(i) for i in inHeightList.split(';')]
    DwnList = [str(i) for i in inDwnList.split(";")]
    classHeightList = [list(a) for a in zip(vegClassList, heightList)]

    if inMapDocName.lower() is not 'current':
        inWD = os.path.join(os.path.dirname(inMapDocName), "data")
    else:
        inWD = inZipDir

    '''WARNINGS IF NEEDED'''

    '''PROCESSING'''

    # removeAllLayers(inWD, inMapDocName)

    saveBinary(inZipDir, inUrl, DwnList)
    unzip(inZipDir, inWD)

    for root, dirs, files in os.walk(inWD, topdown=False):
        for f in files:
            if f.endswith('.las'):
                printArc("\nProcessing {}".format(f))

                # Example USGS file name.
                # USGS_LPC_VA_Shenandoah_2013_DO_N16_4799_20_LAS_2015.las
                UID = f[35:42]

                outLASD = UID + '.lasd'
                lasd1 = arcLASD(f, outLASD, inCellSize, root)

                if inClassGround or inCreateDEM:
                    '''classify ground points using two step method.'''
                    UIDDemName = UID + inDemName
                    lasd1.groundClassify(UIDDemName, inClassGround,
                                         inCreateDEM)

                if inClassVeg:
                    '''classify vegetation points.'''
                    lasd1.heightClassify(classHeightList)

                if inCreateFRS:
                    '''Create a first return surface.'''
                    UIDFrsName = UID + inFrsName
                    lasd1.firstRetSurface(UIDFrsName)

                if inCreateCHM:
                    '''create a canopy height model.'''
                    UIDChmName = UID + inChmName
                    lasd1.canHM(UIDChmName)

                if inCreateRGB:
                    '''make a composite raster from raw count and percentages of
                    points.'''
                    UIDPercRastName = UID + inPercRastName
                    UIDRcRastName = UID + inRcRastName
                    lasd1.pntCntToCompRast(vegClassList, UIDPercRastName,
                                           UIDRcRastName)

                if inCreateDenCov:
                    '''calculate the canopy cover and canopy density.'''
                    UIDDenName = UID + inDenName
                    UIDCovName = UID + inCovName
                    lasd1.canCovDen(vegClassList, UIDDenName, UIDCovName)

                addToMap(inWD, inMapDocName, root)

    printArc("\nDone. Total run time {:.2f} seconds."
             .format(time.time() - start))
