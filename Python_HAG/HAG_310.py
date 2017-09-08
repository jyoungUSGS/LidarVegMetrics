## HAGCanopyMetrics_3'10.py
## 25AUG17 17:40
#############################################################
## RUN THE CODE IN CMD.EXE WINDOW AND NOT IN PYTHON IDLE
#############################################################
## Code originally from Karl Heidemann (USGS-EROS) as IDL wrapper to LASTOOLS;
## Augmented with added products and code refinementsby Monica Palaseanu-Lovejoy (USGS-EGSC);
## Converted to Python by Eric Smith (USGS-LSC volunteer).
## Additional edits by John Young (USGS-LSC) December 2015
## Modifications by Jeff Danielson (USGS-EROS)
## Massive code clean-up and corrections by Karl Heidemann (USGS-EROS) September 2016.
##
## IMPORTANT:: Source LAZ files will be uncompressed to LAS as a first step.
## USER NOTE:: If there are Source LAS files with the same filename as Source LAZ files,
##          :: the LAS files will be overwritten WITHOUT WARNING.
##
## Changes in 3'00:
##   - Reordered denoising to after reprojection.
##       The process needs H&V units to be the same.
##   - Stripped 'drop_z_below 0' references in point file creation;
##       added to raster product generation steps.
##   - Stripped 'drop_z_above MAX_ELEV' references in point file creation;
##       added to raster product generation steps.
##   - Restored the 'remove_vlrs' from cleaning step.
##       MANY VLRs are corrupt and will crash the process.
##       The CRS info in MANY LAS files is incorrect anyway.
##       It is safer and easier to:
##         confirm the CRS manually,
##         include it in the .cfg file,
##         and force it in the reprojection process.
##   - Added EPSG info to the .cfg file, eliminating the .epsg file.
##   - Revamped the folder structure for clarity
##   - Revamped display output for easier progress monitoring
##       * Suppressed blank line per file from FOR loops
##       * Added timing reports for each program section
##
##   - Changed all product generation to use UNBuffered point data files
##       Ensuring that the -merged option is included to avoid edge artifacts.
##   - Source data is used to generate products as follows:
##       UNBuffered Elevation:
##         * Lidar files for Archive
##         * Bare-Earth Surfaces          (2)
##         * First Reflective Surfaces    (2)
##         * Point Density Surface        (1)
##         * Point Density Binary Surface (1)
##       UNBuffered Height:
##         * HAG Max,Avg,Std,Cov Surfaces (4)
##         * Percentile Surfaces         (10)
##         * Stat Surfaces                (7)
##         * Vert Dist Surfaces           (2)
##         * Relative Heights Surfaces    (5)
##         * Height Counts Surfaces       (5)
##         * Canopy Density Surface       (1)
##
## Changes in 3'10:
##   - Changed product folder arrangement and some names.
##   - Changed binned product generation operations to reduce memory consumption
##   - Added the bin, stat, percentile, and canopy_cutoff values to the config file
##   - Added bin values to the filenames of the binned product rasters
##   - Changed the order of processing to do all coarse rasters first
##   - Moved raster statistics, CRS, and pyramid calculations to to occur after 
##     each product block (rather than all at once at the very end).
##
###############################################################################
###############################################################################
##
## LOAD MODULES FOR PYTHON ##
print('')
print('')
print('Load Python modules and configuring the workspace ...')
print('  time')
import time
time0 = time.time()
time1 = time0
OutFldr = ''
print('    gmtime')
from time import gmtime
print('    strftime')
from time import strftime
print('  os')
import os
print('  glob')
import glob
print('  traceback')
import traceback
print('  shutil')
import shutil
print('  multiprocessing')
import multiprocessing
print('  sys')
import sys
print('    exit')
from sys import exit
print('  subprocess')
import subprocess
print('    check_call')
from subprocess import check_call
print('  contextlib')
import contextlib
print('    contextmanager')
from contextlib import contextmanager
try:
  import configparser
  print('  configparser')
except ImportError:
  import ConfigParser as configparser
  print('  ConfigParser')
print('  arcpy')
import arcpy
print('    env')
from arcpy import env
print('    arcpy.sa')
from arcpy.sa import *
#################################################################################
## READ CONFIGURATION FILE ##
print('Read Configuration File ...')
config = configparser.RawConfigParser()
CFG_FILE_LOC = sys.argv[1]
if not os.path.isfile(CFG_FILE_LOC):
  print('!!  Configuration File Not Found  !!')
  sys.exit()
config.read(CFG_FILE_LOC)
SECTION1       = 'Section1'
NAME_PJCT      = config.get(SECTION1, 'NAME_PJCT')
NAME_FLDR_PJCT = config.get(SECTION1, 'NAME_FLDR_PJCT')
LRG_GRD_SPC    = config.get(SECTION1, 'COARSE_GRD_SPACING')
SML_GRD_SPC    = config.get(SECTION1, 'FINE_GRD_SPACING')
ELEV_MAX       = config.get(SECTION1, 'ELEV_MAX')
HAG_MAX        = config.get(SECTION1, 'HAG_MAX')
CORE_NUM       = config.get(SECTION1, 'CORE_NUM')
NOISE_ISO      = config.get(SECTION1, 'LASNOISE_ISOLATED')
NOISE_XY       = config.get(SECTION1, 'LASNOISE_STEP_XY')
NOISE_Z        = config.get(SECTION1, 'LASNOISE_STEP_Z')
RTL_BUF        = config.get(SECTION1, 'RETILE_BUFFER')
TRG_CRS        = config.get(SECTION1, 'TARGET_CRS')
SRC_CRS        = config.get(SECTION1, 'SOURCE_CRS')
SRC_UNITS      = config.get(SECTION1, 'SOURCE_UNITS')
STATISTICS     = config.get(SECTION1, 'STATS')
PERCENTILES    = config.get(SECTION1, 'PERCENTILES')
HEIGHT_BINS    = config.get(SECTION1, 'BINS')
CUTOFF         = config.get(SECTION1, 'CUTOFF')
CORE_NUM=str(multiprocessing.cpu_count()-1)
###############################################################################
while NAME_FLDR_PJCT[-1] == '\\':
  NAME_FLDR_PJCT = NAME_FLDR_PJCT[:-1]
###############################################################################
# Set the workspace and overwrite option
print('Set the workspace and overwrite option ...')
arcpy.env.workspace = "IN_MEMORY"
arcpy.env.overwriteOutput = True
arcpy.CheckOutExtension("Spatial")
SpatRef = arcpy.SpatialReference(int(TRG_CRS))
#################################################################################
# Collect and format reprojection parameters
print('Collect and format reprojection parameters ...')
EPSG_CODE=' --t_srs EPSG:'+TRG_CRS+' --a_srs EPSG:'+SRC_CRS
if SRC_UNITS=="mf":
  EPSG_CODE=EPSG_CODE+' --point-translate "x*1.0 y*1.0 z*0.3048"'
elif SRC_UNITS=="fm":
  EPSG_CODE=EPSG_CODE+' --point-translate "x*1.0 y*1.0 z*3.280839895"'
else:
  SRC_UNITS = ''
#################################################################################
# Set the folder location variables
print('Set the folder location variables ...')
Cleaned    = NAME_FLDR_PJCT+'\\Source\\Cleaned\\'
DeDuped    = NAME_FLDR_PJCT+'\\Source\\DeDuped\\'
sLAS       = NAME_FLDR_PJCT+'\\Source\\LAS\\'
sLAZ       = NAME_FLDR_PJCT+'\\Source\\LAZ\\'
DeNoised   = NAME_FLDR_PJCT+'\\Albers\\DeNoised\\'
LclTiles   = NAME_FLDR_PJCT+'\\Albers\\LclTiles\\'
eUNB       = NAME_FLDR_PJCT+'\\Albers\\UNBuffered\\'
Meta       = NAME_FLDR_PJCT+'\\Metadata\\'
ProdLRG    = NAME_FLDR_PJCT+'\\Product\\%sm\\'%LRG_GRD_SPC
ProdSML    = NAME_FLDR_PJCT+'\\Product\\%sm\\'%SML_GRD_SPC
pLAZ       = NAME_FLDR_PJCT+'\\Product\\LAZ\\Elevation\\'
pHAG       = NAME_FLDR_PJCT+'\\Product\\LAZ\\Height\\'
BEFRSLRG   = ProdLRG+'BE_FRS\\'
HAGLRG     = ProdLRG+'HAG\\'
cDensLRG   = ProdLRG+'Coverage\\Canopy\\'
pDensLRG   = ProdLRG+'Coverage\\Points\\'
hBinsLRG   = ProdLRG+'Height\\Density\\'
hCountsLRG = ProdLRG+'Height\\Counts\\'
StatsLRG   = ProdLRG+'Stats\\'
TP50LRG    = ProdLRG+'TP50\\'
VDRLRG     = ProdLRG+'VDR\\'
BEFRSSML   = ProdSML+'BE_FRS\\'
HAGSML     = ProdSML+'HAG\\'
cDensSML   = ProdSML+'Density\\Canopy\\'
pDensSML   = ProdSML+'Density\\Points\\'
hBinsSML   = ProdSML+'Height\\Density\\'
hCountsSML = ProdSML+'Height\\Counts\\'
StatsSML   = ProdSML+'Stats\\'
TP50SML    = ProdSML+'TP50\\'
VDRSML     = ProdSML+'VDR\\'
#################################################################################
@contextmanager
def suppress_stdout():
    with open(os.devnull, "w") as devnull:
        old_stdout = sys.stdout
        sys.stdout = devnull
        try:
            yield
        finally:
            sys.stdout = old_stdout
###############################################################################
def run_process(cmd, prnt=False):
  pipeout = subprocess.PIPE
  si = subprocess.STARTUPINFO()
  si.dwFlags = subprocess.STARTF_USESTDHANDLES | subprocess.SW_HIDE
  pro = subprocess.Popen(cmd, stdout=pipeout, startupinfo=si)
  out, err = pro.communicate()
#################################################################################
def time_check(time0,time1):
  time_elapsed = time.time()-time1
  min_elapsed = int(time_elapsed / 60)
  sec_elapsed = time_elapsed % 60
  print('    Section Run Time: ' +str(min_elapsed)+ ':' +str(round(sec_elapsed,2)))
  time_elapsed = time.time()-time0
  min_elapsed = int(time_elapsed / 60)
  sec_elapsed = time_elapsed % 60
  now=str.upper(strftime("%Y%b%d"))+str(strftime(" %A %I:%M:%S%p"))
  print('    Cumulative Run Time: ' +str(min_elapsed)+ ':' +str(round(sec_elapsed,2)))
  print('    Current Time: '+now)
  print('')
#################################################################################
def img_crs(OutFldr):
  images = []
  for root,dirs,files in os.walk(OutFldr):
    for filename in files:
      if filename.endswith('.img'):
        f = os.path.join(root,filename)
        arcpy.DefineProjection_management(f,SpatRef)
        arcpy.CalculateStatistics_management(f)
        arcpy.BuildPyramids_management(f)
###############################################################################
print('Setup project folders ...')
if __name__ == "__main__":
  BigCount = int(0)
###############################################################################
## SETUP PROJECT FOLDERS ##
  subs = [Cleaned, DeDuped, sLAS, sLAZ, DeNoised, LclTiles, eUNB, ProdLRG,
          ProdSML, BEFRSLRG, HAGLRG, cDensLRG, pDensLRG, hBinsLRG, hCountsLRG,
          StatsLRG, TP50LRG, VDRLRG, BEFRSSML, HAGSML, cDensSML, pDensSML,
          hBinsSML, hCountsSML, StatsSML, TP50SML, VDRSML, pLAZ, pHAG]
  for i in subs:
    if not os.path.exists(i):
      os.makedirs(i)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
###############################################################################
##
## BEGIN HEIGHT-ABOVE-GROUND PROCEDURES ##
##
###############################################################################
###############################################################################
  print('_'*(42+len(NAME_PJCT)))
  print('')
  print('BEGINNING CANOPY METRICS PROCESSING FOR:  '+NAME_PJCT)
  print('_'*(42+len(NAME_PJCT)))
  print('')
  print('PREPARING POINT CLOUD DATA FOR PROCESSING')
  print('_'*41)
  print('')
#################################################################################
  print('Decompressing LAZ source files to LAS ...')
  LAZ00_FILES = glob.glob(sLAZ+'*.laz')
  if len(LAZ00_FILES) >= 1:
    CMD=['las2las',
        '-i',sLAZ+'*.laz',
        '-odir',sLAS]
    run_process(CMD)
  else:
    print('No LAZ source files ... using LAS files.')
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('De-Duping LAS files ...')
  CMD=['lasduplicate',
#      '-v',
      '-i',sLAS+'*.las',
      '-odir',DeDuped]
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Cleaning LAS files ...')
  CMD=['las2las',
      '-i',DeDuped+'*.las',
      '-odir',Cleaned,
      '-rescale','0.01','0.01','0.001',
      '-reoffset','0','0','0',
      '-remove_padding',
      '-remove_all_vlrs',
      '-repair_zero_returns',
      '-set_version','1.2',
      '-point_type','1']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  if SRC_CRS != TRG_CRS:
    print('ReProjecting LAS files from EPSG:'+SRC_CRS+' to EPSG:'+TRG_CRS+' ...')
    NAMES_FILES_LAS = glob.glob(Cleaned + '*.las')
    for NAME_FILE_LAS in NAMES_FILES_LAS:
      NAME_LAS = NAME_FILE_LAS.split('\\')[-1]
      CMD1='las2las2 '+\
            '-i '+Cleaned+NAME_LAS+' '+\
            '-o '+LclTiles+NAME_LAS+' '+\
            EPSG_CODE
      run_process(CMD1)
    time_check(time0,time1) ; time1 = time.time()
#################################################################################
    print('Compressing /LclTile/*.LAS to /LclTile/*.LAZ ...')
    CMD2='laszip -i '+LclTiles+'*.las'
    run_process(CMD2)
    CMD3='rm '+LclTiles+'*.las'
    run_process(CMD3)
    time_check(time0,time1) ; time1 = time.time()
#################################################################################
  else:
    print('Source lidar is already in EPSG:'+TRG_CRS+'.')
    print('Skipping reprojection ...')
    print('')
    print('Compressing /Cleaned/*.LAS to /LclTiles/*.LAZ ...')
    CMD4='las2las -i '+Cleaned+'*.las -olaz -odir '+LclTiles+' -epsg '+TRG_CRS
    run_process(CMD4)
    time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Flagging Noise ...')
  CMD=['lasnoise',
       '-i',LclTiles+'*.laz',
       '-odir',DeNoised,
       '-ignore_class','2',
       '-step_xy',NOISE_XY,
       '-step_z',NOISE_Z,
       '-isolated',NOISE_ISO,
       '-cores',CORE_NUM,
       '-olaz']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Re-Tiling with *NO BUFfer* ...')
  CMD=['lastile',
       '-i',DeNoised+'*.laz',
       '-o',eUNB+NAME_PJCT+'.laz']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Reformatting file names ...')
  NAMES_FILES_BUF = glob.glob(eUNB + '*.laz')
  for NAME_FILE in NAMES_FILES_BUF:
#   Extract coordinates from filename
    BASE_NAME = (NAME_FILE.split('\\')[-1])[:-4]
    BASE_NAME_SPLIT = BASE_NAME.split('_')
    try:
#     Convert coordinates from strings to integers and trim zeroes
#     Create reformatted tile names from coordinates
      COORDS = [int(i)/1000 for i in BASE_NAME_SPLIT[-2:]]
      DIRX = 'w' if COORDS[0] < 0 else 'e'
      DIRY = 's' if COORDS[1] < 0 else 'n'
      TOKES = [str(abs(i)) for i in COORDS]
      TOKE0 = '0'*(4-len(TOKES[0]))+TOKES[0]+DIRX
      TOKE1 = '0'*(4-len(TOKES[1]))+TOKES[1]+DIRY
      NAME_FILE_TILE = NAME_PJCT+'_'+TOKE0+TOKE1+'.laz'
      shutil.copyfile(NAME_FILE, pLAZ+NAME_FILE_TILE)
    except ValueError:
#     ValueError may occur if coordinates are not read correctly
      print(BASE_NAME + '.laz could not be reformatted.')
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Creating *UNBuffered* HAG LAZ files ...')
# ALL Downstream HAG processes are based on unbuffered tiles
  CMD=['lasheight',
       '-i',pLAZ+'*.laz',
       '-odir',pHAG,
       '-cores',CORE_NUM,
       '-replace_z',
       '-odix','_HAG',
       '-olaz']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
#################################################################################
  print('_'*44)
  print('')
  print('GENERATING %sm LIDAR CANOPY METRICS PRODUCTS'%LRG_GRD_SPC)
  print('_'*44)
  print('')
#################################################################################
  print('Generating BE @ %sm ...'%LRG_GRD_SPC)
  OutFldr=BEFRSLRG
  CMD='lasgrid'+\
      ' -i '+pLAZ+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_BE%sm.img'%LRG_GRD_SPC+\
      ' -merged'+\
      ' -step '+LRG_GRD_SPC+\
      ' -fill 5'+\
      ' -last_only'+\
      ' -keep_class 2'+\
      ' -elevation'+\
      ' -average'
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating FRS @ %sm ...'%LRG_GRD_SPC)
  OutFldr=BEFRSLRG
  CMD='lasgrid'+\
      ' -i '+pLAZ+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_FRS%sm.img'%LRG_GRD_SPC+\
      ' -merged'+\
      ' -step '+LRG_GRD_SPC+\
      ' -fill 5'+\
      ' -first_only'+\
      ' -drop_withheld'+\
      ' -drop_class 0 7 12 15'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0.0'+\
      ' -elevation'+\
      ' -highest'
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
#############################################################################
  print('Generating HAG MAX @ %sm ...' %LRG_GRD_SPC)
  OutFldr=HAGLRG
  CMD='lasgrid'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_HAG%smMAX.img'%LRG_GRD_SPC+\
      ' -merged'+\
      ' -step '+LRG_GRD_SPC+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0.0'+\
      ' -elevation'+\
      ' -highest'
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating HAG AVG @ %sm ...' %LRG_GRD_SPC)
  OutFldr=HAGLRG
  CMD='lasgrid'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_HAG%smAVG.img'%LRG_GRD_SPC+\
      ' -merged'+\
      ' -step '+LRG_GRD_SPC+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0.0'+\
      ' -elevation'+\
      ' -average'
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating HAG STD @ %sm ...' %LRG_GRD_SPC)
  OutFldr=HAGLRG
  CMD='lasgrid'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_HAG%smSTD.img'%LRG_GRD_SPC+\
      ' -merged'+\
      ' -step '+LRG_GRD_SPC+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0.0'+\
      ' -elevation'+\
      ' -std'
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Point Density and Binary @ %sm ...' %LRG_GRD_SPC)
  OutFldr=pDensLRG
  DensOut = OutFldr+NAME_PJCT+"_PointDens%sm.img"%LRG_GRD_SPC
  BinOut  = OutFldr+NAME_PJCT+"_PointDensBin%sm.img"%LRG_GRD_SPC
  CMD='lasgrid'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+DensOut+\
      ' -merged'+\
      ' -step '+LRG_GRD_SPC+\
      ' -counter_16bit'
  run_process(CMD)
  arcpy.CalculateStatistics_management(DensOut)
# Slice the Density image to Binary
  remap = RemapRange([[0,1,"NODATA"],[1,65535,1]])
  BinRas = Reclassify(DensOut, "VALUE", remap, "NODATA")
  BinRas.save(BinOut)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
#####################################################################
  print('Generating Canopy Coverage @ %sm ...' %LRG_GRD_SPC)
  OutFldr=cDensLRG
  CMD='lascanopy'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_HAG%smCanopyCov.img'%LRG_GRD_SPC+\
      ' -merged'+\
      ' -step '+LRG_GRD_SPC+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0.0'+\
      ' -height_cutoff '+CUTOFF+\
      ' -cov'
  run_process(CMD)
  shutil.move(OutFldr+NAME_PJCT+'_HAG%smCanopyCov_cov.img'%LRG_GRD_SPC,
              OutFldr+NAME_PJCT+'_HAG%smCanopyCov.img'%LRG_GRD_SPC)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
#####################################################################
  print('Generating Canopy Density @ %sm ...' %LRG_GRD_SPC)
  OutFldr=cDensLRG
  CMD='lascanopy'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_HAG%smCanopyDens.img'%LRG_GRD_SPC+\
      ' -step '+LRG_GRD_SPC+\
      ' -merged'+\
      ' -keep_class 1 2'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0.0'+\
      ' -height_cutoff '+CUTOFF+\
      ' -dns'
  run_process(CMD)
  shutil.move(OutFldr+NAME_PJCT+'_HAG%smCanopyDens_dns.img'%LRG_GRD_SPC,
              OutFldr+NAME_PJCT+'_HAG%smCanopyDens.img'%LRG_GRD_SPC)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
#########################################################################
  print('Generating Statistics @ %sm ...' %LRG_GRD_SPC)
  OutFldr=StatsLRG
  STAT_LIST=STATISTICS.split()
  NDX = 0
  while NDX < len(STAT_LIST):
    STAT = STAT_LIST[NDX]
    OutFile=OutFldr+NAME_PJCT+'_HAG%smSTATS.img'%LRG_GRD_SPC
    CMD='lascanopy'+\
        ' -i '+pHAG+'*.laz'+\
        ' -o '+OutFile+\
        ' -merged'+\
        ' -step '+LRG_GRD_SPC+\
        ' -keep_class 1'+\
        ' -keep_scan_angle -15 15'+\
        ' -drop_withheld'+\
        ' -drop_z_above '+ELEV_MAX+\
        ' -clamp_z_below 0.0'+\
        ' -height_cutoff '+CUTOFF+\
        ' -'+STAT
    run_process(CMD)
    NDX = NDX+1
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
#########################################################################
  print('Generating Percentiles @ %sm ...' %LRG_GRD_SPC)
  OutFldr=TP50LRG
  PCTL_LIST=PERCENTILES.split()
  NDX = len(PCTL_LIST)
  while NDX > 0:
    PCTLS = str(PCTL_LIST[NDX-2])+' '+str(PCTL_LIST[NDX-1])
    if NDX == 1:
      PCTLS = str(PCTL_LIST[NDX-1])
    OutFile=OutFldr+NAME_PJCT+'_HAG%smCENT.img'%LRG_GRD_SPC
    CMD='lascanopy'+\
        ' -i '+pHAG+'*.laz'+\
        ' -o '+OutFile+\
        ' -merged'+\
        ' -step '+LRG_GRD_SPC+\
        ' -keep_class 1'+\
        ' -keep_scan_angle -15 15'+\
        ' -drop_withheld'+\
        ' -drop_z_above '+ELEV_MAX+\
        ' -clamp_z_below 0.0'+\
        ' -height_cutoff '+CUTOFF+\
        ' -p '+PCTLS
    run_process(CMD)
    NDX = NDX-1
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Height Counts @ %sm ...' %LRG_GRD_SPC)
  OutFldr=hCountsLRG
  BIN_LIST=HEIGHT_BINS.split()
  NDX = len(BIN_LIST)
  while NDX > 0:
    BINS = str(BIN_LIST[NDX-2])+' '+str(BIN_LIST[NDX-1])
    if NDX == 1:
      BINS = '0 '+str(BIN_LIST[NDX-1])
    BINstr = str(BIN_LIST[NDX-1]).replace(".","'")
    OutFile=OutFldr+NAME_PJCT+'_HAG%smHCount_'%LRG_GRD_SPC+BINstr
    CMD='lascanopy'+\
        ' -i '+pHAG+'*.laz'+\
        ' -o '+OutFile+'.img'+\
        ' -merged'+\
        ' -step '+LRG_GRD_SPC+\
        ' -keep_class 1'+\
        ' -keep_scan_angle -15 15'+\
        ' -drop_withheld'+\
        ' -drop_z_above '+ELEV_MAX+\
        ' -clamp_z_below 0.0'+\
        ' -height_cutoff '+CUTOFF+\
        ' -c '+BINS
    run_process(CMD)
    shutil.move(OutFile+'_c00.img',OutFile+'.img')
    NDX = NDX-1
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###########################################################################################
  print('Generating Height Densities @ %sm ...' %LRG_GRD_SPC)
  OutFldr=hBinsLRG
  BIN_LIST=HEIGHT_BINS.split()
  NDX = len(BIN_LIST)
  while NDX > 0:
    BINS = str(BIN_LIST[NDX-2])+' '+str(BIN_LIST[NDX-1])
    if NDX == 1:
      BINS = '0 '+str(BIN_LIST[NDX-1])
    BINstr = str(BIN_LIST[NDX-1]).replace(".","'")
    OutFile=OutFldr+NAME_PJCT+'_HAG%smHDens_'%LRG_GRD_SPC+BINstr
    CMD='lascanopy'+\
        ' -i '+pHAG+'*.laz'+\
        ' -o '+OutFile+'.img'+\
        ' -merged'+\
        ' -step '+LRG_GRD_SPC+\
        ' -keep_class 1'+\
        ' -keep_scan_angle -15 15'+\
        ' -drop_withheld'+\
        ' -drop_z_above '+ELEV_MAX+\
        ' -clamp_z_below 0.0'+\
        ' -height_cutoff '+CUTOFF+\
        ' -d '+BINS
    run_process(CMD)
    shutil.move(OutFile+'_d00.img',OutFile+'.img')
    NDX = NDX-1
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Vertical Distribution Ratios @ %sm ...' %LRG_GRD_SPC)
  OutFldr=VDRLRG
  HOME =  arcpy.Raster(TP50LRG+NAME_PJCT+"_HAG%smCENT_p50.img"%LRG_GRD_SPC)
  CH98 =  arcpy.Raster(TP50LRG+NAME_PJCT+"_HAG%smCENT_p98.img"%LRG_GRD_SPC)
  CH100 = arcpy.Raster(StatsLRG+NAME_PJCT+"_HAG%smSTATS_max.img"%LRG_GRD_SPC)
  VDR98 = (CH98 - HOME) / CH98
  VDR98.save(OutFldr+NAME_PJCT+"_HAG%smVDR98.img"%LRG_GRD_SPC)
  VDR100 = (CH100 - HOME) / CH100
  VDR100.save(OutFldr+NAME_PJCT+"_HAG%smVDR100.img"%LRG_GRD_SPC)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
###############################################################################
  print('_'*44)
  print('')
  print('GENERATING %sm LIDAR CANOPY METRICS PRODUCTS'%SML_GRD_SPC)
  print('_'*44)
  print('')
###############################################################################
  print('Generating BE @ %sm ...'%SML_GRD_SPC)
  OutFldr=BEFRSSML
  CMD='lasgrid'+\
      ' -i '+pLAZ+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_BE%sm.img'%SML_GRD_SPC+\
      ' -merged'+\
      ' -step '+SML_GRD_SPC+\
      ' -fill 5'+\
      ' -last_only'+\
      ' -keep_class 2'+\
      ' -elevation'+\
      ' -average'
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating FRS @ %sm ...'%SML_GRD_SPC)
  OutFldr=BEFRSSML
  CMD='lasgrid'+\
      ' -i '+pLAZ+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_FRS%sm.img'%SML_GRD_SPC+\
      ' -merged'+\
      ' -step '+SML_GRD_SPC+\
      ' -fill 5'+\
      ' -first_only'+\
      ' -drop_withheld'+\
      ' -drop_class 0 7 12 15'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0'+\
      ' -elevation'+\
      ' -highest'
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating HAG MAX @ %sm ...' %SML_GRD_SPC)
  OutFldr=HAGSML
  CMD='lasgrid'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_HAG%smMAX.img'%SML_GRD_SPC+\
      ' -merged'+\
      ' -step '+SML_GRD_SPC+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0'+\
      ' -elevation'+\
      ' -highest'
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating HAG AVG @ %sm ...' %SML_GRD_SPC)
  OutFldr=HAGSML
  CMD='lasgrid'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_HAG%smAVG.img'%SML_GRD_SPC+\
      ' -merged'+\
      ' -step '+SML_GRD_SPC+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0'+\
      ' -elevation'+\
      ' -average'
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating HAG STD @ %sm ...' %SML_GRD_SPC)
  OutFldr=HAGSML
  CMD='lasgrid'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_HAG%smSTD.img'%LRG_GRD_SPC+\
      ' -merged'+\
      ' -step '+SML_GRD_SPC+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0'+\
      ' -elevation'+\
      ' -std'
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Point Density and Binary @ %sm ...' %SML_GRD_SPC)
  OutFldr=pDensSML
  DensOut = OutFldr+NAME_PJCT+"_PointDens%sm.img"%SML_GRD_SPC
  BinOut  = OutFldr+NAME_PJCT+"_PointDensBin%sm.img"%SML_GRD_SPC
  CMD='lasgrid'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+DensOut+\
      ' -merged'+\
      ' -step '+SML_GRD_SPC+\
      ' -counter_16bit'
  run_process(CMD)
  arcpy.CalculateStatistics_management(DensOut)
# Slice the Density image to Binary
  remap = RemapRange([[0,1,"NODATA"],[1,65535,1]])
  BinRas = Reclassify(DensOut, "VALUE", remap, "NODATA")
  BinRas.save(BinOut)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Canopy Coverage @ %sm ...' %SML_GRD_SPC)
  OutFldr=cDensSML
  CMD='lascanopy'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_HAG%smCanopyCov.img'%SML_GRD_SPC+\
      ' -merged'+\
      ' -step '+SML_GRD_SPC+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0'+\
      ' -height_cutoff '+CUTOFF+\
      ' -cov'
##########  run_process(CMD)
  shutil.move(OutFldr+NAME_PJCT+'_HAG%smCanopyCov_cov.img'%SML_GRD_SPC,
              OutFldr+NAME_PJCT+'_HAG%smCanopyCov.img'%SML_GRD_SPC)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Canopy Density @ %sm ...' %SML_GRD_SPC)
  OutFldr=cDensSML
  CMD='lascanopy'+\
      ' -i '+pHAG+'*.laz'+\
      ' -o '+OutFldr+NAME_PJCT+'_HAG%smCanopyDens.img'%SML_GRD_SPC+\
      ' -step '+SML_GRD_SPC+\
      ' -merged'+\
      ' -keep_class 1 2'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -clamp_z_below 0'+\
      ' -height_cutoff '+CUTOFF+\
      ' -dns'
  run_process(CMD)
  shutil.move(OutFldr+NAME_PJCT+'_HAG%smCanopyDens_dns.img'%SML_GRD_SPC,
              OutFldr+NAME_PJCT+'_HAG%smCanopyDens.img'%SML_GRD_SPC)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Statistics @ %sm ...' %SML_GRD_SPC)
  OutFldr=StatsSML
  STAT_LIST=STATISTICS.split()
  NDX = 0
  while NDX < len(STAT_LIST):
    STAT = STAT_LIST[NDX]
    OutFile=OutFldr+NAME_PJCT+'_HAG%smSTATS.img'%SML_GRD_SPC
    CMD='lascanopy'+\
        ' -i '+pHAG+'*.laz'+\
        ' -o '+OutFile+\
        ' -merged'+\
        ' -step '+SML_GRD_SPC+\
        ' -keep_class 1'+\
        ' -keep_scan_angle -15 15'+\
        ' -drop_withheld'+\
        ' -drop_z_above '+ELEV_MAX+\
        ' -clamp_z_below 0'+\
        ' -height_cutoff '+CUTOFF+\
        ' -'+STAT
    run_process(CMD)
    NDX = NDX+1
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
#############################################################################
  print('Generating Percentiles @ %sm ...' %SML_GRD_SPC)
  OutFldr=TP50SML
  PCTL_LIST=PERCENTILES.split()
  NDX = len(PCTL_LIST)
  while NDX > 0:
    PCTLS = str(PCTL_LIST[NDX-2])+' '+str(PCTL_LIST[NDX-1])
    if NDX == 1:
      PCTLS = str(PCTL_LIST[NDX-1])
    OutFile=OutFldr+NAME_PJCT+'_HAG%smCENT.img'%SML_GRD_SPC
    CMD='lascanopy'+\
        ' -i '+pHAG+'*.laz'+\
        ' -o '+OutFile+\
        ' -merged'+\
        ' -step '+SML_GRD_SPC+\
        ' -keep_class 1'+\
        ' -keep_scan_angle -15 15'+\
        ' -drop_withheld'+\
        ' -drop_z_above '+ELEV_MAX+\
        ' -clamp_z_below 0.0'+\
        ' -height_cutoff '+CUTOFF+\
        ' -p '+PCTLS
    run_process(CMD)
    NDX = NDX-1
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Height Counts @ %sm ...' %SML_GRD_SPC)
  OutFldr=hCountsSML
  BIN_LIST=HEIGHT_BINS.split()
  NDX = len(BIN_LIST)
  while NDX > 0:
    BINS = str(BIN_LIST[NDX-2])+' '+str(BIN_LIST[NDX-1])
    if NDX == 1:
      BINS = '0 '+str(BIN_LIST[NDX-1])
    BINstr = str(BIN_LIST[NDX-1]).replace(".","'")
    OutFile=OutFldr+NAME_PJCT+'_HAG%smHCount_'%SML_GRD_SPC+BINstr
    CMD='lascanopy'+\
        ' -i '+pHAG+'*.laz'+\
        ' -o '+OutFile+'.img'+\
        ' -merged'+\
        ' -step '+SML_GRD_SPC+\
        ' -keep_class 1'+\
        ' -keep_scan_angle -15 15'+\
        ' -drop_withheld'+\
        ' -drop_z_above '+ELEV_MAX+\
        ' -clamp_z_below 0.0'+\
        ' -height_cutoff '+CUTOFF+\
        ' -c '+BINS
    run_process(CMD)
    shutil.move(OutFile+'_c00.img',OutFile+'.img')
    NDX = NDX-1
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Generating Height Densities @ %sm ...' %SML_GRD_SPC)
  OutFldr=hBinsSML
  BIN_LIST=HEIGHT_BINS.split()
  NDX = len(BIN_LIST)
  while NDX > 0:
    BINS = str(BIN_LIST[NDX-2])+' '+str(BIN_LIST[NDX-1])
    if NDX == 1:
      BINS = '0 '+str(BIN_LIST[NDX-1])
    BINstr = str(BIN_LIST[NDX-1]).replace(".","'")
    OutFile=OutFldr+NAME_PJCT+'_HAG%smHDens_'%SML_GRD_SPC+BINstr
    CMD='lascanopy'+\
        ' -i '+pHAG+'*.laz'+\
        ' -o '+OutFile+'.img'+\
        ' -merged'+\
        ' -step '+SML_GRD_SPC+\
        ' -keep_class 1'+\
        ' -keep_scan_angle -15 15'+\
        ' -drop_withheld'+\
        ' -drop_z_above '+ELEV_MAX+\
        ' -clamp_z_below 0.0'+\
        ' -height_cutoff '+CUTOFF+\
        ' -d '+BINS
    run_process(CMD)
    shutil.move(OutFile+'_d00.img',OutFile+'.img')
    NDX = NDX-1
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Vertical Distribution Ratios @ %sm ...' %SML_GRD_SPC)
  OutFldr=VDRSML
  HOME =  arcpy.Raster(TP50SML+NAME_PJCT+"_HAG%smCENT_p50.img"%SML_GRD_SPC)
  CH98 =  arcpy.Raster(TP50SML+NAME_PJCT+"_HAG%smCENT_p98.img"%SML_GRD_SPC)
  CH100 = arcpy.Raster(StatsSML+NAME_PJCT+"_HAG%smSTATS_max.img"%SML_GRD_SPC)
  VDR98 = (CH98 - HOME) / CH98
  VDR98.save(OutFldr+NAME_PJCT+"_HAG%smVDR98.img"%SML_GRD_SPC)
  VDR100 = (CH100 - HOME) / CH100
  VDR100.save(OutFldr+NAME_PJCT+"_HAG%smVDR100.img"%SML_GRD_SPC)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
###############################################################################
# Spread the joyous news!
  print('')
  print('_'*(41+len(NAME_PJCT)))
  print('')
  print('CANOPY METRICS PROCESSING COMPLETED FOR: '+NAME_PJCT)
  print('_'*(41+len(NAME_PJCT)))
  print('')
  print('')
sys.exit()
