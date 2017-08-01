## HAGCanopyMetrics_303.py
##
## Code originally from Karl Heidemann (USGS-EROS) as IDL wrapper to LASTOOLS;
## Augmented with added products and code refinements by Monica Palaseanu-Lovejoy (USGS-EGSC);
## Converted to Python by Eric Smith (USGS-LSC volunteer) under direction of John Young (USGS-LSC).
## Additional edits by John Young (USGS-LSC) December 2015
## Modifications by Jeff Danielson (USGS-EROS)
## Massive code clean-up and corrections by Karl Heidemann (USGS-EROS) September 2016.
## Modified from 301 to 302 by John Young, November 2016 to get rid of libLAS projection step, 
##   do projection in LasTools, do all processing from LAZ, and tweak processing parameters  
##   after issues with results, also renamed Albers folder to Tiles and moved Denoised folder 
##   under Source. 
## Modified to version '303' by John Young, June 2017, with a few additional tweaks: 
##   remaed TP50 folder to HghtCentiles, and moved final cleaned LAZ folder (pLAZ) under tiled to seperate out
##   products from source files.  Also added LAS codes to ignore anything that is classified as not veg in the
##   noise filtering step, and add source and target units into LAStools projection step. 
##   Note: the configuration file now requires the source and target spatial reference (EPSG), source and target
##  (x,y) units, and source and target elevation (z) units be specified.
##
## IMPORTANT:: Source LAZ files will be uncompressed to LAS as a first step. ## No longer relevant
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
##   - Restored the 'remove_vlrs' from cleaning step.  ## took this back out, removes everything!
##       MANY VLRs are corrupt and will crash the process.
##       The CRS info in MANY LAS files is incorrect anyway.
##       It is safer and easier to:
##         confirm the CRS manually,
##         include it in the .cfg file,
##         and force it in the reprojection process.  ## if not reprojecting, strips out all GeoTags! 
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
###############################################################################
## LOAD MODULES FOR PYTHON ##
print('')
print('')
print('Loading Python modules and configuring the workspace ...')
import time
time0 = time.time()
time1 = time0
from time import gmtime, strftime
import os
import glob
import traceback
import shutil
import multiprocessing
import sys
from sys import exit
import subprocess
from subprocess import check_call
import contextlib
from contextlib import contextmanager
try:
  import configparser
except ImportError:
  import ConfigParser as configparser
import arcpy
from arcpy import env
from arcpy.sa import *
#################################################################################
### READ CONFIGURATION FILE ##
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
SRC_CRS        = config.get(SECTION1, 'SOURCE_CRS')
SRC_UNITS      = config.get(SECTION1, 'SOURCE_UNITS')
SRC_ELEV_UNITS = config.get(SECTION1, 'SOURCE_ELEV_UNITS')
TRG_CRS        = config.get(SECTION1, 'TARGET_CRS')
TRG_UNITS      = config.get(SECTION1, 'TARGET_UNITS')
TRG_ELEV_UNITS = config.get(SECTION1, 'TARGET_ELEV_UNITS')
CORE_NUM=str(multiprocessing.cpu_count()-1)
###############################################################################
while NAME_FLDR_PJCT[-1] == '\\':
  NAME_FLDR_PJCT = NAME_FLDR_PJCT[:-1]
###############################################################################
# Set the workspace and overwrite option
arcpy.env.workspace = "IN_MEMORY"
arcpy.env.overwriteOutput = True
arcpy.CheckOutExtension("Spatial")
SpatRef = arcpy.SpatialReference(int(TRG_CRS))
#################################################################################

#################################################################################
# Set the folder location variables
Cleaned  = NAME_FLDR_PJCT+'\\Source\\Cleaned\\'
DeDuped  = NAME_FLDR_PJCT+'\\Source\\DeDuped\\'
sLAS     = NAME_FLDR_PJCT+'\\Source\\LAS\\'
sLAZ     = NAME_FLDR_PJCT+'\\Source\\LAZ\\'
DeNoised = NAME_FLDR_PJCT+'\\Source\\DeNoised\\'
LclTiles = NAME_FLDR_PJCT+'\\Tiled\\LclTiles\\'
eUNB     = NAME_FLDR_PJCT+'\\Tiled\\UNBuffered\\'
hUNB     = NAME_FLDR_PJCT+'\\HAG\\UNBuffered\\'
Meta     = NAME_FLDR_PJCT+'\\Metadata\\'
Prod     = NAME_FLDR_PJCT+'\\Product\\'
BEFRS    = NAME_FLDR_PJCT+'\\Product\\BE_FRS\\'
cDens    = NAME_FLDR_PJCT+'\\Product\\Density_Canopy\\'
pDens    = NAME_FLDR_PJCT+'\\Product\\Density_Points\\'
HAG      = NAME_FLDR_PJCT+'\\Product\\HAG\\'
hBins    = NAME_FLDR_PJCT+'\\Product\\Height_Bins\\'
hCounts  = NAME_FLDR_PJCT+'\\Product\\Height_Counts\\'
pLAZ     = NAME_FLDR_PJCT+'\\Tiled\\LAZ\\'
Stats    = NAME_FLDR_PJCT+'\\Product\\Stats\\'
TP50     = NAME_FLDR_PJCT+'\\Product\\HghtCentiles\\'
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
###############################################################################
if __name__ == "__main__":
  BigCount = int(0)
###############################################################################
### SETUP PROJECT FOLDERS ###
  subs = [Meta, Prod, hUNB, pLAZ, BEFRS, cDens, pDens, HAG, hBins, hCounts, Stats, TP50, Cleaned, DeDuped, sLAS, sLAZ, DeNoised, LclTiles, eUNB]
  for i in subs:
    if not os.path.exists(i):
      os.makedirs(i)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
###############################################################################
##
### BEGIN HEIGHT-ABOVE-GROUND PROCEDURES ###
##
  print('_'*(42+len(NAME_PJCT)))
  print('')
  print('BEGINNING CANOPY METRICS PROCESSING FOR:  '+NAME_PJCT)
  print('_'*(42+len(NAME_PJCT)))
  print('')
  print('PREPARING POINT CLOUD DATA FOR PROCESSING')
  print('_'*41)
  print('')
##############################################################################
  # Move files into sub-folders
  print('Moving source files to processing folder ...')
  for fil in os.listdir(NAME_FLDR_PJCT):
    if fil.endswith('.las'):
      shutil.move(NAME_FLDR_PJCT+'\\'+fil, sLAS+fil)
    if fil.endswith('.laz'):
      shutil.move(NAME_FLDR_PJCT+'\\'+fil, sLAZ+fil)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Checking whether to compress LAS source files to LAZ ...')
  LAS00_FILES = glob.glob(sLAS+'*.las')
  if len(LAS00_FILES) >= 1:
    CMD=['las2las',
#        '-v',
        '-i',sLAS+'*.las',
		'-olaz',
        '-odir',sLAZ]
    run_process(CMD)
  else:
    print('No LAS source files ... using LAZ files.')
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('De-Duping LAZ files ...')
  CMD=['lasduplicate',
#      '-v',
      '-i',sLAZ+'*.laz',
	  '-olaz',
      '-odir',DeDuped]
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Cleaning LAZ files ...')
  CMD=['las2las',
#      '-v',
      '-i',DeDuped+'*.laz',
      '-odir',Cleaned,
#      '-rescale','0.01','0.01','0.001',
      '-remove_padding',
#      '-remove_all_vlrs',
      '-repair_zero_returns',
#	  '-keep_scan_angle','-15','15',
      '-drop_withheld',
      '-drop_overlap',
#      '-set_version','1.2',
	  '-olaz',
      '-point_type','1']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  if SRC_CRS != TRG_CRS:
    print('ReProjecting LAZ files from EPSG:'+SRC_CRS+' to EPSG:'+TRG_CRS+' ...')
    CMD=['las2las',
        '-i',Cleaned+'*.laz',
        '-epsg', SRC_CRS, '-'+SRC_UNITS, '-elevation_'+SRC_ELEV_UNITS, '-target_epsg', TRG_CRS, '-target_'+TRG_UNITS, 
        '-target_elevation_'+TRG_ELEV_UNITS, '-olaz', '-odir', LclTiles]
    run_process(CMD)
    time_check(time0,time1) ; time1 = time.time()
#################################################################################
  else:
    print('Not projecting, source lidar is already in EPSG:'+TRG_CRS+'.')
    print('Copying /Cleaned/*.LAZ to /LclTiles/*.LAZ ...')
    for filename in os.listdir(Cleaned):
        if filename.endswith('.laz'):
            shutil.copy(Cleaned+filename, LclTiles)
    time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Flagging Noise ...')
  CMD=['lasnoise',
#       '-v',
       '-i',LclTiles+'*.laz',
       '-odir',DeNoised,
       '-ignore_class','2','6','7','8','9','10','11','12','13','14','15','16','17','18'
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
#       '-v',
       '-i',DeNoised+'*.laz',
       '-o',eUNB+NAME_PJCT+'.laz']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Reformatting file names ...')
  NAMES_FILES_BUF = glob.glob(eUNB + '*.laz')
  for NAME_FILE in NAMES_FILES_BUF:
    # Extract coordinates from filename
    BASE_NAME = (NAME_FILE.split('\\')[-1])[:-4]
    BASE_NAME_SPLIT = BASE_NAME.split('_')##    try:
    try:
      # Convert coordinates from strings to integers and trim zeroes
      COORDS = [int(i)/1000 for i in BASE_NAME_SPLIT[-2:]]
      DIRX = 'w' if COORDS[0] < 0 else 'e'
      DIRY = 's' if COORDS[1] < 0 else 'n'
      TOKES = [str(abs(i)) for i in COORDS]
      TOKES[0] = TOKES[0]+DIRX
      TOKES[1] = TOKES[1]+DIRY
      # Create reformatted tile names from coordinates
      TILE_NAME = '_'.join(BASE_NAME_SPLIT[:-2]+TOKES)
      NAME_FILE_TILE = NAME_FILE.replace(BASE_NAME,TILE_NAME)
      NAME_FILE_TILE = NAME_FILE_TILE.split('\\')[-1]
      shutil.copyfile(NAME_FILE, pLAZ+NAME_FILE_TILE)
    except ValueError:
      # ValueError may occur if coordinates are not read correctly
      print(BASE_NAME + '.laz could not be reformatted.')
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Creating *UNBuffered* HAG LAZ files ...')
## ALL Downstream HAG processes are based on unbuffered tiles
  CMD=['lasheight',
       '-i',pLAZ+'*.laz',
       '-odir',hUNB,
       '-cores',CORE_NUM,
	   '-ignore_class','6','7','8','9','10','11','12','13','14','15','16'
	   '-drop_above',HAG_MAX,
       '-replace_z',
	   '-skip_files',
       '-odix','_HAG',
       '-olaz']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
###############################################################################
  print('_'*41)
  print('')
  print('GENERATING LIDAR CANOPY METRICS PRODUCTS')
  print('_'*41)
  print('')
###############################################################################
  print('Generating BE @ %sm ...'%SML_GRD_SPC)
  CMD=['lasgrid',
#       '-v',
       '-i',pLAZ+'*.laz',
       '-o',BEFRS+NAME_PJCT+'_BE%sm.img'%SML_GRD_SPC,
       '-merged',
       '-elevation',
       '-average',
       '-step',SML_GRD_SPC,
       '-fill','5',
#       '-last_only',
       '-keep_class','2']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating BE @ %sm ...'%LRG_GRD_SPC)
  CMD=['lasgrid',
#       '-v',
       '-i',pLAZ+'*.laz',
       '-o',BEFRS+NAME_PJCT+'_BE%sm.img'%LRG_GRD_SPC,
       '-merged',
       '-elevation',
       '-average',
       '-step',LRG_GRD_SPC,
#       '-fill','5',
#       '-last_only',
       '-keep_class','2']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating FRS @ %sm ...'%SML_GRD_SPC)
  CMD=['lasgrid',
#       '-v',
       '-i',pLAZ+'*.laz',
       '-o',BEFRS+NAME_PJCT+'_FRS%sm.img'%SML_GRD_SPC,
       '-merged',
       '-elevation',
       '-highest',
       '-step',SML_GRD_SPC,
       '-fill','5',
       '-first_only',
       '-drop_z_above',ELEV_MAX,
       '-drop_class','0','7','12','15']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating FRS @ %sm ...'%LRG_GRD_SPC)
  CMD=['lasgrid',
#       '-v',
       '-i',pLAZ+'*.laz',
       '-o',BEFRS+NAME_PJCT+'_FRS%sm.img'%LRG_GRD_SPC,
       '-merged',
       '-elevation',
       '-highest',
       '-step',LRG_GRD_SPC,
#       '-fill','5',
       '-first_only',
       '-drop_z_above',ELEV_MAX,
       '-drop_class','0','7','12','15']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating HAG MAX @ %sm ...' %SML_GRD_SPC)
  CMD=['lasgrid',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',HAG+NAME_PJCT+'_HAG%smMAX.img'%SML_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-step',SML_GRD_SPC,
       '-elevation',
       '-highest',
       '-keep_class','1']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating HAG AVG @ %sm ...' %SML_GRD_SPC)
  CMD=['lasgrid',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',HAG+NAME_PJCT+'_HAG%smAVG.img'%SML_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-step',SML_GRD_SPC,
       '-elevation',
       '-average',
       '-keep_class','1']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating HAG STD @ %sm ...' %SML_GRD_SPC)
  CMD=['lasgrid',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',HAG+NAME_PJCT+'_HAG%smSTD.img'%SML_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-step',SML_GRD_SPC,
       '-elevation',
       '-std',
       '-keep_class','1']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating HAG COV @ %sm ...' %LRG_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',HAG+NAME_PJCT+'_HAG%smCOV.img'%LRG_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-step',LRG_GRD_SPC,
       '-keep_class','1',
       '-cov']
  run_process(CMD)
  shutil.move(HAG+NAME_PJCT+'_HAG%smCOV_cov.img'%LRG_GRD_SPC,
              HAG+NAME_PJCT+'_HAG%smCOV.img'%LRG_GRD_SPC)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating HAG GAP @ %sm ...' %LRG_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',HAG+NAME_PJCT+'_HAG%smGAP.img'%LRG_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-step',LRG_GRD_SPC,
       '-keep_class','1',
	   '-cov',
       '-gap']
  run_process(CMD)
  shutil.move(HAG+NAME_PJCT+'_HAG%smGAP_cov_gap.img'%LRG_GRD_SPC,
              HAG+NAME_PJCT+'_HAG%smGAP.img'%LRG_GRD_SPC)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Percentiles @ %sm ...' %LRG_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',TP50+NAME_PJCT+'_HAG%smCENT.img'%LRG_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-height_cutoff','0',
       '-clamp_z_below','0.0',
       '-keep_class','1',
       '-step',LRG_GRD_SPC,
       '-p','10','20','30','40','50','60','70','80','90','98']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Percentiles @ %sm ...' %SML_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',TP50+NAME_PJCT+'_HAG%smCENT.img'%SML_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-height_cutoff','0',
       '-clamp_z_below','0.0',
       '-keep_class','1',
       '-step',SML_GRD_SPC,
       '-p','10','20','30','40','50','60','70','80','90','98']
  run_process(CMD)
##  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Statistics @ %sm ...' %LRG_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',Stats+NAME_PJCT+'_HAG%smSTATS.img'%LRG_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-height_cutoff','0',
       '-clamp_z_below','0.0',
       '-step',LRG_GRD_SPC,
       '-keep_class','1',
       '-min','-max','-avg','-std','-ske','-kur','-qav']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Statistics @ %sm ...' %SML_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',Stats+NAME_PJCT+'_HAG%smSTATS.img'%SML_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-height_cutoff','0',
       '-clamp_z_below','0.0',
       '-step',SML_GRD_SPC,
       '-keep_class','1',
       '-min','-ske','-kur','-qav']
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Height Densities @ %sm ...' %LRG_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',hBins+NAME_PJCT+'_HAG%smHDens.img'%LRG_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
#       '-height_cutoff','0',
       '-step',LRG_GRD_SPC,
       '-keep_class','1',
       '-d','0.01','2','5','10','20','30',HAG_MAX]
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Height Densities @ %sm ...' %SML_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',hBins+NAME_PJCT+'_HAG%smHDens.img'%SML_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-height_cutoff','0',
       '-step',SML_GRD_SPC,
       '-keep_class','1',
       '-d','0.01','2','5','10','20','30',HAG_MAX]
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Height Counts @ %sm ...' %LRG_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',hCounts+NAME_PJCT+'_HAG%smCount.img'%LRG_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-height_cutoff','0',
       '-step',LRG_GRD_SPC,
       '-keep_class','1',
       '-c','0.01','2','5','10','20','30',HAG_MAX]
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Height Counts @ %sm ...' %SML_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',hCounts+NAME_PJCT+'_HAG%smCount.img'%SML_GRD_SPC,
       '-drop_z_below','0',
       '-drop_z_above',HAG_MAX,
       '-merged',
       '-height_cutoff','0',
       '-step',SML_GRD_SPC,
       '-keep_class','1',
       '-c','0.01','2','5','10','20','30',HAG_MAX]
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Canopy Density @ %sm ...' %LRG_GRD_SPC)
  CMD=['lascanopy',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',cDens+NAME_PJCT+'_HAG%smCanopyDens.img'%LRG_GRD_SPC,
       '-keep_class','1','2',
       '-merged',
       '-step',LRG_GRD_SPC,
       '-dns',
       '-nodata','0']
  run_process(CMD)
  shutil.move(cDens+NAME_PJCT+'_HAG%smCanopyDens_dns.img'%LRG_GRD_SPC,
              cDens+NAME_PJCT+'_HAG%smCanopyDens.img'%LRG_GRD_SPC)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Vertical Distribution Ratios @ %sm ...' %LRG_GRD_SPC)
  HOME =  arcpy.Raster(TP50+NAME_PJCT+"_HAG%smCENT_p50.img"%LRG_GRD_SPC)
  CH98 =  arcpy.Raster(TP50+NAME_PJCT+"_HAG%smCENT_p98.img"%LRG_GRD_SPC)
  CH100 = arcpy.Raster(Stats+NAME_PJCT+"_HAG%smSTATS_max.img"%LRG_GRD_SPC)
  VDR98 = (CH98 - HOME) / CH98
  VDR98.save(NAME_FLDR_PJCT + "\\Product\\Stats\\"+
             NAME_PJCT+"_HAG%smVDR98.img"%LRG_GRD_SPC)
  VDR100 = (CH100 - HOME) / CH100
  VDR100.save(NAME_FLDR_PJCT + "\\Product\\Stats\\"+
              NAME_PJCT+"_HAG%smVDR100.img"%LRG_GRD_SPC)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Generating Point Density and Binary ...' )
  DensOut = pDens+NAME_PJCT+"_DEN%sm.img"%SML_GRD_SPC
  BinOut  = pDens+NAME_PJCT+"_BIN%sm.img"%SML_GRD_SPC
  CMD=['lasgrid',
#       '-v',
       '-i',hUNB+'*.laz',
       '-o',DensOut,
       '-counter_16bit',
       '-merged',
       '-step',SML_GRD_SPC]
  run_process(CMD)
  arcpy.CalculateStatistics_management(DensOut)
  # Slice the Density image to Binary
  remap = RemapRange([[0,1,"NODATA"],[1,65535,1]])
  BinRas = Reclassify(DensOut, "VALUE", remap, "NODATA")
  BinRas.save(BinOut)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
  print('Building Stats and Pyramids for All Images ...')
  images = []
  for root,dirs,files in os.walk(Prod):
  	for filename in files:
  		if filename.endswith('.img'):
  			f = os.path.join(root,filename)
  			images = images + [f]
  for thisfile in images:
  	arcpy.DefineProjection_management(thisfile,SpatRef)
  	arcpy.CalculateStatistics_management(thisfile)
  	arcpy.BuildPyramids_management(thisfile)
  time_check(time0,time1) ; time1 = time.time()
#################################################################################
#################################################################################
  #   Spread the joyous news!
  print('')
  print('_'*(41+len(NAME_PJCT)))
  print('')
  print('CANOPY METRICS PROCESSING COMPLETED FOR: '+NAME_PJCT)
  print('_'*(41+len(NAME_PJCT)))
  print('')
  print('')
sys.exit()
