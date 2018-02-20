## HAGCanopyMetrics_3'11.py
## 19SEP17 10:00
###############################################################################
## RUN THE CODE IN CMD.EXE WINDOW AND NOT IN PYTHON IDLE
###############################################################################
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
## EXTERNAL    :: arcpy, grep (direct in o/s - UnixUtils recommended)
## DEPENDENCIES::
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
## Changes in 3'11:
##   - Changed product folder arrangement and some names.
##   - ALL SOURCE FILES MUST BE .LAZ
##   - ALL SOURCE FILES MUST INITIALLY BE AT THE ROOT OF THE PROJECT FOLDER
###############################################################################
###############################################################################
##
##
if __name__ != "__main__":
  print('')
  print('This code must be run as a stand-alone routine. It cannot be imported.')
  print('Exiting the script.')
  print('')
  sys.exit()
###############################################################################
##>>LOAD MODULES FOR PYTHON
print('')
print('')
print('Loading Python modules and configuring the workspace ...')
print('  time')
import time
time0 = time.time()
time1 = time0
print('    gmtime')
from time import gmtime
print('    strftime')
from time import strftime
print('  os')
import os
print('    path')
from os import path
print('  glob')
import glob
print('  fnmatch')
import fnmatch
print('  traceback')
import traceback
print('  multiprocessing')
import multiprocessing
print('  string')
import string
print('  sys')
import sys
print('    exit')
from sys import exit
print('  subprocess')
import subprocess
print('    check_call')
from subprocess import check_call
print('    check_output')
from subprocess import check_output
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
###############################################################################
def run_process(cmd, prnt=False):
  pipeout = subprocess.PIPE
  si = subprocess.STARTUPINFO()
  si.dwFlags = subprocess.STARTF_USESTDHANDLES | subprocess.SW_HIDE
  pro = subprocess.Popen(cmd, stdout=pipeout, startupinfo=si)
  out, err = pro.communicate()
###############################################################################
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
def img_crs(OutFldr):
  images = []
  for root,dirs,files in os.walk(OutFldr):
    for filename in files:
      if filename.endswith('.tif'):
        f = os.path.join(root,filename)
        arcpy.DefineProjection_management(f,SpatRef)
        arcpy.CalculateStatistics_management(f)
        arcpy.BuildPyramids_management(f)
###############################################################################
##>>READ CONFIGURATION FILE
print('Reading Configuration File ...')
config   = configparser.RawConfigParser()
CFG_FILE = sys.argv[1]
if not os.path.isfile(CFG_FILE):
  print('!!  Configuration File Not Found  !!')
  sys.exit()
config.read(CFG_FILE)
SECTION1    = 'Section1'
FLDR_NAME   = config.get(SECTION1, 'FLDR_NAME')

##>>THE FOLLOWING PARAMETERS WILL VARY WITH THE NATURE OF THE LIDAR DATA
ELEV_MAX    = config.get(SECTION1, 'ELEV_MAX')
HAG_MAX     = config.get(SECTION1, 'HAG_MAX')
NOISE_ISO   = config.get(SECTION1, 'NOISE_ISO')
NOISE_XY    = config.get(SECTION1, 'NOISE_XY')
NOISE_Z     = config.get(SECTION1, 'NOISE_Z')
DEM_REZ     = config.get(SECTION1, 'DEM_REZ')

##>>UNITS and ZUNITS  -MUST-  BE ONE OF: 'meter', 'feet', 'survey_feet' (WITHOUT QUOTES)
SRC_CRS     = config.get(SECTION1, 'SRC_CRS')   # The EPSG Code for the source CRS
SRC_UNITS   = config.get(SECTION1, 'SRC_UNITS').lower()
SRC_ZUNITS  = config.get(SECTION1, 'SRC_ZUNITS').lower()

##>>THE FOLLOWING PARAMETERS SHOULD NOT NEED TO BE CHANGED FOR CDI PROJECTS.
##>>THEY ARE INCLUDED IN THE CFG FILE TO ALLOW THIS SCRIPT TO BE USED ON OTHER
##>>NON-CDI PROJECTS.
TRGT_CRS    = config.get(SECTION1, 'TRGT_CRS')            # 6350 =Albers Eq Area CONUS USGS NAD83(2011)
TRGT_UNITS  = config.get(SECTION1, 'TRGT_UNITS').lower()  # meters
TRGT_ZUNITS = config.get(SECTION1, 'TRGT_ZUNITS').lower() # meters
COARSE_REZ  = config.get(SECTION1, 'COARSE_REZ')          # 25
FINE_REZ    = config.get(SECTION1, 'FINE_REZ')            # 10
STATS       = config.get(SECTION1, 'STATS').lower()       # min, max, avg, std, ske, kur, qav
CENTS       = config.get(SECTION1, 'CENTS')               # 10 20 30 40 50 60 70 80 90 98
BINS        = config.get(SECTION1, 'BINS')                # 5 10 15 20 25 30 ... n (by 5s; n based on max veg height)
FLOOR       = config.get(SECTION1, 'FLOOR')               # 0.10
BREST       = config.get(SECTION1, 'BREST')               # 1.37
CORE_NUM    = str(multiprocessing.cpu_count()-1)
###############################################################################
print('  Using '+CORE_NUM+' cores for processing')
###############################################################################
while FLDR_NAME[-1] == '\\':
  FLDR_NAME = FLDR_NAME[:-1]
PJCT_NAME = (FLDR_NAME.split('\\')[-1])
###############################################################################
##>>Set the workspace and overwrite option
print('Setting up the workspace ...')
arcpy.env.workspace = "IN_MEMORY"
arcpy.env.overwriteOutput = True
arcpy.CheckOutExtension("Spatial")
SpatRef = arcpy.SpatialReference(int(TRGT_CRS))
###############################################################################
##>>Set the folder location variables
SRC       = FLDR_NAME+'\\SRC'
Source    =   SRC+'\\1Source\\'
Duped     =   SRC+'\\2Duped\\'
Cleaned   =   SRC+'\\3Cleaned\\'
Scaled    =   SRC+'\\4Scaled\\'
Noised    =   SRC+'\\5Noised\\'
#-----------------------------------
LAZ       = FLDR_NAME+'\\LAZ'
LAZe      =   LAZ+'\\Elev\\'
LAZh      =   LAZ+'\\Hght\\'
#-----------------------------------
ELV       = FLDR_NAME+'\\tiles'
ELVc      =   ELV+'\\%sm\\BE_FRS\\'%COARSE_REZ
ELVf      =   ELV+'\\%sm\\BE_FRS\\'%FINE_REZ
ELVd      =   ELV+'\\%sm\\BE_FRS\\'%DEM_REZ
#-----------------------------------
VEG       = FLDR_NAME+'\\tiles'
VEGc      =   VEG+'\\%sm\\'%COARSE_REZ
VEGcC     =     VEGc+'\\canopy\\'
VEGcR     =     VEGc+'\\ratios\\'
VEGcS     =     VEGc+'\\stats\\'
VEGcHc    =     VEGc+'\\height_cnt\\'
VEGcHd    =     VEGc+'\\height_den\\'
VEGcHp    =     VEGc+'\\height_pct\\'
VEGf      =   VEG+'\\%sm\\'%FINE_REZ
VEGfC     =     VEGf+'\\canopy\\'
VEGfR     =     VEGf+'\\ratios\\'
VEGfS     =     VEGf+'\\stats\\'
VEGfHd    =     VEGf+'\\height_cnt\\'
VEGfHc    =     VEGf+'\\height_den\\'
VEGfHp    =     VEGf+'\\height_pct\\'
#-----------------------------------
MET       = FLDR_NAME+'\\META\\'
STK       = FLDR_NAME+'\\STKS\\'
OutFldr   = ''
###############################################################################
subs = [SRC, Source, Duped, Cleaned, Scaled, Noised,
        LAZ, LAZe, LAZh,
        ELV, ELVc, ELVf, ELVd,
        VEG, VEGc, VEGcC, VEGcS, VEGcR, VEGcHd, VEGcHc, VEGcHp,
             VEGf, VEGfC, VEGfS, VEGfR, VEGfHd, VEGfHc, VEGfHp]
for i in subs:
  if not os.path.exists(i):
    os.makedirs(i)
time_check(time0,time1) ; time1 = time.time()
###############################################################################
###############################################################################
#
###>>BEGIN HEIGHT-ABOVE-GROUND PROCEDURES<<##
#
###############################################################################
###############################################################################
print('_'*(42+len(PJCT_NAME)))
print('')
print('BEGINNING CANOPY METRICS PROCESSING FOR:  '+PJCT_NAME)
print('_'*(42+len(PJCT_NAME)))
print('')
print('PREPARING POINT CLOUD DATA FOR PROCESSING')
print('_'*42)
print('')
###############################################################################
###>>Gather LAZ files into Source folder
###>>Lowercase all the  extensions
print('Gathering LAZ files into the Source folder ...')
for f3 in os.listdir(FLDR_NAME):
  if f3.lower().endswith('.laz'):
    f4=f3[:-4]
    os.rename(FLDR_NAME+'\\'+f3,Source+f4+'.laz')
if len(os.listdir(Source)) == 0:
  print('')
  print('No LAZ files were found in the project.')
  print('Exiting the script.')
  sys.exit()
time_check(time0,time1) ; time1 = time.time()
###############################################################################
print('De-Duping LAZ files ...')
CMD='lasduplicate'+\
    ' -i '+Source+'*.laz'+\
    ' -odir '+Duped+\
    ' -olaz'+\
    ' -cores '+CORE_NUM
run_process(CMD)
time_check(time0,time1) ; time1 = time.time()
###############################################################################
print('Cleaning LAS files ...')
CMD='las2las'+\
    ' -i '+Duped+'*.laz'+\
    ' -odir '+Cleaned+\
    ' -olaz'+\
    ' -cores '+CORE_NUM+\
    ' -remove_padding'+\
    ' -remove_all_vlrs'+\
    ' -repair_zero_returns'+\
    ' -set_version '+'1.2'+\
    ' -point_type '+'1'
run_process(CMD)
time_check(time0,time1) ; time1 = time.time()
###############################################################################
if SRC_CRS == TRGT_CRS and SRC_UNITS == TRGT_UNITS and SRC_ZUNITS == TRGT_ZUNITS:
  print('Source lidar is already in EPSG:'+TRGT_CRS+'. Skipping reprojection.')
  print('ReScaling and ReOffsetting LAZ files ...')
  CMD='las2las'+\
      ' -i '+Cleaned+'*.laz'+\
      ' -odir '+Scaled+\
      ' -olaz'+\
      ' -cores '+CORE_NUM+\
      ' -rescale 0.01 0.01 0.001'+\
      ' -auto_reoffset'
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
#------------------------------------------------------------------------------
else:
  print('ReProjecting, ReScaling, and ReOffsetting LAZ files ...')
  print('  [EPSG:'+SRC_CRS+' to EPSG:'+TRGT_CRS+']')
  CMD='las2las'+\
      ' -i '+Cleaned+'*.laz'+\
      ' -odir '+Scaled+\
      ' -olaz'+\
      ' -cores '+CORE_NUM+\
      ' -target_precision 0.01'+\
      ' -target_elevation_precision 0.001'+\
      ' -epsg '+SRC_CRS+\
      ' -target_epsg '+TRGT_CRS+\
      ' -'+SRC_UNITS+\
      ' -target_'+TRGT_UNITS+\
      ' -elevation_'+SRC_ZUNITS+\
      ' -target_elevation_'+TRGT_ZUNITS
  run_process(CMD)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
print('Flagging Noise ...')
CMD='lasnoise'+\
    ' -i '+Scaled+'*.laz'+\
    ' -odir '+Noised+\
    ' -olaz'+\
    ' -cores '+CORE_NUM+\
    ' -step_xy '+NOISE_XY+\
    ' -step_z '+NOISE_Z+\
    ' -isolated '+NOISE_ISO
run_process(CMD)
time_check(time0,time1) ; time1 = time.time()
###############################################################################
print('Re-Tiling without Buffer ...')
CMD='lastile'+\
    ' -i '+Noised+'*.laz'+\
    ' -o '+LAZe+PJCT_NAME+'.laz'+\
    ' -olaz'+\
    ' -cores '+CORE_NUM
run_process(CMD)
time_check(time0,time1) ; time1 = time.time()
###############################################################################
print('Reformatting file names ...')
FILENAMES = glob.glob(LAZe+'*.laz')
for FILENAME in FILENAMES:
#>>Extract coordinates from filenames and
#>>create reformatted tile names from coordinates
  BASE_NAME = (FILENAME.split('\\')[-1])[:-4]
  BASE_NAME_SPLIT = BASE_NAME.split('_')
  try:
    COORDS = [int(i)/1000 for i in BASE_NAME_SPLIT[-2:]]
    DIRX = 'w' if COORDS[0] < 0 else 'e'
    DIRY = 's' if COORDS[1] < 0 else 'n'
    TOKES = [str(abs(i)) for i in COORDS]
    TOKE0 = DIRX+'0'*(4-len(TOKES[0]))+TOKES[0]
    TOKE1 = DIRY+'0'*(4-len(TOKES[1]))+TOKES[1]
    NAME_FILE_TILE = PJCT_NAME+'_'+TOKE0+TOKE1+'.laz'
    os.rename(FILENAME, LAZe+NAME_FILE_TILE)
  except ValueError:
    print(BASE_NAME + '.laz could not be reformatted.')
time_check(time0,time1) ; time1 = time.time()
###############################################################################
print('Creating Unbuffered Height LAZ files ...')
CMD='lasheight'+\
    ' -i '+LAZe+'*.laz'+\
    ' -odir '+LAZh+\
    ' -olaz'+\
    ' -cores '+CORE_NUM+\
    ' -replace_z'
run_process(CMD)
time_check(time0,time1) ; time1 = time.time()
################################################################################
################################################################################
#print('_'*44)
#print('')
#print('GENERATING BARE EARTH, FIRST REFLECTIVE, HEIGHT, AND POINT MASK SURFACES')
#print('_'*44)
#print('')
################################################################################
################################################################################

#------------------------------------------------------------------------------
####for REZ in [COARSE_REZ]:
####for REZ in [FINE_REZ,DEM_REZ]:
for REZ in [COARSE_REZ,FINE_REZ,DEM_REZ]:
#------------------------------------------------------------------------------

  OutFldr=ELV+'\\%sm\\BE_FRS'%REZ
  print('Generating Bare Earth Surface @ %sm ...'%REZ)
  CMD='lasgrid'+\
      ' -i '+LAZe+'*.laz'+\
      ' -otif'+\
      ' -odir '+OutFldr+\
      ' -odix _%sm_dem'%REZ+\
      ' -mem 1999'+\
      ' -step '+REZ+\
      ' -last_only'+\
      ' -keep_class 2'+\
      ' -elevation'+\
      ' -average'+\
      ' -fill '+(REZ*2)
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating First Reflective Surface @ %sm ...'%REZ)
  CMD='lasgrid'+\
      ' -i '+LAZe+'*.laz'+\
      ' -otif'+\
      ' -odir '+OutFldr+\
      ' -odix _%sm_dsm'%REZ+\
      ' -mem 1999'+\
      ' -step '+REZ+\
      ' -first_only'+\
      ' -drop_withheld'+\
      ' -drop_class 0 7 12 15'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -elevation'+\
      ' -highest'+\
      ' -fill '+(REZ*2)
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Height Surface @ %sm ...'%REZ)
  CMD='lasgrid'+\
      ' -i '+LAZh+'*.laz'+\
      ' -otif'+\
      ' -odir '+OutFldr+\
      ' -odix _%sm_chm'%REZ+\
      ' -mem 1999'+\
      ' -step '+REZ+\
      ' -drop_withheld'+\
      ' -drop_class 0 7 12 15'+\
      ' -clamp_z_below 0.0'+\
      ' -elevation'+\
      ' -highest'+\
      ' -fill '+(REZ*2)
  run_process(CMD)
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Point Density and Mask @ %sm ...' %REZ)
  CMD='lasgrid'+\
      ' -i '+LAZe+'*.laz'+\
      ' -otif'+\
      ' -odir '+OutFldr+\
      ' -odix _%sm_pnt-den'%REZ+\
      ' -step '+REZ+\
      ' -counter_16bit'+\
      ' -fill '+(REZ*2)
  run_process(CMD)
#------------------------------------------------------------------------------
###>>Calculate raster statistics for the Density rasters
###>>and slice Density rasters to create Mask rasters
  remap=RemapRange([[0,1,"NODATA"],[1,65535,1]])
  for root, dirnames, filenames in os.walk(OutFldr):
    for filename in fnmatch.filter(filenames,'*_pnt-den.tif'):
      DensFile = os.path.join(root,filename)
      MaskFile = string.replace(DensFile,'_pnt-den.tif','_pnt-msk.tif')
      arcpy.CalculateStatistics_management(DensFile)
      MaskRas=Reclassify(DensFile,"VALUE",remap,"NODATA")
      MaskRas.save(MaskFile)
# -------------------------------------------------------------------------------
  img_crs(OutFldr)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
print('Cleaning up Martin\'s useless bonus files...')
for root, dirnames, filenames in os.walk(LAZ):
  for extension in ('*.tif','*.tfw','*.kml'):
    for filename in fnmatch.filter(filenames,extension):
      os.remove(os.path.join(root, filename))
time_check(time0,time1) ; time1 = time.time()
###############################################################################
###############################################################################
print('_'*44)
print('')
print('GENERATING LIDAR CANOPY METRICS PRODUCTS')
print('_'*44)
print('')
###############################################################################
###############################################################################
#>>Establish the BINs for CDI projects, based on
#>>This code clamps the top BIN at 120 meters to prevent BINs based on noise.
print('Calculating BINs for lascanopy calculations')
FILENAMES = glob.glob(LAZh+'*.laz')
MaxZ=float(0.0)
BIN_LIST=[5]
BIN=10
TOP=125  #Set this to 75 or 125 to ensure BINs include 70 or 120
for FILENAME in FILENAMES:
  Maxes=subprocess.check_output('lasinfo -nc -nv -stdout -i '+FILENAME+' | grep -E "max x y z:"',shell=True)
  ThisZ=float(Maxes.split()[6])
  MaxZ=max(ThisZ,MaxZ)
MaxZ=min(MaxZ+5,TOP)
while BIN <= MaxZ:
	BIN_LIST.append(BIN)
	BIN=BIN + 5
BINS=str.replace(str.replace(str.replace(str(BIN_LIST),',',''),']',''),'[','')
time_check(time0,time1) ; time1 = time.time()

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#>>If the BINs defined in the config file should be used,
#>>UNCOMMENT the following line:
#
#BINS= config.get(SECTION1, 'BINS')
#
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

###############################################################################
###############################################################################

#------------------------------------------------------------------------------
##for REZ in [COARSE_REZ]:
for REZ in [COARSE_REZ,FINE_REZ]:
#------------------------------------------------------------------------------

  print('Generating Canopy Coverage @ %sm ...'%REZ)
  OUT_FLDR=VEG+'\\%sm\\canopy\\'%REZ
  CMD='lascanopy'+\
      ' -i '+LAZh+'*.laz'+\
      ' -otif'+\
      ' -odir '+OUT_FLDR+\
      ' -step '+REZ+\
      ' -keep_class 1 2'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -drop_z_below '+FLOOR+\
      ' -height_cutoff '+BREST+\
      ' -cov'
  run_process(CMD)
  img_crs(OUT_FLDR)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Canopy Density @ %sm ...'%REZ)
  OUT_FLDR=VEG+'\\%sm\\canopy\\'%REZ
  CMD='lascanopy'+\
      ' -i '+LAZh+'*.laz'+\
      ' -otif'+\
      ' -odir '+OUT_FLDR+\
      ' -step '+REZ+\
      ' -keep_class 1 2'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -drop_z_below '+FLOOR+\
      ' -height_cutoff '+BREST+\
      ' -dns'
  run_process(CMD)
  img_crs(OUT_FLDR)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  OUT_FLDR=VEG+'\\%sm\\stats\\'%REZ
  STAT_LIST=STATS.split()
  for STAT in STAT_LIST:
    print('Generating Statistics ('+STAT+') @ %sm ...'%REZ)
    CMD='lascanopy'+\
        ' -i '+LAZh+'*.laz'+\
        ' -otif'+\
        ' -odir '+OUT_FLDR+\
        ' -step '+REZ+\
        ' -keep_class 1'+\
        ' -keep_scan_angle -15 15'+\
        ' -drop_withheld'+\
        ' -drop_z_above '+ELEV_MAX+\
        ' -drop_z_below '+FLOOR+\
        ' -height_cutoff '+FLOOR+\
        ' -'+STAT
    run_process(CMD)
#------------------------------------------------------------------------------
#>>Fix filenames that lastools cannot
    for root, dirnames, filenames in os.walk(OUT_FLDR):
      for filename in fnmatch.filter(filenames,'*%s.tif'%STAT):
        newname=string.replace(filename,'_'+STAT+'.tif','_%sm_h'%REZ+STAT+'.tif')
        os.rename(os.path.join(root, filename),os.path.join(root, newname))
#------------------------------------------------------------------------------
  img_crs(OUT_FLDR)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Height Counts @ %sm ...'%REZ)
  OUT_FLDR=VEG+'\\%sm\\height_cnt\\'%REZ
  CMD='lascanopy'+\
      ' -i '+LAZh+'*.laz'+\
      ' -odir '+OUT_FLDR+\
      ' -otif'+\
      ' -step '+REZ+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -drop_z_below '+FLOOR+\
      ' -height_cutoff '+BREST+\
      ' -c '+BINS
  run_process(CMD)
#------------------------------------------------------------------------------
#>>Fix filenames that lastools cannot
  NDX = 0
  for BIN in BIN_LIST:
    NDXSTR = '_c{:0>2}'.format(NDX)
    BINSTR = '_hcnt{:0>3}'.format(BIN)
    NDX = NDX+1
    for root, dirnames, filenames in os.walk(OUT_FLDR):
      for filename in fnmatch.filter(filenames,'*%s.tif'%NDXSTR):
        newname=string.replace(filename,NDXSTR+'.tif','_%sm'%REZ+BINSTR+'.tif')
        os.rename(os.path.join(root, filename),os.path.join(root, newname))
#------------------------------------------------------------------------------
  img_crs(OUT_FLDR)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Height Densities @ %sm ...'%REZ)
  OUT_FLDR=VEG+'\\%sm\\height_den\\'%REZ
  CMD='lascanopy'+\
      ' -i '+LAZh+'*.laz'+\
      ' -odir '+OUT_FLDR+\
      ' -otif'+\
      ' -step '+REZ+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -drop_z_below '+FLOOR+\
      ' -height_cutoff '+BREST+\
      ' -d '+BINS
  run_process(CMD)
#------------------------------------------------------------------------------
#>>Fix filenames that lastools cannot
  NDX = 0
  for BIN in BIN_LIST:
    NDXSTR = '_d{:0>2}'.format(NDX)
    BINSTR = '_hden{:0>3}'.format(BIN)
    NDX = NDX+1
    for root, dirnames, filenames in os.walk(OUT_FLDR):
      for filename in fnmatch.filter(filenames,'*%s.tif'%NDXSTR):
        newname=string.replace(filename,NDXSTR+'.tif','_%sm'%REZ+BINSTR+'.tif')
        os.rename(os.path.join(root, filename),os.path.join(root, newname))
#------------------------------------------------------------------------------
  img_crs(OUT_FLDR)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Height Percentiles @ %sm ...'%REZ)
  OUT_FLDR=VEG+'\\%sm\\height_pct\\'%REZ
  CMD='lascanopy'+\
      ' -i '+LAZh+'*.laz'+\
      ' -odir '+OUT_FLDR+\
      ' -otif'+\
      ' -step '+REZ+\
      ' -keep_class 1'+\
      ' -keep_scan_angle -15 15'+\
      ' -drop_withheld'+\
      ' -drop_z_above '+ELEV_MAX+\
      ' -drop_z_below '+FLOOR+\
      ' -height_cutoff '+BREST+\
      ' -p '+CENTS
  run_process(CMD)
#------------------------------------------------------------------------------
#>>Fix filenames that lastools cannot
  CENT_LIST=CENTS.split()
  for CENT in CENT_LIST:
    NDXSTR = '_p{:0>2}'.format(CENT)
    CENTSTR = '_hpct{:0>2}'.format(CENT)
    for root, dirnames, filenames in os.walk(OUT_FLDR):
      for filename in fnmatch.filter(filenames,'*%s.tif'%NDXSTR):
        newname=string.replace(filename,NDXSTR+'.tif','_%sm'%REZ+CENTSTR+'.tif')
        os.rename(os.path.join(root, filename),os.path.join(root, newname))
##-----------------------------------------------------------------------------
  img_crs(OUT_FLDR)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
  print('Generating Vertical Distribution Ratios @ %sm ...'%REZ)
  STATFLDR=VEG+'\\%sm\\stats\\'%REZ
  PCNTFLDR=VEG+'\\%sm\\height_pct\\'%REZ
  OUT_FLDR=VEG+'\\%sm\\ratios\\'%REZ
  for root, dirnames, filenames in os.walk(STATFLDR):
    for filename in fnmatch.filter(filenames,'*_hmax.tif'):
      fnHMX    = os.path.join(STATFLDR,filename)
      fnP50    = os.path.join(PCNTFLDR,string.replace(filename,'_hmax.tif','_hpct50.tif'))
      fnP98    = os.path.join(PCNTFLDR,string.replace(filename,'_hmax.tif','_hpct98.tif'))
      fnVDR98  = os.path.join(OUT_FLDR,string.replace(filename,'_hmax.tif','_vdr98.tif'))
      fnVDR100 = os.path.join(OUT_FLDR,string.replace(filename,'_hmax.tif','_vdr100.tif'))
      P50    = arcpy.Raster(fnP50)
      P98    = arcpy.Raster(fnP98)
      HMX    = arcpy.Raster(fnHMX)
      VDR98 = (P98-P50)/P98
      VDR98.save(fnVDR98)
      VDR100 = (HMX-P50)/HMX
      VDR100.save(fnVDR100)
  img_crs(OUT_FLDR)
  time_check(time0,time1) ; time1 = time.time()
###############################################################################
###############################################################################
print('Cleaning up Martin and Esri\'s useless bonus files...')
for root, dirnames, filenames in os.walk(FLDR_NAME+'\\tiles'):
  for extension in ('*.kml','*.tfw'):
    for filename in fnmatch.filter(filenames,extension):
      os.remove(os.path.join(root, filename))
time_check(time0,time1) ; time1 = time.time()
###############################################################################
###############################################################################
#>>Spread the joyous news!
print('')
print('_'*(41+len(PJCT_NAME)))
print('')
print('CANOPY METRICS PROCESSING COMPLETED FOR: '+PJCT_NAME)
print('_'*(41+len(PJCT_NAME)))
print('')
print('')
exit(0)
