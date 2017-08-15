pdal pipeline "%~dp0\las_2_dem.json"
gdal_fillnodata.py -md 10 -si 3 "C:/Temp/pdal_dem2.tif"
