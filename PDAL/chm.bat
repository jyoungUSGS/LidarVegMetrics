pdal pipeline "%~dp0\las_2_dem.json"
pdal pipeline "%~dp0\las_2_frs.json"
gdal_calc.py --calc="A-B" -A "C:/Temp/pdal_frs.tif" -B "C:/Temp/pdal_dem.tif" --outfile="C:/Temp/gdal_chm.tif"
