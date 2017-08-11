pdal pipeline "C:\Users\nfkruska\Documents\GitHub\LidarVegMetrics\PDAL\las_2_dem.json"
gdal_translate -a_srs EPSG:2283 C:/Temp/pdal_dem.tif C:/Temp/pdal_dem2.tif
gdal_fillnodata.py -md 10 -si 3 "C:/Temp/pdal_dem2.tif"
