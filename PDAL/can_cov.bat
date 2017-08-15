pdal pipeline "%~dp0\las_2_veg_fr_pnt_cnt.json"
pdal pipeline "%~dp0\las_2_all_fr_pnt_cnt.json"
gdal_calc --calc="A/B*100" -A "C:/Temp/pdal_veg_fr_pnt_cnt.tif" -B "C:/Temp/pdal_all_fr_pnt_cnt.tif" --outfile="C:/Temp/gdal_can_cov.tif"
