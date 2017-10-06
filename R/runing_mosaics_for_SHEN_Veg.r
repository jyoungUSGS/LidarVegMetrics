setwd("G:\\CDI2017")

source("G:\\CDI2017\\Scripts\\R\\mosaic_list_of_rasters.r")


exts <- matrix(c("geotiff", "image", "csv tables", "all files","shapefiles","*.tif", "*.img", "*.csv", "*.*", "*.shp"), nrow = 5, ncol = 2)

#AVR
if (interactive())     ras1<-choose.files(filters = exts)

# max
if (interactive())     ras2<-choose.files(filters = exts)

# STD
if (interactive())     ras3<-choose.files(filters = exts)

# BE
if (interactive())     ras4<-choose.files(filters = exts)


# FRS
if (interactive())     ras5<-choose.files(filters = exts)

# DCAN
if (interactive())     ras6<-choose.files(filters = exts)

# DEN
if (interactive())     ras7<-choose.files(filters = exts)

# BIN
if (interactive())     ras8<-choose.files(filters = exts)

# COV 24m
if (interactive())     ras9<-choose.files(filters = exts)

# GAP 24m
if (interactive())     ras10<-choose.files(filters = exts)

# Hbin 0
if (interactive())     ras11<-choose.files(filters = exts)

# Hbin 1
if (interactive())     ras12<-choose.files(filters = exts)

# Hbin 2
if (interactive())     ras13<-choose.files(filters = exts)

# Hbin 3
if (interactive())     ras14<-choose.files(filters = exts)

# Hbin 4
if (interactive())     ras15<-choose.files(filters = exts)

# Hbin 5
if (interactive())     ras16<-choose.files(filters = exts)

# Hcount 0
if (interactive())     ras17<-choose.files(filters = exts)

# Hcount 1
if (interactive())     ras18<-choose.files(filters = exts)

# Hcount 2
if (interactive())     ras19<-choose.files(filters = exts)

# Hcount 3
if (interactive())     ras20<-choose.files(filters = exts)

# Hcount 4
if (interactive())     ras21<-choose.files(filters = exts)

# Hcount 5
if (interactive())     ras22<-choose.files(filters = exts)



crs1 <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#AVR
mos1 <- mosaicList(ras1, overlapping = "mean")
crs(mos1) <- crs(crs1)
writeRaster(mos1, "G:\\CDI2017\\Mosaics\\HAG\\Shen_AVR_mosaic.tif")

# max
#if (interactive())     ras2<-choose.files(filters = exts)
mos2 <- mosaicList(ras2, overlapping = "mean")
crs(mos2) <- crs(crs1)
writeRaster(mos2, "G:\\CDI2017\\Mosaics\\HAG\\Shen_MAX_mosaic.tif")

# STD
#if (interactive())     ras3<-choose.files(filters = exts)
mos3 <- mosaicList(ras3, overlapping = "mean")
crs(mos3) <- crs(crs1)
writeRaster(mos3, "G:\\CDI2017\\Mosaics\\HAG\\Shen_STD_mosaic.tif")

# BE
#if (interactive())     ras4<-choose.files(filters = exts)
mos4 <- mosaicList(ras4, overlapping = "mean")
crs(mos4) <- crs(crs1)
writeRaster(mos4, "G:\\CDI2017\\Mosaics\\BE_FRS\\Shen_BE1_mosaic.tif")

# FRS
#if (interactive())     ras5<-choose.files(filters = exts)
mos5 <- mosaicList(ras5, overlapping = "mean")
crs(mos5) <- crs(crs1)
writeRaster(mos5, "G:\\CDI2017\\Mosaics\\BE_FRS\\Shen_FRS_mosaic.tif")

# DCAN
#if (interactive())     ras5<-choose.files(filters = exts)
mos6 <- mosaicList(ras6, overlapping = "mean")
crs(mos6) <- crs(crs1)
writeRaster(mos6, "G:\\CDI2017\\Mosaics\\DENS\\Shen_DCAN_24m__mosaic.tif")

# DENS
#if (interactive())     ras5<-choose.files(filters = exts)
mos7 <- mosaicList(ras7, overlapping = "mean")
crs(mos7) <- crs(crs1)
writeRaster(mos7, "G:\\CDI2017\\Mosaics\\DENS\\Shen_DENS_mosaic.tif")

# BiN
#if (interactive())     ras5<-choose.files(filters = exts)
mos8 <- mosaicList(ras8, overlapping = "mean")
crs(mos8) <- crs(crs1)
writeRaster(mos8, "G:\\CDI2017\\Mosaics\\DENS\\Shen_BIN_mosaic.tif")

# COV 24m
#if (interactive())     ras5<-choose.files(filters = exts)
mos9 <- mosaicList(ras9, overlapping = "mean")
crs(mos9) <- crs(crs1)
writeRaster(mos9, "G:\\CDI2017\\Mosaics\\HAG\\Shen_COV_24m__mosaic.tif")

# GAP 24m
#if (interactive())     ras5<-choose.files(filters = exts)
mos10 <- mosaicList(ras10, overlapping = "mean")
crs(mos10) <- crs(crs1)
writeRaster(mos10, "G:\\CDI2017\\Mosaics\\HAG\\Shen_GAP_24m_mosaic.tif")

# HBiN 0
#if (interactive())     ras5<-choose.files(filters = exts)
mos11 <- mosaicList(ras11, overlapping = "mean")
crs(mos11) <- crs(crs1)
writeRaster(mos11, "G:\\CDI2017\\Mosaics\\H_BINS\\Shen_HBIN_0_mosaic.tif")

# HBiN 1
#if (interactive())     ras5<-choose.files(filters = exts)
mos12 <- mosaicList(ras12, overlapping = "mean")
crs(mos12) <- crs(crs1)
writeRaster(mos12, "G:\\CDI2017\\Mosaics\\H_BINS\\Shen_HBIN_1_mosaic.tif")

# HBiN 2
#if (interactive())     ras5<-choose.files(filters = exts)
mos13 <- mosaicList(ras13, overlapping = "mean")
crs(mos13) <- crs(crs1)
writeRaster(mos13, "G:\\CDI2017\\Mosaics\\H_BINS\\Shen_HBIN_2_mosaic.tif")

# HBiN 3
#if (interactive())     ras5<-choose.files(filters = exts)
mos14 <- mosaicList(ras14, overlapping = "mean")
crs(mos14) <- crs(crs1)
writeRaster(mos14, "G:\\CDI2017\\Mosaics\\H_BINS\\Shen_HBIN_3_mosaic.tif")

# HBiN 4
#if (interactive())     ras5<-choose.files(filters = exts)
mos15 <- mosaicList(ras15, overlapping = "mean")
crs(mos15) <- crs(crs1)
writeRaster(mos15, "G:\\CDI2017\\Mosaics\\H_BINS\\Shen_HBIN_4_mosaic.tif")

# HBiN 5
#if (interactive())     ras5<-choose.files(filters = exts)
mos16 <- mosaicList(ras16, overlapping = "mean")
crs(mos16) <- crs(crs1)
writeRaster(mos16, "G:\\CDI2017\\Mosaics\\H_BINS\\Shen_HBIN_5_mosaic.tif")

# Hcount 0
#if (interactive())     ras5<-choose.files(filters = exts)
mos17 <- mosaicList(ras17, overlapping = "mean")
crs(mos17) <- crs(crs1)
writeRaster(mos17, "G:\\CDI2017\\Mosaics\\H_COUNT\\Shen_HCOUNT_0_mosaic.tif")

# Hcount 1
#if (interactive())     ras5<-choose.files(filters = exts)
mos18 <- mosaicList(ras18, overlapping = "mean")
crs(mos18) <- crs(crs1)
writeRaster(mos18, "G:\\CDI2017\\Mosaics\\H_COUNT\\Shen_HCOUNT_1_mosaic.tif")

# Hcount 2
#if (interactive())     ras5<-choose.files(filters = exts)
mos19 <- mosaicList(ras19, overlapping = "mean")
crs(mos19) <- crs(crs1)
writeRaster(mos19, "G:\\CDI2017\\Mosaics\\H_COUNT\\Shen_HCOUNT_2_mosaic.tif")

# Hcount 3
#if (interactive())     ras5<-choose.files(filters = exts)
mos20 <- mosaicList(ras20, overlapping = "mean")
crs(mos20) <- crs(crs1)
writeRaster(mos20, "G:\\CDI2017\\Mosaics\\H_COUNT\\Shen_HCOUNT_3_mosaic.tif")

# Hcount 4
#if (interactive())     ras5<-choose.files(filters = exts)
mos21 <- mosaicList(ras21, overlapping = "mean")
crs(mos21) <- crs(crs1)
writeRaster(mos21, "G:\\CDI2017\\Mosaics\\H_COUNT\\Shen_HCOUNT_4_mosaic.tif")

# Hcount 5
#if (interactive())     ras5<-choose.files(filters = exts)
mos22 <- mosaicList(ras22, overlapping = "mean")
crs(mos22) <- crs(crs1)
writeRaster(mos22, "G:\\CDI2017\\Mosaics\\H_COUNT\\Shen_HCOUNT_5_mosaic.tif")

if (interactive())     ras23a<-choose.files(filters = exts)
if (interactive())     ras23b<-choose.files(filters = exts)
if (interactive())     ras23c<-choose.files(filters = exts)
if (interactive())     ras23d<-choose.files(filters = exts)
if (interactive())     ras23e<-choose.files(filters = exts)
if (interactive())     ras23f<-choose.files(filters = exts)
if (interactive())     ras23g<-choose.files(filters = exts)
if (interactive())     ras23h<-choose.files(filters = exts)
if (interactive())     ras23i<-choose.files(filters = exts)
if (interactive())     ras23j<-choose.files(filters = exts)
if (interactive())     ras23k<-choose.files(filters = exts)

ras24 <- list()
mos24 <- list()
nc <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 98)

for (m in 1:length(ras23a)) {

ras24[[m]] <- c(ras23a[m], ras23b[m], ras23c[m], ras23d[m], ras23e[m],
ras23f[m], ras23g[m], ras23h[m], ras23i[m], ras23j[m], ras23k[m])

mos24[[m]] <- mosaicList(ras24[[m]], overlapping = "mean")
crs(mos24[[m]]) <- crs(crs1)
writeRaster(mos24[[m]], paste("G:\\CDI2017\\Mosaics\\H_CENT\\Shen_HCENT_p", nc[m],"_mosaic.tif", sep = ""))

}

if (interactive())     ras25a<-choose.files(filters = exts)
if (interactive())     ras25b<-choose.files(filters = exts)
if (interactive())     ras25c<-choose.files(filters = exts)
if (interactive())     ras25d<-choose.files(filters = exts)
if (interactive())     ras25e<-choose.files(filters = exts)
if (interactive())     ras25f<-choose.files(filters = exts)
if (interactive())     ras25g<-choose.files(filters = exts)
if (interactive())     ras25h<-choose.files(filters = exts)
if (interactive())     ras25i<-choose.files(filters = exts)
if (interactive())     ras25j<-choose.files(filters = exts)
if (interactive())     ras25k<-choose.files(filters = exts)

ras26 <- list()
mos26 <- list()
nc <- c("avg", "kur", "max", "min", "qav", "ske", "std", "vdr98", "vdr100")

for (m in 1:length(ras25a)) {

ras26[[m]] <- c(ras25a[m], ras25b[m], ras25c[m], ras25d[m], ras25e[m],
ras25f[m], ras25g[m], ras25h[m], ras25i[m], ras25j[m], ras25k[m])

mos26[[m]] <- mosaicList(ras26[[m]], overlapping = "mean")
crs(mos26[[m]]) <- crs(crs1)
writeRaster(mos26[[m]], paste("G:\\CDI2017\\Mosaics\\STATS\\Shen_STATS_", nc[m],"_mosaic.tif", sep = ""))

}

library(foreign)
library(stringr)

if (interactive())     tabdbf<-choose.files(filters = exts)

for (i in 1:length(tabdbf)){

tab1 <- read.dbf(tabdbf[i])

path1 <- str_split(tabdbf[i], "\\\\")
l1 <- length(path1[[1]])
nm1 <- path1[[1]][l1]
nm2 <- str_split(nm1, "[.]")[[1]]

nm3 <- paste(path1[[1]][1], "\\",path1[[1]][2], "\\","Tables_csv\\Veg_Metrics", "\\",nm2[1], ".csv", sep = "")

write.csv(tab1, nm3, row.names = FALSE)

}

if (interactive())     tabcsv<-choose.files(filters = exts)

tab1 <- read.csv(tabcsv[1])
tab1 <- tab1[order(tab1$SiteID),]rob.corr

veg_lm <- list()

for (i in 1:length(tabcsv)) {
path1 <- str_split(tabcsv[i], "\\\\")
l1 <- length(path1[[1]])
nm1 <- path1[[1]][l1]

nm2 <- str_split(nm1, "_")[[1]]

nm3 <- paste(unlist(sapply(str_extract_all(nm2, '\\b[A-Z0-9]{1,}\\b'), paste, sep=""))[1],
"_",unlist(sapply(str_extract_all(nm2, '\\b[A-Z0-9]{1,}\\b'), paste, sep=""))[2], sep = "")
 
tab1 <- read.csv(tabcsv[i])
tab1 <- tab1[order(tab1$SiteID),]

names(tab1)[8] <- paste("Mean_",nm3, sep="")

veg_lm[[i]] <- tab1[,c(1, 8)]


}

veg_lidm <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "SiteID", all = TRUE),veg_lm)

if (interactive())     tabveg<-choose.files(filters = exts)

# [1] "G:\\CDI2017\\Tables_csv\\shen_plots.csv" 
# [2] "G:\\CDI2017\\Tables_csv\\shen_shrubs.csv"
# [3] "G:\\CDI2017\\Tables_csv\\shen_trees.csv" 

plots <- read.csv(tabveg[1])
shrubs <- read.csv(tabveg[2])
trees <- read.csv(tabveg[3])



