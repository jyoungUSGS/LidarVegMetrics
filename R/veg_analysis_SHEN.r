# SHEN_plots data
# squares of 24 x 24 m

setwd("G:\\cdi2017")

exts <- matrix(c("geotiff", "image", "csv tables", "all files","shapefiles","*.tif", "*.img", "*.csv", "*.*", "*.shp"), nrow = 5, ncol = 2)

if (interactive())     tabveg<-choose.files(filters = exts)

# [1] "G:\\CDI2017\\Tables_csv\\shen_plots.csv" 
# [2] "G:\\CDI2017\\Tables_csv\\shen_shrubs.csv"
# [3] "G:\\CDI2017\\Tables_csv\\shen_trees.csv" 

plots <- read.csv(tabveg[1])
shrubs <- read.csv(tabveg[2])
trees <- read.csv(tabveg[3])

trees1a <- trees[, c(1, 3, 7, 11, 12)]
shrubs1a <- shrubs[, c(1, 3, 7, 12, 15)]

names(trees1a)[5] <- "BA_m2"
names(shrubs1a)<- names(trees1a)
 
veg1 <- rbind(trees1a, shrubs1a)


colnames(trees)

# tabulate how many trees are for each site of a certain genus - need for later
tr1 <- with(trees, table(SiteID, Genus))
shr1 <- with(shrubs, table(SiteID, Genus))

tr1ts <- with(veg1, table(SiteID, Genus))

 # source("F:\\R_scripts\\split_violin_plots_for_ggplot.r")
 
# violin plots per genus of DBH and area values - 
# just to see if there is anything interesting happening

#Eliminate genus Abies because there are only 2 trees in the whole table and no measurements
# were done on them

trees1 <- trees[trees$Genus != "Abies",]

# DBH plots - only simple violin plota

for (i in seq(1, length(unique(trees1$Genus)),6)) {
windows(i)
plot(ggplot(trees1[trees1$Genus %in% levels(droplevels(trees1$Genus))[i:(i+5)],],
	aes(Genus, DBHcm, fill = Genus, colour = Genus)) +
	geom_violin(alpha=0.3, size=0.7, width=0.7, trim = TRUE, scale = "width", adjust = 0.5) + 
	geom_boxplot(width=0.1, outlier.shape = 1, outlier.colour="grey30", notch = FALSE, notchwidth = .5, alpha = 0.5, colour = "black")+
	scale_colour_manual(values=rainbow(6), name = "Sites")+
	scale_fill_manual(values=rainbow(6), name = "Sites") +
	labs(y = "DBH, cm", x = "") +
	theme(axis.text.x = element_text(size = 15, color = "grey30")) +
	theme(axis.text.y = element_text(size = 15, color = "grey30")) +
	theme(axis.title.y = element_text(size = 20)) +
	theme(legend.position="none") +
	geom_text(data=NULL, x=4, y=max(trees1[trees1$Genus %in% levels(droplevels(trees1$Genus))[i:(i+5)],]$DBHcm,na.rm=TRUE), label = "Genus DBH, cm", col = "black", size = 5.5))

}

for (i in seq(1, length(unique(trees1$Genus)), 6)) {
jpeg(paste0("G:\\CDI2017\\Imag\\DBH_genus_",i,".jpg"), width=11, height=8.5,units="in", quality = 100, res=200)
plot(ggplot(trees1[trees1$Genus %in% levels(droplevels(trees1$Genus))[i:(i+5)],],
	aes(Genus, DBHcm, fill = Genus, colour = Genus)) +
	geom_violin(alpha=0.3, size=0.7, width=0.7, trim = TRUE, scale = "width", adjust = 0.5) + 
	geom_boxplot(width=0.1, outlier.shape = 1, outlier.colour="grey30", notch = FALSE, notchwidth = .5, alpha = 0.5, colour = "black")+
	scale_colour_manual(values=rainbow(6), name = "Sites")+
	scale_fill_manual(values=rainbow(6), name = "Sites") +
	labs(y = "DBH, cm", x = "") +
	theme(axis.text.x = element_text(size = 15, color = "grey30")) +
	theme(axis.text.y = element_text(size = 15, color = "grey30")) +
	theme(axis.title.y = element_text(size = 20)) +
	theme(legend.position="none") +
	geom_text(data=NULL, x=4, y=max(trees1[trees1$Genus %in% levels(droplevels(trees1$Genus))[i:(i+5)],]$DBHcm,na.rm=TRUE), label = "Genus DBH, cm", col = "black", size = 5.5))

dev.off()
}

#the colors do not work correctly - i need to figure that out if need be

for (i in seq(1, length(unique(veg1$Genus)), 8)) {
windows(i)
#jpeg(paste0("G:\\CDI2017\\Imag\\DBH_genus_",i,".jpg"), width=11, height=8.5,units="in", quality = 100, res=200)
plot(ggplot(veg1[veg1$Genus %in% levels(droplevels(veg1$Genus))[i:(i+7)],],
	aes(Genus, DBHcm, fill = Strata, colour = Strata)) +
	geom_split_violin(alpha=0.3, size=0.7, width=0.7, trim = TRUE, scale = "width", adjust = 0.5) + 
	geom_boxplot(width=0.1, outlier.shape = 1, outlier.colour="grey30", notch = FALSE, notchwidth = .5, alpha = 0.5, colour = "black")+
	scale_shape_manual(values=c(17, 19))+
	scale_colour_manual(values=rainbow(16), name = "Sites")+
	scale_fill_manual(values=rainbow(16), name = "Sites") +
	labs(y = "DBH, cm", x = "") +
	theme(axis.text.x = element_text(size = 15, color = "grey30")) +
	theme(axis.text.y = element_text(size = 15, color = "grey30")) +
	theme(axis.title.y = element_text(size = 20)) +
	theme(legend.position="none") +
	geom_text(data=NULL, x=4, y=max(veg1[veg1$Genus %in% levels(droplevels(veg1$Genus))[i:(i+5)],]$DBHcm,na.rm=TRUE), label = "Genus DBH, cm", col = "black", size = 5.5))

#dev.off()
}

# area plots

for (i in seq(1, length(unique(trees1$Genus)),6)) {
windows(i)
plot(ggplot(trees1[trees1$Genus %in% levels(droplevels(trees1$Genus))[i:(i+5)],],
	aes(Genus, TreeBA_m2, fill = Genus, colour = Genus)) +
	geom_violin(alpha=0.3, size=0.7, width=0.7, trim = TRUE, scale = "width", adjust = 0.5) + 
	geom_boxplot(width=0.1, outlier.shape = 1, outlier.colour="grey30", notch = FALSE, notchwidth = .5, alpha = 0.5, colour = "black")+
	scale_colour_manual(values=rainbow(6), name = "Sites")+
	scale_fill_manual(values=rainbow(6), name = "Sites") +
	labs(y = "Basal Area, sq. m", x = "") +
	theme(axis.text.x = element_text(size = 15, color = "grey30")) +
	theme(axis.text.y = element_text(size = 15, color = "grey30")) +
	theme(axis.title.y = element_text(size = 20)) +
	theme(legend.position="none") +
	geom_text(data=NULL, x=4, y=max(trees1[trees1$Genus %in% levels(droplevels(trees1$Genus))[i:(i+5)],]$TreeBA_m2,na.rm=TRUE), label = "Genus Basal Area, sq. m", col = "black", size = 5.5))
}

for (i in seq(1, length(unique(trees1$Genus)), 6)) {
jpeg(paste0("G:\\CDI2017\\Imag\\Basal_area_genus_",i,".jpg"), width=11, height=8.5,units="in", quality = 100, res=200)
plot(ggplot(trees1[trees1$Genus %in% levels(droplevels(trees1$Genus))[i:(i+5)],],
	aes(Genus, TreeBA_m2, fill = Genus, colour = Genus)) +
	geom_violin(alpha=0.3, size=0.7, width=0.7, trim = TRUE, scale = "width", adjust = 0.5) + 
	geom_boxplot(width=0.1, outlier.shape = 1, outlier.colour="grey30", notch = FALSE, notchwidth = .5, alpha = 0.5, colour = "black")+
	scale_colour_manual(values=rainbow(6), name = "Sites")+
	scale_fill_manual(values=rainbow(6), name = "Sites") +
	labs(y = "Basal Area, sq. m", x = "") +
	theme(axis.text.x = element_text(size = 15, color = "grey30")) +
	theme(axis.text.y = element_text(size = 15, color = "grey30")) +
	theme(axis.title.y = element_text(size = 20)) +
	theme(legend.position="none") +
	geom_text(data=NULL, x=4, y=max(trees1[trees1$Genus %in% levels(droplevels(trees1$Genus))[i:(i+5)],]$TreeBA_m2,na.rm=TRUE), label = "Genus Basal Area, sq. m", col = "black", size = 5.5))
	
	dev.off()
}

#tr1 <- with(trees, table(SiteID, Genus))

#only trees
tr1 <- data.frame(SiteID = rownames(tr1), data.frame(unclass(with(trees, table(SiteID, Genus)))))
veg <- data.frame(unclass(with(trees, table(SiteID, Genus))))

#trees and shrubs
tr1ts <- data.frame(SiteID = rownames(tr1ts), data.frame(unclass(with(veg1, table(SiteID, Genus)))))
vegts <- data.frame(unclass(with(veg1, table(SiteID, Genus))))

# only trees
plots1 <- plots[,-9]

plots1 <- plots1[sort(plots1$SiteID),]
tr1 <- tr1[sort(tr1$SiteID),]

tr.center <- apply(tr1[,-1],2,function(x){(x-mean(x))/sd(x)})   ### standardizing

spc.pres<-apply(tr1[,-1]>0,2,sum)

spc.pres1 <- data.frame(Genus = names(spc.pres), No_plots = as.numeric(spc.pres))

col1 <- rainbow(dim(spc.pres1[1]))
colveg <- cbind(as.character(spc.pres1$Genus), col1)

windows(2)
plot(sort(spc.pres1$No_plots),pch = 16, cex = 2, col = colveg[,2][order(spc.pres1$Genus)], main="Cumulative Distribution of Species Occurrences",xlab='Cumulative Count of Species',ylab='Number of Plots') 

legend(2, 120, spc.pres$Genus, pch = 16, col = col1, bty = "n", pt.cex = 1.5, ncol = 5)

identify(seq(1,dim(spc.pres1)[1]), sort(spc.pres1$No_plots), labels=colveg[,1][order(spc.pres)])

# trees & shrubs
plots1 <- plots[,-9]

plots1 <- plots1[sort(plots1$SiteID),]
tr1ts <- tr1ts[sort(tr1ts$SiteID),]

mn <- unique(names(tr1ts[-1]))
mn <- c("SiteID", mn[order(mn)])
tr1ts <- tr1ts[,mn ] 


trts.center <- apply(tr1ts[,-1],2,function(x){(x-mean(x))/sd(x)})   ### standardizing

spcts.pres<-apply(tr1ts[,-1]>0,2,sum)

spcts.pres1 <- data.frame(Genus = names(spcts.pres), No_plots = as.numeric(spcts.pres))

col1.ts <- rainbow(dim(spcts.pres1)[1])
colveg.ts <- cbind(as.character(spcts.pres1$Genus), col1.ts)

#windows(2)
plot(sort(spcts.pres1$No_plots),pch = 16, cex = 2, col = colveg.ts[,2][order(spcts.pres1$Genus)], main="Cumulative Distribution of Species Occurrences",xlab='Cumulative Count of Species',ylab='Number of Plots') 

legend(2, 130, spcts.pres1$Genus, pch = 16, col = col1.ts, bty = "n", pt.cex = 1.5, ncol = 8)

identify(seq(1,dim(spcts.pres1)[1]), sort(spcts.pres1$No_plots), labels=colveg.ts[,1][order(spcts.pres)])


# What is the mean cover of each species when it occurs (not averaging zeros for plots 
# where it is absent)?

cover <- apply(veg, 1, function(x){(x*100)/sum(x)})   ###to transform in percent cover …. 

tmp<-apply(cover,1,sum)
spc.mean<-tmp/spc.pres  #### to get the average cover for each species
plot(sort(spc.mean),pch = 16, cex = 2, col = colveg[,2][order(spc.mean)],main="Cumulative Species Abundance Distribution",xlab="Cumulative Number of Species", ylab="Mean Abundance")

legend(2, 40, names(spc.pres), pch = 16, col = col1, bty = "n", pt.cex = 1.5, ncol = 3)

identify(seq(1,dim(spc.pres1)[1]), sort(spc.mean), labels=colveg[,1][order(spc.mean)])

# trees & shrubs
cover.ts <- apply(vegts, 1, function(x){(x*100)/sum(x)})   ###to transform in percent cover …. 

mn1 <- row.names(cover.ts)
cover.ts <- cover.ts[order(mn1), ]

tmp<-apply(cover.ts,1,sum)
spcts.mean<-tmp/spcts.pres  #### to get the average cover for each species

plot(sort(spcts.mean),pch = 16, cex = 2, col = colveg.ts[,2][order(spcts.mean)],
main="Cumulative Species Abundance Distribution",xlab="Cumulative Number of Species", 
ylab="Mean Abundance")

legend(2, 25, names(spcts.pres), pch = 16, col = col1.ts, bty = "n", pt.cex = 1.5, ncol = 8)

identify(seq(1,dim(spcts.pres1)[1]), sort(spcts.mean), labels=colveg.ts[,1][order(spcts.mean)])


# Is the mean abundance of species correlated with the number of plots they occur in?


source("F:\\R_scripts\\corcoplot_fct.R")

la <-layout(matrix(c(2,1), 2, 1,byrow = TRUE),c(3,1), c(1,3))
layout.show(la)
par(mar=(c(5,5,5,4)))

cor_1 <- cor.plot(spc.pres,spc.mean, quan = 0.5, alpha = 0.025, 
xlab = "Species occurrences", ylab = "Species mean abundance", 
xlim1=TRUE, ylim1=TRUE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3)
points(spc.pres,spc.mean, pch = 16, cex=2, col = col1)
identify(spc.pres,spc.mean, labels=colveg[,1])

par(mar=(c(0,0,0,0)))
barplot(spc.pres, col = "white", border = NA, names.arg = "", axes = F)
legend(2, 90, names(spc.pres), pch = 16, col = col1, bty = "n", pt.cex = 1.5, ncol = 6)


# Arguably, species which are widespread or abundant (if not both), give an area's 
# vegetation much of its character, while those species which are locally distributed 
# may highlight sites of specific interest and add "interesting" detail to the study 
# of vegetation. Salix, Viburnum and Abies have the lowest abundance and spread.

# trees & shrubs

la <-layout(matrix(c(2,1), 2, 1,byrow = TRUE),c(3,1), c(1,3))
layout.show(la)
par(mar=(c(5,5,5,4)))

cor_1ts <- cor.plot(spcts.pres,spcts.mean, quan = 0.5, alpha = 0.025, 
xlab = "Species occurrences", ylab = "Species mean abundance", 
xlim1=TRUE, ylim1=TRUE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3)
points(spcts.pres,spcts.mean, pch = 16, cex=2, col = col1.ts)
identify(spcts.pres,spcts.mean, labels=colveg.ts[,1])

par(mar=(c(0,0,0,0)))
barplot(spcts.pres, col = "white", border = NA, names.arg = "", axes = F)
legend(2, 100, names(spcts.pres), pch = 16, col = col1, bty = "n", pt.cex = 1.5, ncol = 8)

# Is the total abundance of vegetation correlated with the number of species in a plot?

plt.pres<-apply(veg>0,1,sum)  ### to calculate the number of species in each plot.
percent <- apply(veg, 1, function(x){(x*100)/sum(x)})     ###to transform in percent cover …. 
plt <-apply(veg,1,sum) 
plt.sum <- (plt*100)/max(plt)   #### to calculate the total cover on each plot

#and then plot as:
plot(sort(plt.sum), ylab = "Total percent cover on each plot")

#Finally, to answer our question on the relation between total cover and number of species/plot, 
plot(plt.pres,plt.sum, xlab = "Number of species per plot", ylab = "Total percent cover per plot")

#cor(plt.pres,plt.sum)

abline(h=80, col = "green", lty=2)
abline(h=50, col = "green", lty=2)
abline(h=90, col = "orange", lty=2)
abline(h=30, col = "orange", lty=2)
text(c(13.5, 13.5, 13.5, 13.5), c(31, 51, 81, 91), c("30% cover per plot", "50% cover per plot",
"80% cover per plot", "90% cover per plot"), col = c("orange", "green", "green", "orange"))

identify(plt.pres,plt.sum, names(plt.sum))
x2 <- cbind(plt.pres,plt.sum)

# Apparently, there is very little relationship between the number of species per plot and the 
# total cover. Surprisingly one of the plots (2L117-1) with the highest number of species per plot
# (14) has a percent cover of less than 40% (39.40%). One of the plots with a small number 
# of species (only 4) has the highest % cover of 100%(1L123-1). Only 3 plots have total percent cover 
# greater than 80%, (1L123-1, 3L463-1, and 2L119-1) with 4,6 and 11 species per plot respectively.
# the lowest % cover is 12.40% (2L511-1) with only 6 tree species.

#trees & shrubs

pltts.pres<-apply(vegts>0,1,sum)  ### to calculate the number of species in each plot.
percent.ts <- apply(vegts, 1, function(x){(x*100)/sum(x)})     ###to transform in percent cover …. 
plt.ts <-apply(vegts,1,sum) 
pltts.sum <- (plt.ts*100)/max(plt.ts)   #### to calculate the total cover on each plot

#and then plot as:
plot(sort(pltts.sum), ylab = "Total percent cover on each plot")

#Finally, to answer our question on the relation between total cover and number of species/plot, 
plot(pltts.pres,pltts.sum, xlab = "Number of species per plot", ylab = "Total percent cover per plot")

#cor(plt.pres,plt.sum)

abline(h=80, col = "green", lty=2)
abline(h=50, col = "green", lty=2)
abline(h=90, col = "orange", lty=2)
abline(h=30, col = "orange", lty=2)
text(c(20, 20, 20, 20), c(31, 51, 81, 91), c("30% cover per plot", "50% cover per plot",
"80% cover per plot", "90% cover per plot"), col = c("orange", "green", "green", "orange"))

identify(pltts.pres,pltts.sum, names(pltts.sum))

x2ts <- cbind(pltts.pres,pltts.sum)
x2tsa <- data.frame(SiteID= names(pltts.pres), x2ts)
# range(pltts.pres)


# Apparently, there is very little relationship between the number of species per plot and the 
# total cover. Surprisingly one of the plots (2L511-1) with the highest number of species per plot
# (21) has a percent cover of less than 50% (49.98%). One of the plots with the highest % cover 
# of 100%(1L123-1) has a smaller number of species (only 11)
#  Only 3 plots have total percent cover 
# greater than 80%, (1L123-1, 3L463-1, and 2L119-1) with 11,9 and 15 species per plot respectively.
# The lowest % cover is 23.45% (1L498-1) with only 6 tree species.

# Joint Distributions of Lidar data

lidar_cor <- cor(veg_lidm[,-1], use = "complete.obs")


library(reshape2)
library(ggplot2)

lid1 <- lidar_cor
mn <- row.names(lidar_cor)
 mn <- mn[order(mn)]
lid1 <- lid1[mn, ] 
lid1 <- lid1[, mn]

cor_heatmap <- melt(lid1, id.vars = row.names(lidar_cor))

names(cor_heatmap)[3] <- c("Class.cor")

ggplot(cor_heatmap, aes(Var2, Var1 )) +
  geom_tile(aes(fill = Class.cor)) +
  scale_fill_gradient2(low = "red3", high = "steelblue", limits=c(-1, 1)) +
  ylab("List of vegetation lidar metrics") +
  xlab("List of vegetation lidar metrics") +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Pearson \nCorrelation", vjust = 10) 

#save the jpg  
jpeg(paste0("G:\\CDI2017\\Imag\\Corr_heatmap_mean_veg_metrics.jpg"), width=11, height=8.5,units="in", quality = 100, res=200)
plot(ggplot(cor_heatmap, aes(Var2, Var1 )) +
  geom_tile(aes(fill = Class.cor)) +
  scale_fill_gradient2(low = "red3", high = "steelblue", limits=c(-1, 1)) +
  ylab("List of vegetation lidar metrics") +
  xlab("List of vegetation lidar metrics") +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Pearson \nCorrelation", vjust = 10))
  dev.off()
  
 # Robust Correlation
 
 library (library(gtools))
 library(MASS)
 
 x <- veg_lidm[,-1]
 aa1 <- combinations(40, 2, colnames(x))
 
 n1 <- dim(aa1)[1]
 covr <- list()
 
 for (i in 1:n1){
 
 ind1 <- which(colnames(x)==aa1[i,1])
 ind2 <- which(colnames(x)==aa1[i,2])
 
 x1 <- as.matrix(cbind(x[,ind1], x[,ind2]))
  x1 <- na.omit(x1)
  
  covr[[i]] <- cov.mcd(x1, cor=TRUE)$cor[1,2]
 }
 
aa2 <- data.frame(aa1, Rob.Cor = unlist(covr))

aa2 <- aa2[order(aa2[,1]),]

library(plyr)
metricsOrd <- unique(aa2[,1])

aa2[,1] <- factor(aa2[,1],levels=metricsOrd)

 m1 <- matrix(, 40, 40)
 m1[lower.tri(m1)] <- aa2[,3]
 m1 <- t(m1)
 m1[lower.tri(m1)] <- aa2[,3]
 diag(m1) <- 1
 
 mn <- row.names(lidar_cor)
 mn <- mn[order(mn)]
 dimnames(m1) <- list(mn, mn)
 
 rcor_heatmap <- melt(m1, id.vars = row.names(m1))


names(rcor_heatmap)[3] <- c("Rob.cor")

ggplot(rcor_heatmap, aes(Var2, Var1 )) +
  geom_tile(aes(fill = Rob.cor)) +
  scale_fill_gradient2(low = "red3", high = "steelblue", limits=c(-1, 1)) +
  ylab("List of vegetation lidar metrics") +
  xlab("List of vegetation lidar metrics") +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Robust \nCorrelation", vjust = 10) 

 
jpeg(paste0("G:\\CDI2017\\Imag\\RobCorr_heatmap_mean_veg_metrics.jpg"), width=11, height=8.5,units="in", quality = 100, res=200)
plot(ggplot(rcor_heatmap, aes(Var2, Var1 )) +
  geom_tile(aes(fill = Rob.cor)) +
  scale_fill_gradient2(low = "red3", high = "steelblue", limits=c(-1, 1)) +
  ylab("List of vegetation lidar metrics") +
  xlab("List of vegetation lidar metrics") +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Robust \nCorrelation", vjust = 10))
  dev.off()
 

# starting to look at the lidar metrics
 
  source("F:\\R_scripts\\corcoplot_fct.R")

 library(labdsv)
 
lidar <- veg_lidm

attach(lidar) 

n <- length(names(lidar))


for (i in seq(2, n, 8)){ 
windows(20,10)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))
#par(mar=c(5,6,5,3))
for (j in i:(i+7)){

cor_1 <- cor.plot(lidar[,j], pltts.sum, quan = 0.5, alpha = 0.025, 
xlab=paste(names(lidar)[j], ", m", sep = ""), 
ylab =" Total percent cover per plot", tcol1="blue", tcol2="red", ecol1="blue", 
ecol2="red", elty1=2, elty2=1, elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3)

}

}

# saving jpg Trees + Shrubs

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Lidar_metrics_corr_percent_cov_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){

cor_1 <- cor.plot(lidar[,j], pltts.sum, quan = 0.5, alpha = 0.025, 
xlab=paste(names(lidar)[j], ", m", sep = ""), 
ylab =" Total percent cover per plot", tcol1="blue", tcol2="red", ecol1="blue", 
ecol2="red", elty1=2, elty2=1, elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3)

}
dev.off()
}

# do the same thing but only with trees

for (i in seq(2, n, 8)){ 
windows(20,10)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))
#par(mar=c(5,6,5,3))
for (j in i:(i+7)){

cor_1 <- cor.plot(lidar[,j], plt.sum, quan = 0.5, alpha = 0.025, 
xlab=paste(names(lidar)[j], ", m", sep = ""), 
ylab =" Total percent cover per plot", tcol1="blue", tcol2="red", ecol1="blue", 
ecol2="red", elty1=2, elty2=1, elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3)

}

}

# saving jpg

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Lidar_metrics_corr_trees_percent_cov_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){

cor_1 <- cor.plot(lidar[,j], plt.sum, quan = 0.5, alpha = 0.025, 
xlab=paste(names(lidar)[j], ", m", sep = ""), 
ylab =" Total percent cover per plot, trees", tcol1="blue", tcol2="red", ecol1="blue", 
ecol2="red", elty1=2, elty2=1, elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3)

}
dev.off()
}

# lets look at PCA of enviro variables with trees

# since i have NA vals in my data let's replace them with 0

lidar.1 <- t(lidar[,-1])
colnames(lidar.1) <- lidar[,1]

lidar.2 <- apply(lidar.1, c(1,2), function(x) if(is.na(x)) x=0 else x=x)

pca.lid<-pca(lidar.2, cor=TRUE)

pca.lidar <- pca.lid$loadings


for (i in seq(1, dim(pca.lidar)[2], 8)){ 
windows(20,10)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))
#par(mar=c(5,6,5,3))
for (j in i:(i+7)){

cor_1 <- cor.plot(pca.lidar[,j], plt.sum, quan = 0.5, alpha = 0.025, 
xlab=paste("Lidar metrics PCA,",colnames(pca.lidar)[j]), 
ylab =" Total percent cover per plot, trees", tcol1="blue", tcol2="red", ecol1="blue", 
ecol2="red", elty1=2, elty2=1, elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3)

}

}

# saving jpg

for (i in seq(1, dim(pca.lidar)[2], 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Lidar_metrics_corr_PCAtrees_percent_cov_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))
#par(mar=c(5,6,5,3))
for (j in i:(i+7)){

cor_1 <- cor.plot(pca.lidar[,j], plt.sum, quan = 0.5, alpha = 0.025, 
xlab=paste("Lidar metrics PCA,",colnames(pca.lidar)[j]), 
ylab =" Total percent cover per plot, trees", tcol1="blue", tcol2="red", ecol1="blue", 
ecol2="red", elty1=2, elty2=1, elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3)

}
dev.off()
}

# PCA and eigenvalues for vegetation

# only trees
pca.1<-pca(veg,cor=TRUE)

# Variance explained by eigenvectors

summary(pca.1)

pcanames <- paste("P", seq(1, dim(pca.1$loadings)[2]), sep="")

prop.var <- (100*pca.1$sdev^2)/(sum(pca.1$sdev^2))

barplot(prop.var, names.arg=pcanames, ylim = c(0, 10),ylab = "Proportion of Variance", 
main = "Principal Component Analysis for Shenandoah trees vegetation", col = rainbow(length(pcanames)))

# the first 19 PCA var sums barely above 75% at 76.58511%

jpeg(paste0("G:\\CDI2017\\Imag\\PCA_vegetation_only_trees.jpg"), width=20, 
height=10,units="in", quality = 100, res=200)
barplot(prop.var, names.arg=pcanames, ylim = c(0, 10),ylab = "Proportion of Variance", 
main = "Principal Component Analysis for Shenandoah trees vegetation", cex.main=2, col = rainbow(length(pcanames)))
dev.off()

#Species Loadings

loadings.pca(pca.1,dim=19)

pca.1load <- pca.1$loadings

# PCA1
# Negative correlation
 pca.1load[,1][pca.1load[,1] <= -0.3]
 
#  Fraxinus     Prunus    Robinia 
#  -0.3400659  -0.3673755 -0.3685252 

# Positive correlation
pca.1load[,1][pca.1load[,1] >= 0.2]

  # Quercus 
#   0.2936071 

# PCA2
# Negative correlation
 pca.1load[,2][pca.1load[,2] <= -0.2]
 
# Crataegus   Fraxinus     Prunus    Robinia 
# -0.2650185 -0.2050076 -0.2853723 -0.2245743 

# Positive correlation
pca.1load[,2][pca.1load[,2] >= 0.3]

# Ailanthus    Cercis    Cornus Juniperus 
# 0.3282103 0.3921674 0.3211156 0.3877142

# PCA3
# Negative correlation
 pca.1load[,3][pca.1load[,3] <= -0.2]
 
# Amelanchier      Cercis   Crataegus   Juniperus       Nyssa     Quercus     Robinia 
#  -0.2989691  -0.2723463  -0.2625438  -0.2869943  -0.2784011  -0.2009628  -0.2029257

# Positive correlation
pca.1load[,3][pca.1load[,3] >= 0.2]

# Liriodendron     Platanus        Salix        Tilia 
#  0.2038647       0.2873757       0.2749604    0.2051312

# Plot Scores

scores.pca(pca.1,dim=19)  ### to see only the first 19 since the first 19 PCS make a little over 75%

# These scores are what are typically plotted in a PCA "ordination." 

scores <- pca.1$scores
### scale scores between -1 and 1 so I can plot together with loadings …..
nrows <- nrow(scores)
ncols <- ncol(scores)

for (i in 1:nrows) {
	for (j in 1:ncols){
if (pca.1$scores[i,j] < 0) scores[i,j] <- (-1)*(pca.1$scores[i,j])/min(pca.1$scores) else scores[i,j] <- pca.1$scores[i,j]/max(pca.1$scores)
}}

pc1 <- scores[,1]
pc2 <- scores[,2]
plot(pc1, pc2, pch=16, cex=2, col = "steelblue", xlim = c(-1, 1), 
main = "Principal Component Analysis, Shenandoah Trees")
abline(v=0, lty=2, col="green")
abline(h=0, lty=2, col="green")
a0 <- rep(0, length(pc1))
b0 <- rep(0, length(pc1))
a1 <- pc1
b1 <- pc2
#segments(a0,b0,a1,b1, col = "dark grey")
#text((a1+0.05), (b1), dimnames(scores)[[1]], cex = 0.7)
identify(pc1, pc2, dimnames(scores)[[1]], col = "grey30", cex=0.7)
loadings1 <- pca.1$loadings
load1 <- loadings1[,1]
load2 <- loadings1[,2]
points(load1, load2, pch=20, cex = 2, col = "red")
c0 <- rep(0,length(load1))
d0 <- rep(0,length(load1))
c1 <- load1
d1 <- load2
segments(c0,d0,c1,d1, col = "red")
identify(load1, load2, dimnames(loadings1)[[1]], col = "red") 

# do the same for pc15 and 16

pc15 <- scores[,15]
pc16 <- scores[,16]
plot(pc15, pc16, pch=16, cex=2, col = "steelblue", xlim = c(-1, 1), 
main = "Principal Component Analysis, Shenandoah Trees")
abline(v=0, lty=2, col="green")
abline(h=0, lty=2, col="green")
a0 <- rep(0, length(pc15))
b0 <- rep(0, length(pc15))
a1 <- pc15
b1 <- pc16
#segments(a0,b0,a1,b1, col = "dark grey")
#text((a1+0.05), (b1), dimnames(scores)[[1]], cex = 0.7)
identify(pc15, pc16, dimnames(scores)[[1]], col = "grey30", cex=0.7)
loadings1 <- pca.1$loadings
load15 <- loadings1[,15]
load16 <- loadings1[,16]
points(load15, load16, pch=20, cex = 2, col = "red")
c0 <- rep(0,length(load15))
d0 <- rep(0,length(load15))
c1 <- load15
d1 <- load16
segments(c0,d0,c1,d1, col = "red")
identify(load15, load16, dimnames(loadings1)[[1]], col = "red") 

#PCA15
# Negative correlation
 pca.1load[,15][pca.1load[,15] <= -0.2]
 
# Diospyros       Rhus 
# -0.2605494 -0.3332604 

# Positive correlation
pca.1load[,15][pca.1load[,15] >= 0.2]

# Castanea    Kalmia  Viburnum 
# 0.3420385 0.6080844 0.2971671

#PCA16
# Negative correlation
 pca.1load[,16][pca.1load[,16] <= -0.2]
 
 #    Abies  Crataegus      Tsuga 
#-0.4433815 -0.2298281 -0.5813041 


# Positive correlation
pca.1load[,16][pca.1load[,16] >= 0.2]

# Paulownia  Viburnum 
# 0.3508055 0.2141210

#########################################
# do the same thing for trees and shrubs
#########################################

pca.1ts<-pca(vegts,cor=TRUE)

# Variance explained by eigenvectors

summary(pca.1ts)

pcanames.ts <- paste("P", seq(1, dim(pca.1ts$loadings)[2]), sep="")

prop.varts <- (100*pca.1ts$sdev^2)/(sum(pca.1ts$sdev^2))

barplot(prop.varts, names.arg=pcanames.ts, ylim = c(0, 10), ylab = "Proportion of Variance", 
main = "Principal Component Analysis for Shenandoah trees & shrubs vegetation", col = rainbow(length(pcanames.ts)))

# the first 26 PCA (out of 64) var sums barely above 75% at 76.198052%

jpeg(paste0("G:\\CDI2017\\Imag\\PCA_vegetation_trees_shrubs.jpg"), width=20, 
height=10,units="in", quality = 100, res=200)
barplot(prop.varts, names.arg=pcanames.ts, ylim = c(0, 10),ylab = "Proportion of Variance", 
main = "Principal Component Analysis for Shenandoah trees & shrubs vegetation", cex.main=2, col = rainbow(length(pcanames)))
dev.off()

#Species Loadings

loadings.pca(pca.1,dim=26)

pca.1loadts <- pca.1ts$loadings

# PCA1
# Negative correlation
 pca.1loadts[,1][pca.1loadts[,1] <= -0.2]
 
#  Fraxinus    Robinia      Ulmus    Lindera     Smilax      Vitis 
# -0.2176236 -0.2211123 -0.2044321 -0.2287797 -0.2097555 -0.3025152 

# Positive correlation
pca.1loadts[,1][pca.1loadts[,1] >= 0.2]

#   Kalmia   Quercus 
# 0.2111449 0.2775843 

# PCA2
# Negative correlation
 pca.1loadts[,2][pca.1loadts[,2] <= -0.2]
 
# Crataegus     Prunus    Robinia   Clematis     Ptelea      Ribes      Rubud 
# -0.2755559 -0.2800743 -0.3000886 -0.3153932 -0.3153932 -0.2277593 -0.2185036

# Positive correlation
pca.1loadts[,2][pca.1loadts[,2] >= 0.2]

#   Smilax 
# 0.2259286 

# PCA3
# Negative correlation
 pca.1loadts[,3][pca.1loadts[,3] <= -0.2]
 
#    Cercis  Juniperus    Asimina 
# -0.2861515 -0.2822135 -0.2382978

# Positive correlation
pca.1loadts[,3][pca.1loadts[,3] >= 0.2]

# Liriodendron     Platanus        Salix        Tilia 
#  0.2038647       0.2873757       0.2749604    0.2051312

# Plot Scores

scores.pca(pca.1ts,dim=26)  ### to see only the first 19 since the first 19 PCS make a little over 75%

# These scores are what are typically plotted in a PCA "ordination." 

scores.ts <- pca.1ts$scores
### scale scores between -1 and 1 so I can plot together with loadings …..
nrows.ts <- nrow(scores)
ncols.ts <- ncol(scores)

for (i in 1:nrows.ts) {
	for (j in 1:ncols.ts){
if (pca.1ts$scores[i,j] < 0) scores.ts[i,j] <- (-1)*(pca.1ts$scores[i,j])/min(pca.1ts$scores) else scores.ts[i,j] <- pca.1ts$scores[i,j]/max(pca.1ts$scores)
}}

pc1 <- scores.ts[,1]
pc2 <- scores.ts[,2]
plot(pc1, pc2, pch=16, cex=2, col = "steelblue", xlim = c(-1, 1), 
main = "Principal Component Analysis, Shenandoah Trees & Shrubs")
abline(v=0, lty=2, col="green")
abline(h=0, lty=2, col="green")
a0 <- rep(0, length(pc1))
b0 <- rep(0, length(pc1))
a1 <- pc1
b1 <- pc2
#segments(a0,b0,a1,b1, col = "dark grey")
#text((a1+0.05), (b1), dimnames(scores)[[1]], cex = 0.7)
identify(pc1, pc2, dimnames(scores.ts)[[1]], col = "grey30", cex=0.7)
loadings1.ts <- pca.1ts$loadings
load1.ts <- loadings1.ts[,1]
load2.ts <- loadings1.ts[,2]
points(load1.ts, load2.ts, pch=20, cex = 2, col = "red")
c0 <- rep(0,length(load1.ts))
d0 <- rep(0,length(load1.ts))
c1 <- load1.ts
d1 <- load2.ts
segments(c0,d0,c1,d1, col = "red")
identify(load1.ts, load2.ts, dimnames(loadings1.ts)[[1]], col = "red") 

# do the same for pc18 and 19

pc18 <- scores.ts[,18]
pc19 <- scores.ts[,19]

plot(pc18, pc19, pch=16, cex=2, bg = "grey", col = "steelblue", xlim = c(-1, 1), ylim = c(-0.6, 0.6),
main = "Principal Component Analysis, Shenandoah Trees & Shrubs")
abline(v=0, lty=2, col="green")
abline(h=0, lty=2, col="green")
a0 <- rep(0, length(pc18))
b0 <- rep(0, length(pc19))
a1 <- pc18
b1 <- pc19
#segments(a0,b0,a1,b1, col = "dark grey")
#text((a1+0.05), (b1), dimnames(scores)[[1]], cex = 0.7)
identify(pc18, pc19, dimnames(scores.ts)[[1]], col = "grey30", cex=0.7)
loadings1.ts <- pca.1$loadings
load18 <- loadings1.ts[,18]
load19 <- loadings1.ts[,19]
points(load18, load19, pch=20, cex = 2, col = "red")
c0 <- rep(0,length(load18))
d0 <- rep(0,length(load19))
c1 <- load18
d1 <- load19
segments(c0,d0,c1,d1, col = "red")
identify(load18, load19, dimnames(loadings1.ts)[[1]], col = "red") 

#PCA18
# Negative correlation
 pca.1loadts[,18][pca.1loadts[,18] <= -0.2]
 
#        Fagus          Rhus       Unknown   Menispermum Toxicodendron 
#   -0.2077161    -0.2133246    -0.2729017    -0.4831455    -0.2116750 

# Positive correlation
pca.1loadts[,18][pca.1loadts[,18] >= 0.2]

# Platanus     Salix 
# 0.2138880 0.2437382

#PCA19
# Negative correlation
 pca.1loadts[,19][pca.1loadts[,19] <= -0.2]
 
#    Betula   Fraxinus      Tilia       Ilex  Menziesia 
# -0.2424551 -0.2549623 -0.3796688 -0.3006207 -0.2376769 


# Positive correlation
pca.1loadts[,19][pca.1loadts[,19] >= 0.2]

#    Abies   Quercus   Corylus 
# 0.3837444 0.2080141 0.2030402 

#################################################################################################
# Environmental Analysis

# Of course the whole point of getting the first few axes of variation is to begin an analysis of 
# the vegetation/environment relation. First, we will employ a technique called 
# "Redundancy Analysis", which is described in detail by Legendre and Legendre (1998). 
# In a nutshell, the approach is to use linear regression to replace the observed values of 
# vegetation abundance in each plot with their fitted values, regressing each plot in turn on 
# specified environmental variables. First, we want to center the species abundances on their 
# means and standardize their variance.
#####################################################################################################

###### only TREES ##########

### if tree genus appear in less than 10% of the site it is out of the list

veg.site <- apply(veg, 2, function(x) y=length(x[x!=0]))
veg1 <- veg[, which(veg.site >= dim(veg)[1]*0.1)]

veg.center <- apply(veg1,2,function(x){(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)})


# Next, we want to specify which environmental variables we want to use, and to center them as well. 
# We will use the lidar metrics dirrectly as enviro variables, but we can use the lidar PCA vals as well.

pca <- pca.lidar

env.scale <- apply(pca,2,function(x){(x-mean(x))/sd(x)})



# In this case, since the environmental data represent the independent variables in the respective 
# regression, and since they were generally measured on different scales, we want to both center and 
# scale to unit standard deviation to simplify interpretation of the regression coefficients. 
# Now, we want to regress each species (column in veg.center) against the centered and scaled 
# environmental variables.

veg.lm <- lm(veg.center~env.scale)

# Now, we create the matrix of fitted values:

veg.fit <- fitted(veg.lm)

# It's possible to view the regression coefficients of the models, but every species is a separate 
# regression, and the details are generally overwhelming. It's sometimes worth scanning, however, 
# to see if the same variables are judged significant for a large number of species. Finally, we 
# calculate the PCA on the fitted values:

rda <- pca(veg.fit,cor=TRUE)
varplot.pca(rda,dim=5)
### or equivalent and much nicer plot
rda1 <- prcomp(veg.fit, center = TRUE, scale=TRUE)
m <- summary(rda1)
m1 <- as.matrix(m$importance)

barplot(m1[2,1:16], col = rainbow(16), ylim = c(0, 0.25), 
main = "PCA of regressed fitted values, Shenandoah trees")

sum(m1[2,])
# [1] 0.99999 - the trees Genus eliminated contribute probably less than 0.000001% of variation.

####  We can view the ordination exactly as any other ordination:

plot(rda,title="Redundancy Analysis")

scores <- rda$scores
### scale scores between -1 and 1 so I can plot together with loadings …..
nrows <- nrow(scores)
ncols <- ncol(scores)
for (i in 1:nrows) {
	for (j in 1:ncols){
if (rda$scores[i,j] < 0) scores[i,j] <- (-1)*(rda$scores[i,j])/min(rda$scores) else scores[i,j] <- rda$scores[i,j]/max(rda$scores)
}}

pc1 <- scores[,1]
pc2 <- scores[,2]
par(mar(5, 5, 5, 5))
plot(pc1, pc2, pch=16, cex=2, col = "steelblue", xlim = c(-1, 1), 
main = "Redundancy Analysis, Shenandoah trees", xlab = "RDA1", ylab = "RDA2")
abline(v=0, lty=2, col="green")
abline(h=0, lty=2, col="green")
a0 <- rep(0, length(pc1))
b0 <- rep(0, length(pc1))
a1 <- pc1
b1 <- pc2
#segments(a0,b0,a1,b1, col = "dark grey")
# text((a1+0.05), (b1), dimnames(scores)[[1]], cex=0.7)
identify(pc1, pc2, dimnames(scores)[[1]], col = "grey30", cex=0.7)

loadings <- rda$loadings
load1 <- loadings[,1]
load2 <- loadings[,2]
points(load1, load2, pch=20, cex=2)
c0 <- rep(0,length(load1))
d0 <- rep(0,length(load1))
c1 <- load1
d1 <- load2
segments(c0,d0,c1,d1, col = "red")
points(load1, load2, pch=20, cex=2, col = "red")
identify(load1, load2, dimnames(loadings)[[1]], col = "red") 

#########################################################
# Redundancy analysis for trees + shrubs
#############################################################

# if tree genus appear in less than 10% of the site it is out of the list

vegts.site <- apply(vegts, 2, function(x) y=length(x[x!=0]))
vegts1 <- vegts[, which(vegts.site >= dim(veg)[1]*0.1)]

vegts.center <- apply(vegts1,2,function(x){(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)})

# Next, we want to specify which environmental variables we want to use, and to center them as well. 
# We will use the lidar metrics directly as enviro variables, but we can use the lidar PCA vals as well.

pca <- pca.lidar

env.scale <- apply(pca,2,function(x){(x-mean(x))/sd(x)})

# In this case, since the environmental data represent the independent variables in the respective 
# regression, and since they were generally measured on different scales, we want to both center and 
# scale to unit standard deviation to simplify interpretation of the regression coefficients. 
# Now, we want to regress each species (column in veg.center) against the centered and scaled 
# environmental variables.

vegts.lm <- lm(vegts.center~env.scale)

# Now, we create the matrix of fitted values:

vegts.fit <- fitted(vegts.lm)

# It's possible to view the regression coefficients of the models, but every species is a separate 
# regression, and the details are generally overwhelming. It's sometimes worth scanning, however, 
# to see if the same variables are judged significant for a large number of species. Finally, we 
# calculate the PCA on the fitted values:

rdats <- pca(vegts.fit,cor=TRUE)
varplot.pca(rdats,dim=5)
### or equivalent and much nicer plot
rdats1 <- prcomp(vegts.fit, center = TRUE, scale=TRUE)
mts <- summary(rdats1)
m1ts <- as.matrix(mts$importance)

barplot(m1[2,1:16], col = rainbow(16), ylim = c(0, 0.25), 
main = "PCA of regressed fitted values, Shenandoah trees & shrubs")

sum(m1ts[2,])
# [1] 0.99999 - the trees + shrub Genus eliminated contribute probably less than 0.000001% of variation.

####  We can view the ordination exactly as any other ordination:

plot(rdats,title="Redundancy Analysis \n Shenandoah trees & shrubs")

scorests <- rdats$scores
### scale scores between -1 and 1 so I can plot together with loadings …..
nrowsts <- nrow(scorests)
ncolsts <- ncol(scorests)
for (i in 1:nrowsts) {
	for (j in 1:ncolsts){
if (rdats$scores[i,j] < 0) scorests[i,j] <- (-1)*(rdats$scores[i,j])/min(rdats$scores) else scorests[i,j] <- rdats$scores[i,j]/max(rdats$scores)
}}

pc1ts <- scorests[,1]
pc2ts <- scores[,2]
par(mar(5, 5, 5, 5))
plot(pc1ts, pc2ts, pch=16, cex=2, col = "steelblue", xlim = c(-1, 1), 
main = "Redundancy Analysis, Shenandoah trees & shrubs", xlab = "RDA1", ylab = "RDA2")
abline(v=0, lty=2, col="green")
abline(h=0, lty=2, col="green")
a0 <- rep(0, length(pc1ts))
b0 <- rep(0, length(pc1ts))
a1 <- pc1
b1 <- pc2
#segments(a0,b0,a1,b1, col = "dark grey")
# text((a1+0.05), (b1), dimnames(scores)[[1]], cex=0.7)
identify(pc1ts, pc2ts, dimnames(scorests)[[1]], col = "grey30", cex=0.7)

loadingsts <- rdats$loadings
load1ts <- loadingsts[,1]
load2ts <- loadingsts[,2]
points(load1ts, load2ts, pch=20, cex=2)
c0 <- rep(0,length(load1ts))
d0 <- rep(0,length(load1ts))
c1 <- load1ts
d1 <- load2ts
segments(c0,d0,c1,d1, col = "red")
points(load1ts, load2ts, pch=20, cex=2, col = "red")
identify(load1ts, load2ts, dimnames(loadingsts)[[1]], col = "red") 

# trees

# We can use Generalized Linear Models (GLM) and Generalized Additive Models (GAM) to do the same 
# analysis. Rather than analysing a species response along environmental gradients, 
# we'll be analysing the distribution of environmental variables (and later species distributions) 
# along ordination axes. Traditionally, this was done primarily by correlation or linear regression. 
cumsum(prop.var)
# In our example, the first two axes (pca.1$scores[,1] and pca.1$scores[,2]) describe only about 16% 
# of the total variance, but we'll start with a 2-dimensional approach and go from there.

elev.pca.glm<-glm(Mean_HAG_BE~pca.1$scores[,1]+pca.1$scores[,2])
summary(elev.pca.glm)

# This command is telling us that elevation is negatively related to pca.1$scores[,1] and 
# pca.1$scores[,2], and that the relation to pca.1$scores[,1] is not stat. significant while 
# pca.1$scores[,2] is statistically significant, as determined by looking at the coefficients or 
# t-values. In addition, there are several other items of interest in the summary. First, because we 
# did not specify otherwise, the family was taken to be Gaussian, meaning we expected normal errors 
# unrelated to the expected value. This is correct for a variable like elevation, which is unbounded 
# on either end(for all practical purposes), and represents a continuous value rather than a count. 
# Fitting a GLM with a Gaussian error model is equivalent to a least-squares regression. 

elev.pca.lm<-lm(Mean_HAG_BE~pca.1$scores[,1]+pca.1$scores[,2])
summary(elev.pca.lm)

# Notice that the regression coefficients, standard errors, and t-values are all the same. 
# The linear model explains the output in terms of variance (with multiple R^2 and an F test) 
# instead of deviance, but the model is identical. The R^2 reported is identical to the D^2 value 
# we would calculate from the GLM. To see the model,

require(mgcv)   # to load the gam() function
require(akima)   # to load the interp() function
plot(pca.1$scores[,1],pca.1$scores[,2], pch=20, cex=2, col = "steelblue",
main = "Shenandoah trees PCA response to elevation (mean BE)", xlab = "PCA 1", ylab = "PCA 2")
contour(interp(pca.1$scores[,1],pca.1$scores[,2],fitted(elev.pca.glm)),add=TRUE,col=2,labcex=0.8)
contour(interp(pca.1$scores[,1],pca.1$scores[,2],Mean_HAG_BE),add=T,col=3)
legend(0.5, 4.5, c("Elevation.PCA.GLM", "Elevation"), col = c("red", "green"), lty=1, bty="n")

# The problem is that the actual elevations are too variable at a small scale, and need to be 
# generalized to plot. More importantly, we need some idea of how well the ordination correlates to 
# elevation, and by fitting a formal model we get much better quantification than we get by analysis 
# of the raw data by eye. Nonetheless, the contours of the raw data give us the impression that the 
# relation of elevation to the axes is non-linear and that perhaps we should fit a better model.

elev.pca2.glm <- glm(Mean_HAG_BE~pca.1$scores[,1]+I(pca.1$scores[,1]^2)+pca.1$scores[,2]+I(pca.1$scores[,2]^2))
summary(elev.pca2.glm)

# How good the new model is?

anova(elev.pca.glm,elev.pca2.glm,test="Chi")

# The second model achieves a very modest reduction in deviance at the cost of 2 degrees of freedom 
# and this reduction is not statistically significant. Thus, the second model is not really an 
# improvement. Besides, pca.1$scores[,1] and pca.1$scores[,2] are not convincingly quadratic 
# (and they are not ….). 
anova(elev.pca2.glm,test="Chi")

plot(pca.1$scores[,1],pca.1$scores[,2], pch=20, cex=2, col = "steelblue",
main = "Shenandoah trees PCA response to elevation (mean BE)", xlab = "PCA 1", ylab = "PCA 2")
contour(interp(pca.1$scores[,1],pca.1$scores[,2],fitted(elev.pca2.glm)),add=T,col=2)
contour(interp(pca.1$scores[,1],pca.1$scores[,2],Mean_HAG_BE),add=T,col=3)
legend(0.5, 4.5, c("Elevation.PCA.GLM2", "Elevation"), col = c("red", "green"), lty=1, bty="n")

# what if i do for all variables
n <- dim(lidar)[2]

for (i in seq(2, n, 8)){ 
jpeg(paste("G:\\CDI2017\\Imag\\Shen_trees_PCA_response_to_enviro_vars_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){

if (length(which(is.na(lidar[,j]))) !=0) {
a1 <- which(is.na(lidar[,j]))
pca.lm<-lm(lidar[c(-a1),j]~pca.1$scores[c(-a1),1]+pca.1$scores[c(-a1),2])
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

plot(pca.1$scores[c(-a1),1],pca.1$scores[c(-a1),2], pch=20, cex=cex1, col = adjustcolor( "steelblue", alpha.f = 0.6),
main = paste("Shenandoah trees PCA response to", names(lidar)[j], "\n",
paste("R-sq. = ", round(summary(pca.lm)$r.squared,3), ";",sep = ""),
paste("Adj. R-sq. = ",round(summary(pca.lm)$adj.r.squared,3), sep = "")) , 
xlab = "PCA 1", ylab = "PCA 2")
contour(interp(pca.1$scores[c(-a1),1],pca.1$scores[c(-a1),2],fitted(pca.lm)),add=TRUE,col=2,labcex=0.8)
contour(interp(pca.1$scores[c(-a1),1],pca.1$scores[c(-a1),2],lidar[c(-a1),j]),add=T,col=3)

xl <- round( min(pca.1$scores[c(-a1),1])+(max(pca.1$scores[c(-a1),1])-min(pca.1$scores[c(-a1),1]))*2/3, 2)
yl <- round( min(pca.1$scores[c(-a1),1])+(max(pca.1$scores[c(-a1),1])-min(pca.1$scores[c(-a1),1]))/3, 2)

legend(xl, yl, c("PCA LM model", names(lidar)[j]), col = c("red", "green"), lty=1, bty="n")

} else {
pca.lm<-lm(lidar[,j]~pca.1$scores[,1]+pca.1$scores[,2])
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

plot(pca.1$scores[,1],pca.1$scores[,2], pch=20, cex=cex1, col = adjustcolor( "steelblue", alpha.f = 0.6),
main = paste("Shenandoah trees PCA response to", names(lidar)[j], "\n",
paste("R-sq. = ", round(summary(pca.lm)$r.squared,3), ";",sep = ""),
paste("Adj. R-sq. = ",round(summary(pca.lm)$adj.r.squared,3), sep = "")) , 
xlab = "PCA 1", ylab = "PCA 2")
contour(interp(pca.1$scores[,1],pca.1$scores[,2],fitted(pca.lm)),add=TRUE,col=2,labcex=0.8)
contour(interp(pca.1$scores[,1],pca.1$scores[,2],lidar[,j]),add=T,col=3)
xl <- round( min(pca.1$scores[,1])+(max(pca.1$scores[,1])-min(pca.1$scores[,1]))*2/3, 2)
yl <- round( min(pca.1$scores[,1])+(max(pca.1$scores[,1])-min(pca.1$scores[,1]))/3, 2)

legend(xl,yl, c("PCA LM model", names(lidar)[j]), col = c("red", "green"), lty=1, bty="n")

}
}
dev.off()
}

# Same analysis but using lidar PCA instead of lidar metrics.

n <- dim(pca.lidar)[2]

for (i in seq(1, n, 8)){ 
jpeg(paste("G:\\CDI2017\\Imag\\Shen_trees_PCA_response_to_enviro_PCA_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){

if (length(which(is.na(pca.lidar[,j]))) !=0) {
a1 <- which(is.na(pca.lidar[,j]))
pca.lm<-lm(pca.lidar[c(-a1),j]~pca.1$scores[c(-a1),1]+pca.1$scores[c(-a1),2])
cex1 =  c(1+(2*(pca.lidar[,j]-min(pca.lidar[,j], na.rm = TRUE))/(max(pca.lidar[,j], na.rm = TRUE)-min(pca.lidar[,j], na.rm = TRUE))))

plot(pca.1$scores[c(-a1),1],pca.1$scores[c(-a1),2], pch=20, cex=cex1, col = adjustcolor( "steelblue", alpha.f = 0.6),
main = paste("Shenandoah trees PCA response to lidar metrics", colnames(pca.lidar)[j],"\n",
paste("R-sq. = ", round(summary(pca.lm)$r.squared,3), ";",sep = ""),
paste("Adj. R-sq. = ",round(summary(pca.lm)$adj.r.squared,3), sep = "")) , 
xlab = "PCA 1", ylab = "PCA 2")
contour(interp(pca.1$scores[c(-a1),1],pca.1$scores[c(-a1),2],fitted(pca.lm)),add=TRUE,col=2,labcex=0.8)
contour(interp(pca.1$scores[c(-a1),1],pca.1$scores[c(-a1),2],pca.lidar[c(-a1),j]),add=T,col=3)
xl <- round( min(pca.1$scores[c(-a1),1], na.rm = TRUE)+(max(pca.1$scores[c(-a1),1], na.rm = TRUE)-min(pca.1$scores[c(-a1),1], na.rm = TRUE))*2/3, 2)
yl <- round( min(pca.1$scores[c(-a1),1], na.rm = TRUE)+(max(pca.1$scores[c(-a1),1], na.rm = TRUE)-min(pca.1$scores[c(-a1),1], na.rm = TRUE))/3, 2)

legend(xl,yl, c("PCA LM model", paste("Lidar metrics",colnames(pca.lidar)[j])), col = c("red", "green"), lty=1, bty="n")

} else {
pca.lm<-lm(pca.lidar[,j]~pca.1$scores[,1]+pca.1$scores[,2])
cex1 =  c(1+(2*(pca.lidar[,j]-min(pca.lidar[,j], na.rm = TRUE))/(max(pca.lidar[,j], na.rm = TRUE)-min(pca.lidar[,j], na.rm = TRUE))))

plot(pca.1$scores[,1],pca.1$scores[,2], pch=20, cex=cex1, col = adjustcolor( "steelblue", alpha.f = 0.6),
main = paste("Shenandoah trees PCA response to lidar metrics", colnames(pca.lidar)[j], "\n",
paste("R-sq. = ", round(summary(pca.lm)$r.squared,3), ";",sep = ""),
paste("Adj. R-sq. = ",round(summary(pca.lm)$adj.r.squared,3), sep = "")) , 
xlab = "PCA 1", ylab = "PCA 2")
contour(interp(pca.1$scores[,1],pca.1$scores[,2],fitted(pca.lm)),add=TRUE,col=2,labcex=0.8)
contour(interp(pca.1$scores[,1],pca.1$scores[,2],pca.lidar[,j]),add=T,col=3)
xl <- round( min(pca.1$scores[,1])+(max(pca.1$scores[,1])-min(pca.1$scores[,1]))*2/3, 2)
yl <- round( min(pca.1$scores[,1])+(max(pca.1$scores[,1])-min(pca.1$scores[,1]))/3, 2)

legend(xl,yl, c("PCA LM model", paste("Lidar metrics",colnames(pca.lidar)[j])), col = c("red", "green"), lty=1, bty="n")

}
}
dev.off()
}

# all the lidar metrics except BE and FRS should be bounded by 0 at the lower end. So we should fit a
# Poisson distribution instead and see if this will work slightly better
# in this case let's eliminate BE and FRS metrics - works as dismal as before ....

lidar.a <- lidar[, c(-24, -30,-37, -38)]

lidar.1a <- t(lidar.a[,-1])
colnames(lidar.1a) <- lidar.a[,1]

lidar.2a <- apply(lidar.1a, c(1,2), function(x) if(is.na(x)) x=0 else x=x)

pca.lida<-pca(lidar.2a, cor=TRUE)

pca.lidara <- pca.lida$loadings

n <- dim(lidar.a)[2]

for (i in seq(2, n, 8)){ 
jpeg(paste("G:\\CDI2017\\Imag\\Shen_trees_PCA_response_to_lidveg_vars_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){

if (length(which(is.na(lidar.a[,j]))) !=0) {
a1 <- which(is.na(lidar.a[,j]))
pca.gam<-gam(lidar.a[c(-a1),j]~pca.1$scores[c(-a1),1]+pca.1$scores[c(-a1),2], family=poisson)

plot(pca.1$scores[c(-a1),1],pca.1$scores[c(-a1),2], pch=20, cex=2.5, col = "steelblue",
main = paste("Shenandoah trees PCA response to", names(lidar.a)[j], "\n",
paste("R-sq. = ", round(summary(pca.gam)$r.sq,4), ";",sep = ""),
paste("Deviance explained = ",round(summary(pca.gam)$dev.expl,4), sep = "")) , 
xlab = "PCA 1", ylab = "PCA 2")
contour(interp(pca.1$scores[c(-a1),1],pca.1$scores[c(-a1),2],fitted(pca.gam)),add=TRUE,col=2,labcex=0.8)
contour(interp(pca.1$scores[c(-a1),1],pca.1$scores[c(-a1),2],lidar[c(-a1),j]),add=T,col=3)
legend(-2, -6, c("PCA GAM model; Family = Poisson", names(lidar)[j]), col = c("red", "green"), lty=1, bty="n")

} else {
pca.gam<-gam(lidar.a[,j]~pca.1$scores[,1]+pca.1$scores[,2], family = poisson)

plot(pca.1$scores[,1],pca.1$scores[,2], pch=20, cex=2, col = "steelblue",
main = paste("Shenandoah trees PCA response to", names(lidar.a)[j], "\n",
paste("R-sq. = ", round(summary(pca.gam)$r.sq,4), ";",sep = ""),
paste("Deviance explained = ",round(summary(pca.gam)$dev.expl,4), sep = "")) , 
xlab = "PCA 1", ylab = "PCA 2")
contour(interp(pca.1$scores[,1],pca.1$scores[,2],fitted(pca.gam)),add=TRUE,col=2,labcex=0.8)
contour(interp(pca.1$scores[,1],pca.1$scores[,2],lidar.a[,j]),add=T,col=3)
legend(-4, -6, c("PCA GAM model; Family = Poisson", names(lidar.a)[j]), col = c("red", "green"), lty=1, bty="n")

}
}
dev.off()
}

########################################
# General Surface Plotter
#############################################

# Plotting a variable as a surface on an ordination is such a common task that it is worthwhile to 
# create a general ordination surface routine. Therefore, the LabDSV library includes not only a 
# plotting routine, but a surfacing routine. It employs GAM to fit the surface (so you don't have to 
# know a priori the shape of the response, and reports back D^2 for the fit.).

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))


plot(pca.1, pch = 16, col = "steelblue")
s1 <- surf(pca.1,lidar[,2])

famname <- summary(s1)$family[[1]]

famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

title(main = paste("Shenandoah trees PCA – ", names(lidar)[j], "GAM fit"), line = 3)

title(main = paste0("Family: ",famn1, ", D^2 = ", round(summary(s1)$r.sq,4)), line = 1, cex=1)

### if tree genus appear in less than 10% of the site it is out of the list

veg.site <- apply(veg, 2, function(x) y=length(x[x!=0]))
veg1 <- veg[, which(veg.site >= dim(veg)[1]*0.1)]

pca.veg <- pca(veg1, cor = TRUE)

n <- dim(lidar)[2]
df1 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste("G:\\CDI2017\\Imag\\Shen_trees_PCA_GAM_response_to_enviro_vars_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){

if (length(which(is.na(lidar[,j]))) !=0) {
a1 <- which(is.na(lidar[,j]))
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

plot(pca.veg, pch = 16, cex=cex1, col = adjustcolor( "steelblue", alpha.f = 0.6))

pca.1a1<-pca(veg1[c(-a1),],cor=TRUE)

s1 <- surf(pca.1a1,lidar[c(-a1),j])

famname <- summary(s1)$family[[1]]

famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

title(main = paste("Shenandoah trees PCA – ", names(lidar)[j], "GAM fit"))
#title(main = paste0("Family: ",famn1, ", D^2 = ", round(summary(s1)$r.sq,4)), line = 1, cex=1)
legend(-6, -3, c(paste0("Family: ",famn1), paste0("R^2 = ", round(summary(s1)$r.sq,4)), 
 paste0("Deviance Explained: ", round(summary(s1)$dev.expl,4))), bty="n")

} else {
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

plot(pca.veg, pch = 16, cex=cex1, col = adjustcolor( "steelblue", alpha.f = 0.6))
s1 <- surf(pca.veg,lidar[,j])

famname <- summary(s1)$family[[1]]

famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

title(main = paste("Shenandoah trees PCA – ", names(lidar)[j], "GAM fit"))
#title(main = paste0("Family: ",famn1, ", D^2 = ", round(summary(s1)$r.sq,4)), line = 1, cex=1)
legend(-6, -3, c(paste0("Family: ",famn1), paste0("R^2 = ", round(summary(s1)$r.sq,4)), 
 paste0("Deviance Explained: ", round(summary(s1)$dev.expl,4))), bty="n")
}

df1[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 

}
dev.off()
}

write.csv(df1, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_PCA_GAM_response_lidar_metrics.csv", row.names = FALSE)

##########################################################
############### R square is negative sometimes!!!!!!!#######################################
#############################################################################################
#
# The adjusted r-squared for the model. Defined as the proportion of variance explained, where 
# original variance and residual variance are both estimated using unbiased estimators. 
# This quantity can be negative if your model is worse than a one parameter constant model, and can 
# be higher for the smaller of two nested models! The proportion null deviance explained is 
# probably more appropriate for non-normal errors. Note that r.sq does not include any offset in 
# the one parameter model.
#################################################################################

############### Deviance explained ################################################
########################################################################################
#
# The proportion of the null deviance explained by the model. The null deviance is computed taking 
# account of any offset, so dev.expl can be substantially lower than r.sq when an offset is present.
##########################################################################################

### Metrics for which R-sq is >0.2 in trees

df2 <- df1[df1[,3]>=0.2,]
df2 <- df2[order(df2[,3], decreasing = TRUE),]

write.csv(df2, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_PCA_GAM_lidar_metrics_02.csv", row.names = FALSE)

#      Lidar_metrics GAM_Family R_square Deviance_Expl
# 6      Mean_HBIN_5   Gaussian   0.4845        0.7764
# 24  Mean_STATS_MAX   Gaussian    0.397        0.4329
# 38    Mean_HAG_MAX   Gaussian   0.3432        0.3793
# 28 Mean_STATS_SDEV   Gaussian   0.3318        0.3653
# 16  Mean_HCENT_P98   Gaussian   0.3287        0.3642
# 15  Mean_HCENT_P90   Gaussian   0.2846         0.319
# 27  Mean_STATS_QAV   Gaussian   0.2822        0.3263
# 39    Mean_HAG_SDT   Gaussian   0.2729        0.3035
# 14  Mean_HCENT_P80   Gaussian   0.2642        0.2986
# 40    Mean_HAG_AVR   Gaussian   0.2513        0.2911
# 25 Mean_STATS_MEAN   Gaussian   0.2508        0.2885
# 13  Mean_HCENT_P70   Gaussian    0.244        0.2793
# 12  Mean_HCENT_P60   Gaussian    0.222         0.259
# 11  Mean_HCENT_P50   Gaussian   0.2196        0.2597
# 10  Mean_HCENT_P40   Gaussian   0.2157        0.2568

########################################################################################
#
#################### Do the same thing with trees & shrubs #############################
#
#########################################################################################

### if tree or shrub genus appears in less than 10% of the site it is out of the list

vegts.site <- apply(vegts, 2, function(x) y=length(x[x!=0]))
veg1ts <- vegts[, which(vegts.site >= dim(vegts)[1]*0.1)]

# from 64 dif. Genera only 29 remained

pca.vegts <- pca(veg1ts, cor = TRUE)

n <- dim(lidar)[2]
df3 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste("G:\\CDI2017\\Imag\\Shen_trees+shrubs_PCA_GAM_response_to_enviro_vars_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){

if (length(which(is.na(lidar[,j]))) !=0) {
a1 <- which(is.na(lidar[,j]))

plot(pca.vegts, pch = 16, col = "steelblue")

vegts.site1 <- apply(veg1ts[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg1ts1 <- veg1ts[c(-a1),][, which(vegts.site1 > 0)]


pca.1a1<-pca(veg1ts1,cor=TRUE)

s1 <- surf(pca.1a1,lidar[c(-a1),j])

famname <- summary(s1)$family[[1]]

famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

title(main = paste("Shenandoah trees & shrubs PCA – ", names(lidar)[j], "GAM fit"))
#title(main = paste0("Family: ",famn1, ", D^2 = ", round(summary(s1)$r.sq,4)), line = 1, cex=1)
legend(-6, -3, c(paste0("Family: ",famn1), paste0("R^2 = ", round(summary(s1)$r.sq,4)), 
 paste0("Deviance Explained: ", round(summary(s1)$dev.expl,4))), bty="n")

} else {
plot(pca.vegts, pch = 16, col = "steelblue")
s1 <- surf(pca.vegts,lidar[,j])

famname <- summary(s1)$family[[1]]

famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

title(main = paste("Shenandoah trees & shrubs PCA – ", names(lidar)[j], "GAM fit"))
#title(main = paste0("Family: ",famn1, ", D^2 = ", round(summary(s1)$r.sq,4)), line = 1, cex=1)
legend(-1, -4.5, c(paste0("Family: ",famn1), paste0("R^2 = ", round(summary(s1)$r.sq,4)), 
 paste0("Deviance Explained: ", round(summary(s1)$dev.expl,4))), bty="n")
 
}

df3[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 

}
dev.off()
}

write.csv(df3, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_PCA_GAM_response_lidar_metrics.csv", row.names = FALSE)

df4 <- df3[df3[,3]>=0.3,]
df4 <- df4[order(df4[,3], decreasing = TRUE),]

write.csv(df4, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_PCA_GAM_04_lidar_metrics.csv", row.names = FALSE)

#################################################################
############## Doing trees + shrubs + seedlings #################
#################################################################

if (interactive())     seed1<-choose.files(filters = exts) 

seedl <- read.csv(seed1)

trees1a <- trees[,c(1, 3, 7)]
shrubs1a <- shrubs[,c(1, 3, 7)]

veg2 <- rbind(trees1a, shrubs1a, seedl)
tr2ts <- with(veg2, table(SiteID, Genus))
tr2ts <- data.frame(SiteID = rownames(tr2ts), data.frame(unclass(with(veg2, table(SiteID, Genus)))))
veg2ts <- data.frame(unclass(with(veg2, table(SiteID, Genus))))

veg2ts.site <- apply(veg2ts, 2, function(x) y=length(x[x!=0]))
veg3ts <- veg2ts[, which(veg2ts.site >= dim(veg2ts)[1]*0.1)]

# we remain we 43 out of 74 veg genera .....

pca.veg3ts <- pca(veg3ts, cor = TRUE)

n <- dim(lidar)[2]
df5 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste("G:\\CDI2017\\Imag\\Shen_trees+shrubs_seedl_PCA_GAM_response_to_enviro_vars_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){

if (length(which(is.na(lidar[,j]))) !=0) {
a1 <- which(is.na(lidar[,j]))

plot(pca.veg3ts, pch = 16, col = "steelblue")

veg3ts.site <- apply(veg3ts[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg3ts1 <- veg3ts[c(-a1),][, which(veg3ts.site > 0)]


pca.1a1<-pca(veg3ts1,cor=TRUE)

s1 <- surf(pca.1a1,lidar[c(-a1),j])

famname <- summary(s1)$family[[1]]

famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

title(main = paste("Shenandoah trees, shrubs & seedlings PCA \n", names(lidar)[j], "GAM fit"))
#title(main = paste0("Family: ",famn1, ", D^2 = ", round(summary(s1)$r.sq,4)), line = 1, cex=1)
legend(-6, -3, c(paste0("Family: ",famn1), paste0("R^2 = ", round(summary(s1)$r.sq,4)), 
 paste0("Deviance Explained: ", round(summary(s1)$dev.expl,4))), bty="n")

} else {
plot(pca.veg3ts, pch = 16, col = "steelblue")
s1 <- surf(pca.veg3ts,lidar[,j])

famname <- summary(s1)$family[[1]]

famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

title(main = paste("Shenandoah trees, shrubs & seedlings PCA \n", names(lidar)[j], "GAM fit"))
#title(main = paste0("Family: ",famn1, ", D^2 = ", round(summary(s1)$r.sq,4)), line = 1, cex=1)
legend(0.5, 7, c(paste0("Family: ",famn1), paste0("R^2 = ", round(summary(s1)$r.sq,4)), 
 paste0("Deviance Explained: ", round(summary(s1)$dev.expl,4))), bty="n")
 
}

df5[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 

}
dev.off()
}

write.csv(df5, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_PCA_GAM_04_lidar_metrics.csv", row.names = FALSE)

df6 <- df5[df5[,3]>=0.3,]
df6 <- df6[order(df6[,3], decreasing = TRUE),]

write.csv(df6, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_PCA_GAM_04_lidar_metrics.csv", row.names = FALSE)

mc1 <- unique(c(df4[,1], df6[,1]))

df6a <- df6[match(mc1, df6[,1]),]
df4a <- df4[match(mc1, df4[,1]),]
try2 <- cbind(df4a, df6a)

write.csv(try2, "G:\\CDI2017\\Veg_anal_table\\Shen_tts_tss_PCA_GAM_04_lidar_met_comp.csv", row.names = FALSE)

############# get the results from only trees as well ###############

mc2 <- unique(c(df4[,1], df6[,1], df2[,1]))

df6b <- df6[match(mc2, df6[,1]),]
df4b <- df4[match(mc2, df4[,1]),]
df2b <- df2[match(mc2, df2[,1]),]

try3 <- cbind(df2b, df4b, df6b)

write.csv(try3, "G:\\CDI2017\\Veg_anal_table\\Shen_tr_tts_tss_PCA_GAM_03_lidar_met_comp.csv", row.names = FALSE)

#########################################################################################
#
########################## Principal Coordinates Analysis ###############################
#
#########################################################################################
#
# The assumption that species exhibit monotonic (linear, strictly) responses to environment means 
# that it is really suitable for VERY short gradients. PCA is probably best suited to studying 
# within-community variation of a single community type, rather than variation among communities. 
# One significant improvement is Principal Coordinates Analysis (PCO or PCoA), which is an 
# eigen-analysis of a distance or dissimilarity matrix. 
#
##############################################################################################

library(vegan)
distr.euc <- dist(veg,'euclidean')
distr.bray <- vegdist(veg,method="bray")
distr.ruz <- dsvdis(veg,index="ruzicka")

# all trees = veg
# all trees + shrubs = vegts
# all trees + shrubs + seedlings = veg2ts

# present in >= 10% of sites trees = veg1
# present in >= 10% of sites trees + shrubs = veg1ts
# present in >= 10% of sites trees + shrubs + seedlings = veg3ts

#trees
distr.euc <- dist(veg,'euclidean')
distr.bray <- vegdist(veg,method="bray")
distr.ruz <- dsvdis(veg,index="ruzicka")

#trees + shrubs
dists.euc <- dist(vegts,'euclidean')
dists.bray <- vegdist(vegts,method="bray")
dists.ruz <- dsvdis(vegts,index="ruzicka")

#trees + shrubs + seedlings 
distss.euc <- dist(veg2ts,'euclidean')
distss.bray <- vegdist(veg2ts,method="bray")
distss.ruz <- dsvdis(veg2ts,index="ruzicka")

# In contrast to PCA, the pco() function only calculates two dimensions by default. More dimensions 
# can be specified with the k=n arguments, where n is the number of dimensions desired. Finally, 
# rather than having loadings and scores, pco()stores the plot coordinates as points and the 
# eigenvalues as eig.

# trees
euctr.pco <- pco(distr.euc,k=95)
barplot(euctr.pco$eig[1:15], names.arg = paste("PCO", 1:15, sep = ""), main = "Principal Coordinate Analysis for \nShenandoah vegetation Euclidian dissimilarity", col = rainbow(15))
sumtr.eig <- sum(euctr.pco$eig[euctr.pco$eig>0])
val.tr <- round(((euctr.pco$eig[1]+euctr.pco$eig[2])/sumtr.eig),4) 
val.tr
# [1] [1] 0.5124

barplot(euctr.pco$eig[1:15], names.arg = paste("PC", 1:15, sep = ""), main = "Principal Coordinate Analysis for \nShenandoah vegetation Euclidian dissimilarity", col = rainbow(15))
title(main = paste("The first two PCO axes account for",val.tr), line = 0, cex.main = 1)

jpeg(paste("G:\\CDI2017\\Imag\\PCO_Euclid_trees_barplot.jpg"), width=20, 
height=10,units="in", quality = 100, res=200)
barplot(euctr.pco$eig[1:15], names.arg = paste("PC", 1:15, sep = ""), main = "Principal Coordinate Analysis for \nShenandoah vegetation Euclidian dissimilarity", col = rainbow(15))
title(main = paste("The first two PCO axes account for",val.tr), line = 0, cex.main = 1)
dev.off()

# If we want to plot the projected sites on the first 2 PCO:
plot(euctr.pco)

# While Euclidean distance has a nice intuitive or familiar feel (it is after all how we see the 
# world), it often does not work well with vegetation data. Similar to the correlation coefficient 
# we used in PCA, over high beta-diversities Euclidean distance suffers a number of problems. While 
# certainly it achieves a great reduction in dimensionality (over half in two dimensions), it doesn’t
# succeed in spreading out the points very clearly. 
# Many ecologists suggest Manhattan distance as an alternative. 

distr.mht <- dist(veg,"manhattan")
mhttr.pco <- pco(distr.mht,k=37)

summhttr.eig <- sum(mhttr.pco$eig[mhttr.pco$eig>0])
mhtval.tr <- round(((mhttr.pco$eig[1]+mhttr.pco$eig[2])/summhttr.eig),4) 
mhtval.tr

# Finally, let's compare the binary version. 
distr.bin <- dist(veg,"binary")
bintr.pco <- pco(distr.bin,k=37)

sumbintr.eig <- sum(bintr.pco$eig[bintr.pco$eig>0])
binval.tr <- round(((bintr.pco$eig[1]+bintr.pco$eig[2])/sumbintr.eig),4) 
binval.tr


layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))

plot(euctr.pco$points[,1], euctr.pco$points[,2], main="Euclidian distance, trees", 
pch=16, xlab = "PCO 1", ylab = "PCO 2",cex=2, col = "dark cyan")

legend(20, 150, c(paste("The first two PCO axes \naccount for",val.tr)), bty="n", cex = 1.5)

plot(mhttr.pco$points[,1], mhttr.pco$points[,2], main = "Manhattan distance, trees", 
pch=16, xlab = "PCO 1", ylab = "PCO 2",cex=2, col = "orange")

legend(0, 270, c(paste("The first two PCO axes \naccount for",mhtval.tr)), bty="n", cex = 1.5)

plot(bintr.pco$points[,1], bintr.pco$points[,2], main = "Binary distance, trees", 
pch=16, xlab = "PCO 1", ylab = "PCO 2",cex=2, col = "dark violet")

legend(0.1, 0.44, c(paste("The first two PCO axes \naccount for",binval.tr)), bty="n", cex = 1.5)

jpeg(paste("G:\\CDI2017\\Imag\\PCO_Euclid_Manht_Binary_trees_comp.jpg"), width=20, 
height=10,units="in", quality = 100, res=200)
layout(matrix(c(1,2,3), 1, 3, byrow=TRUE))
plot(euctr.pco$points[,1], euctr.pco$points[,2], main="Euclidian distance, trees", 
pch=16, xlab = "PCO 1", ylab = "PCO 2",cex=2, col = "dark cyan")
legend(20, 150, c(paste("The first two PCO axes \naccount for",val.tr)), bty="n", cex = 1.5)
plot(mhttr.pco$points[,1], mhttr.pco$points[,2], main = "Manhattan distance, trees", 
pch=16, xlab = "PCO 1", ylab = "PCO 2",cex=2, col = "orange")
legend(0, 270, c(paste("The first two PCO axes \naccount for",mhtval.tr)), bty="n", cex = 1.5)
plot(bintr.pco$points[,1], bintr.pco$points[,2], main = "Binary distance, trees", 
pch=16, xlab = "PCO 1", ylab = "PCO 2",cex=2, col = "dark violet")
legend(0.1, 0.44, c(paste("The first two PCO axes \naccount for",binval.tr)), bty="n", cex = 1.5)
dev.off()

# Qualitatively the shape is similar, with a little more dispersion. How different are the 
# ordinations really? One way to compare is to look at the respective eigenvalues.


plot(euctr.pco$eig/sum(euctr.pco$eig),type="b",xlab="Axis Number",ylab="Fraction of Sum", 
col = "dark cyan", pch = 16, cex=1.5)

lines(mhttr.pco$eig/sum(mhttr.pco$eig),type="b",col="orange", pch = 16, cex=1.5)
lines(bintr.pco$eig/sum(bintr.pco$eig),type="b", col="dark violet", pch=16, cex=1.5)
legend(10, 0.35, c("Euclidian distance", "Manhattan distance", "Binary distance"), 
col = c("dark cyan","orange", "dark violet"), lty = c(1, 1, 1), pch = c(16, 16, 16), bty="n")

# Using the binary ordination as an example, let's look at the distribution of species in the 
# ordination space. We can use the points() function as modified by the LabDSV library.

plot(bintr.pco,title="Canopy Tress")
points(bintr.pco,veg$Quercus>0)

# We can also plot the probability of the distribution of Quercus or any other Genus. 
attach(veg)
plot(bintr.pco,title="Quercus")
points(bintr.pco,Quercus>0, pch=16)

s1 <- surf(bintr.pco,Quercus, col = "dark cyan")

round(summary(s1)$r.sq,4) 
round(summary(s1)$dev.expl,4)

n <- dim(veg)[2]
df7 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(1, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_distrib_binary_PCO_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(bintr.pco,title=paste("Shenandoah Trees \n", names(veg)[j],"Distribution \n PCO Binary Ordination"))
points(bintr.pco,veg[,j]>0, pch=16)
s1 <- surf(bintr.pco,veg[,j], col = "dark cyan")

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

legend(0.2, 0.47, c("Vegetation Plots", paste("Plots with", names(veg[j])), 
paste0("Family: ",famn1), paste0("R^2 = ", round(summary(s1)$r.sq,4)), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), pch = c(1,16, -1, -1, -1 ), 
 col = c("black", "red", NA, NA, NA), bty="n")
 
df7[j,] <- c(names(veg)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df8 <- df7[df7[,3]>=0.3,]
df8 <- df8[order(df8[,3], decreasing = TRUE),]

write.csv(df8, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_prob_distr_binary_03_PCO.csv", row.names = FALSE)
write.csv(df7, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_prob_distr_binary_PCO.csv", row.names = FALSE)

# trees + shrubs
# all trees = veg
# all trees + shrubs = vegts
# all trees + shrubs + seedlings = veg2ts

attach(vegts)
dists.bin <- dist(vegts,"binary")
bints.pco <- pco(dists.bin,k=dim(vegts)[2])

plot(bints.pco,title="Quercus")
points(bints.pco,Quercus>0, pch=16)

s1 <- surf(bints.pco,Quercus, col = "dark cyan")

round(summary(s1)$r.sq,4) 
round(summary(s1)$dev.expl,4)

n <- dim(vegts)[2]
df9 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(1, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_distrib_binary_PCO_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(bints.pco,title=paste("Shenandoah Trees & Shrubs \n", names(vegts)[j],"Distribution \n PCO Binary Ordination"))
points(bints.pco,vegts[,j]>0, pch=16)
s1 <- surf(bints.pco,vegts[,j], col = "dark cyan")

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

legend(0.25, -0.2, c("Vegetation Plots", paste("Plots with", names(vegts[j])), 
paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), pch = c(1,16, -1, -1, -1 ), 
 col = c("black", "red", NA, NA, NA), bty="n")
 
df9[j,] <- c(names(vegts)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df10 <- df9[df9[,3]>=0.3,]
df10 <- df10[order(df10[,3], decreasing = TRUE),]

write.csv(df10, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_prob_distr_binary_03_PCO.csv", row.names = FALSE)
write.csv(df9, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs_prob_distr_binary_PCO.csv", row.names = FALSE)


### Trees + shrubs + seedlings

# all trees + shrubs + seedlings = veg2ts
attach(veg2ts)

distss.bin <- dist(veg2ts,"binary")
bintss.pco <- pco(distss.bin,k=dim(veg2ts)[2])

plot(bintss.pco,title="Quercus")
points(bintss.pco,Quercus>0, pch=16)

s1 <- surf(bintss.pco,Quercus, col = "dark cyan")

round(summary(s1)$r.sq,4) 
round(summary(s1)$dev.expl,4)

n <- dim(veg2ts)[2]
df11 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(1, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs+seedl_distrib_binary_PCO_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(bintss.pco,title=paste("Shenandoah Trees & Shrubs & Seedlings \n", names(veg2ts)[j],"Distribution \n PCO Binary Ordination"))
points(bintss.pco,veg2ts[,j]>0, pch=16)
s1 <- surf(bintss.pco,veg2ts[,j], col = "dark cyan")

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

legend(0.2, -0.27, c("Vegetation Plots", paste("Plots with", names(veg2ts[j]))), 
 pch = c(1,16), col = c("black", "red", NA, NA, NA), bty="n")
 
 legend(0.25, 0.42, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df11[j,] <- c(names(veg2ts)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df12 <- df11[df11[,3]>=0.3,]
df12 <- df12[order(df12[,3], decreasing = TRUE),]

write.csv(df12, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_distr_binary_03_PCO.csv", row.names = FALSE)
write.csv(df11, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs+seedl_distr_binary_PCO.csv", row.names = FALSE)

df8$veg = "tr"
df10$veg = "ts"
df12$veg = "tss"

mc1 <- unique(c(df8[,1], df10[,1], df12[,1]))

df8a <- df8[match(mc1, df8[,1]),]
df10a <- df10[match(mc1, df10[,1]),]
df12a <- df12[match(mc1, df12[,1]),]

try3 <- cbind(df8a, df10a, df12a)

write.csv(try3, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_distr_binary_03_PCO_comp.csv", row.names = FALSE)

# let's see how the lidar metrics do on the binary PCO ordination
# trees
# lidar metrics are in = lidar

plot(bintr.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees Binary Ordination"))
surf(bintr.pco,lidar[,j])

n <- dim(lidar)[2]
df13 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_binary_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(bintr.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees Binary Ordination"))
s1 <- surf(bintr.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(0.2, 0.45, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df13[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df14 <- df13[df13[,3]>=0.2,]
df14 <- df14[order(df14[,3], decreasing = TRUE),]

write.csv(df14, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_distr_binary_02_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df13, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_distr_binary_PCO_lidar_metrics.csv", row.names = FALSE)

# trees + shrubs

plot(bints.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs Binary Ordination"))
surf(bints.pco,lidar[,j])

n <- dim(lidar)[2]
df15 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_binary_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(bints.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs Binary Ordination"))
s1 <- surf(bints.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(0.2, -0.25, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df15[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df16 <- df15[df15[,3]>=0.2,]
df16 <- df16[order(df16[,3], decreasing = TRUE),]

write.csv(df16, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_distr_binary_02_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df15, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs_distr_binary_PCO_lidar_metrics.csv", row.names = FALSE)

# trees + shrubs + seedlings

plot(bintss.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs & Seedlings \n Binary Ordination"))
surf(bintss.pco,lidar[,j])

n <- dim(lidar)[2]
df17 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs+seedl_binary_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(bintss.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs & Seedlings \n Binary Ordination"))
s1 <- surf(bintss.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(0.2, -0.25, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df17[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df18 <- df17[df17[,3]>=0.3,]
df18 <- df18[order(df18[,3], decreasing = TRUE),]

write.csv(df18, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_distr_binary_02_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df17, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs+seedl_distr_binary_PCO_lidar_metrics.csv", row.names = FALSE)

df14$veg = "tr"
df16$veg = "ts"
df18$veg = "tss"

mc1 <- unique(c(df14[,1], df16[,1], df18[,1]))

df14a <- df14[match(mc1, df14[,1]),]
df16a <- df16[match(mc1, df16[,1]),]
df18a <- df18[match(mc1, df18[,1]),]

try4 <- cbind(df14a, df16a, df18a)

write.csv(try4, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_distr_binary_03_PCO_comp_lidar_metrics.csv", row.names = FALSE)

### what about other different ordinations? We can test which of the PCOs is better with regard 
# to possible lidar metrics
# Euclidian Distance
# Manhattan
# Bray/Curtis


#trees
distr.euc <- dist(veg,'euclidean')
distr.bray <- vegdist(veg,method="bray")
distr.mht <- dist(veg,"manhattan")

euctr.pco <- pco(distr.euc,k=dim(veg)[2])
mhttr.pco <- pco(distr.mht,k=dim(veg)[2])
braytr.pco <- pco(distr.bray,k=dim(veg)[2])

#trees + shrubs
dists.euc <- dist(vegts,'euclidean')
dists.bray <- vegdist(vegts,method="bray")
dists.mht <- dist(vegts,"manhattan")

eucts.pco <- pco(dists.euc,k=dim(vegts)[2])
mhtts.pco <- pco(dists.mht,k=dim(vegts)[2])
brayts.pco <- pco(dists.bray,k=dim(vegts)[2])

#trees + shrubs + seedlings 
distss.euc <- dist(veg2ts,'euclidean')
distss.bray <- vegdist(veg2ts,method="bray")
distss.mht <- dist(veg2ts,"manhattan")

euctss.pco <- pco(distss.euc,k=dim(veg2ts)[2])
mhttss.pco <- pco(distss.mht,k=dim(veg2ts)[2])
braytss.pco <- pco(distss.bray,k=dim(veg2ts)[2])

#trees Euclidean

plot(euctr.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees Euclidean Ordination"))
surf(euctr.pco,lidar[,j])

n <- dim(lidar)[2]
df19 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_euclid_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(euctr.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees Euclidean Ordination"))
s1 <- surf(euctr.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(75, 170, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df19[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df20 <- df19[df19[,3]>=0.3,]
df20 <- df20[order(df20[,3], decreasing = TRUE),]

write.csv(df20, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_distr_euclid_02_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df19, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_distr_euclid_PCO_lidar_metrics.csv", row.names = FALSE)

#trees Manhattan

plot(mhttr.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees Manhattan Ordination"))
surf(mhttr.pco,lidar[,j])

n <- dim(lidar)[2]
df21 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_manhat_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(mhttr.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees Manhattan Ordination"))
s1 <- surf(mhttr.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(100, 300, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df21[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df22 <- df21[df21[,3]>=0.3,]
df22 <- df22[order(df22[,3], decreasing = TRUE),]

write.csv(df22, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_distr_manhat_03_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df21, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_distr_manhat_PCO_lidar_metrics.csv", row.names = FALSE)

# Bray-Curtis trees

plot(braytr.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees Bray-Curtis Ordination"))
surf(braytr.pco,lidar[,j])

n <- dim(lidar)[2]
df23 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_bray_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(braytr.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees Bray-Curtis Ordination"))
s1 <- surf(braytr.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-0.5, 0.45, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df23[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df24 <- df23[as.numeric(df23[,3])>=0.3,]
df24 <- df24[order(df24[,3], decreasing = TRUE),]

# for whatever reason it understands 9e-04 as maybe 9 or something because in results it gets it
# even if i want everything that is >= 0.3. What is very weird is that at R prompt
# 9e-04 >= 0.3 is FALSE. it seems it does not read it as.numeric from the column ..... very weird.

write.csv(df24, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_distr_bray_03_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df23, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_distr_bray_PCO_lidar_metrics.csv", row.names = FALSE)

# For trees binary my tables are: df13 and df14 

# Euclidian

df20$Ordination <- "Euclidean"
df22$Ordination <- "Manhattan"
df24$Ordination <- "Bray-Curtis"
df14 <- df14[,-5]
df14$Ordination <- "Binary"

mc1 <- unique(c(df14[,1], df20[,1], df22[,1], df24[,1]))
mc1 <- mc1[order(mc1)]

df14a <- df14[match(mc1, df14[,1]),]
df20a <- df20[match(mc1, df20[,1]),]
df22a <- df22[match(mc1, df22[,1]),]
df24a <- df24[match(mc1, df24[,1]),]

try5 <- cbind(df20a, df22a, df14a, df24a)
try5[,1] <- mc1
try5 <- try5[,c(-6, -11, -16)]

names(try5)[c(3, 4, 7, 8, 11, 12, 15, 16)] <- c("Euclidean_R_sq", "Euclidean_D_sq",
"Manhattan_R_sq", "Manhattan_D_sq", "Binary_R_sq", "Binary_D_sq", "Bray_Curtis_R_sq",
"Bray_Curtis_D_sq")

try6 <- try5[, c(1, 3, 4, 7, 8, 11, 12, 15, 16)]

#write.csv(try5, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_distr_compar_03_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(try6, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_distr_compar_03_PCO_lidar_metrics.csv", row.names = FALSE)

##############################################################################################
#
#trees + shrubs Euclidean
#
###############################################################################################

eucts.pco <- pco(dists.euc,k=dim(vegts)[2])
mhtts.pco <- pco(dists.mht,k=dim(vegts)[2])
brayts.pco <- pco(dists.bray,k=dim(vegts)[2])


plot(eucts.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs Euclidean Ordination"))
surf(eucts.pco,lidar[,j])

n <- dim(lidar)[2]
df25 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_euclid_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(eucts.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs \n Euclidean Ordination"))
s1 <- surf(eucts.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-170, -125, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df25[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df26 <- df25[df25[,3]>=0.3,]
df26 <- df26[order(df26[,3], decreasing = TRUE),]

write.csv(df26, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_dist_euclid_02_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df25, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs_dist_euclid_PCO_lidar_metrics.csv", row.names = FALSE)

#trees & shrubs Manhattan

plot(mhtts.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs Manhattan Ordination"))
surf(mhtts.pco,lidar[,j])

n <- dim(lidar)[2]
df27 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_manhat_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(mhtts.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs \n Manhattan Ordination"))
s1 <- surf(mhtts.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-250, -125, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df27[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df28 <- df27[df21[,3]>=0.3,]
df28 <- df28[order(df28[,3], decreasing = TRUE),]

write.csv(df28, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_distr_manhat_03_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df27, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs_distr_manhat_PCO_lidar_metrics.csv", row.names = FALSE)

# Bray-Curtis trees & shrubs

plot(brayts.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs \n Bray-Curtis Ordination"))
surf(brayts.pco,lidar[,j])

n <- dim(lidar)[2]
df29 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_bray_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(brayts.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs \n Bray-Curtis Ordination"))
s1 <- surf(brayts.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-0.5, 0.45, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df29[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df30 <- df29[as.numeric(df29[,3])>=0.3,]
df30 <- df30[order(df30[,3], decreasing = TRUE),]

# for whatever reason it understands 9e-04 as maybe 9 or something because in results it gets it
# even if i want everything that is >= 0.3. What is very weird is that at R prompt
# 9e-04 >= 0.3 is FALSE. it seems it does not read it as.numeric from the column ..... very weird.

write.csv(df30, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_distr_bray_03_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df29, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs_distr_bray_PCO_lidar_metrics.csv", row.names = FALSE)

# For trees+shrubs binary my tables are: df15 and df16 

# Euclidean

df26$Ordination <- "Euclidean"
df28$Ordination <- "Manhattan"
df30$Ordination <- "Bray-Curtis"
df16 <- df16[,-5]
df16$Ordination <- "Binary"

mc1 <- unique(c(df16[,1], df26[,1], df28[,1], df30[,1]))
mc1 <- mc1[order(mc1)]

df16a <- df16[match(mc1, df16[,1]),]
df26a <- df26[match(mc1, df26[,1]),]
df28a <- df28[match(mc1, df28[,1]),]
df30a <- df30[match(mc1, df30[,1]),]

try7 <- cbind(df26a, df28a, df16a, df30a)
try7[,1] <- mc1
try7 <- try7[,c(-6, -11, -16)]

names(try7)[c(3, 4, 7, 8, 11, 12, 15, 16)] <- c("Euclidean_R_sq", "Euclidean_D_sq",
"Manhattan_R_sq", "Manhattan_D_sq", "Binary_R_sq", "Binary_D_sq", "Bray_Curtis_R_sq",
"Bray_Curtis_D_sq")

try8 <- try7[, c(1, 3, 4, 7, 8, 11, 12, 15, 16)]

#write.csv(try5, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_distr_compar_03_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(try8, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_distr_compar_03_PCO_lidar_metrics.csv", row.names = FALSE)

###########################################################################################
#
#trees+shrubs+seedlings
#
############################################################################################

# Euclidean trees+shrubs+seedlings

plot(euctss.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs $ Seedlings \n Euclidean Ordination"))
surf(euctss.pco,lidar[,j])

n <- dim(lidar)[2]
df31 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs+seedl_euclid_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(euctss.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs & Seedlings \n Euclidean Ordination"))
s1 <- surf(euctss.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-150, -115, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df31[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df32 <- df31[as.numeric(df31[,3])>=0.3,]
df32 <- df32[order(df32[,3], decreasing = TRUE),]

write.csv(df32, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_dist_euclid_02_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df31, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs_seedl_dist_euclid_PCO_lidar_metrics.csv", row.names = FALSE)

#trees & shrubs & seedlings Manhattan

n <- dim(lidar)[2]
df33 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs+seedl_manhat_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(mhttss.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs & Seedlings\n Manhattan Ordination"))
s1 <- surf(mhttss.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-400, 270, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df33[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df34 <- df33[df33[,3]>=0.3,]
df34 <- df34[order(df34[,3], decreasing = TRUE),]

write.csv(df34, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_distr_manhat_03_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df33, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs+seedl_distr_manhat_PCO_lidar_metrics.csv", row.names = FALSE)

# Bray-Curtis trees & shrubs & seedlings

n <- dim(lidar)[2]
df35 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs+seedl_bray_PCO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(braytss.pco,title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs & Seedlings \n Bray-Curtis Ordination"))
s1 <- surf(braytss.pco,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-0.53, -0.32, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
df35[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df36 <- df35[as.numeric(df35[,3])>=0.3,]
df36 <- df36[order(df36[,3], decreasing = TRUE),]

# for whatever reason it understands 9e-04 as maybe 9 or something because in results it gets it
# even if i want everything that is >= 0.3. What is very weird is that at R prompt
# 9e-04 >= 0.3 is FALSE. it seems it does not read it as.numeric from the column ..... very weird.

write.csv(df36, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_distr_bray_03_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(df35, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs+seedl_distr_bray_PCO_lidar_metrics.csv", row.names = FALSE)

# For trees+shrubs binary my tables are: df17 and df18 

# Euclidian

df32$Ordination <- "Euclidean"
df34$Ordination <- "Manhattan"
df36$Ordination <- "Bray-Curtis"
df18 <- df18[,-5]
df18$Ordination <- "Binary"

mc1 <- unique(c(df18[,1], df36[,1], df34[,1], df32[,1]))
mc1 <- mc1[order(mc1)]

df18a <- df18[match(mc1, df18[,1]),]
df32a <- df32[match(mc1, df32[,1]),]
df34a <- df34[match(mc1, df34[,1]),]
df36a <- df36[match(mc1, df36[,1]),]

try9 <- cbind(df32a, df34a, df18a, df36a)
try9[,1] <- mc1
try9 <- try9[,c(-6, -11, -16)]

names(try9)[c(3, 4, 7, 8, 11, 12, 15, 16)] <- c("Euclidean_R_sq", "Euclidean_D_sq",
"Manhattan_R_sq", "Manhattan_D_sq", "Binary_R_sq", "Binary_D_sq", "Bray_Curtis_R_sq",
"Bray_Curtis_D_sq")

try10 <- try9[, c(1, 3, 4, 7, 8, 11, 12, 15, 16)]

write.csv(try10, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_distr_compar_03_PCO_lidar_metrics.csv", row.names = FALSE)

# It's not apparent here on a single data set, but the relative merits of the metrics/dissimilarities 
# differ depending on the beta-diversity (or gradient widths) of the data set. PCO of a euclidean 
# distance matrix is equivalent to PCA, and is best suited to VERY short gradients or VERY low 
# beta-diversity. As beta-diversity increases, the balance will shift from Bray/Curtis to binary as 
# the best index. 

##########################################################################################
#
#             Non-metric Multidimensional Scaling
#
##########################################################################################

# To calculate a NMDS, simply use the nmds() function on a dissimilarity/distance matrix as follows. 

# trees

distr.euc <- dist(veg,'euclidean')
distr.bray <- vegdist(veg,method="bray")
distr.mht <- dist(veg,"manhattan")
distr.bin <- dist(veg,"binary")

# have no idea why using Bray_curtis in this case .... i will probably do it for all 4 distances ....
braytr.nmds <- nmds(distr.bray)
euctr.nmds <- nmds(distr.euc)
mhttr.nmds <- nmds(distr.mht)
bintr.nmds <- nmds(distr.bin) # does not work with 0 or negative distance

jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_NMDS_comp_correl.jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,6), 2, 3, byrow=TRUE))

plot(braytr.nmds, title = "Bray-Curtis NMDS, 2 dimensions")
plot(euctr.nmds, title = "Euclidean NMDS, 2 dimensions")
plot(mhttr.nmds, title = "Manhattan NMDS, 2 dimensions")

ordcomp(braytr.nmds,distr.bray, title = "Correlation: Bray-Curtis trees \n Ordination dissimilarities vs.\n computed distance" )
ordcomp(euctr.nmds,distr.euc, title = "Correlation: Euclidean trees \n Ordination dissimilarities vs.\n computed distance" )
ordcomp(mhttr.nmds,distr.mht, title = "Correlation: Manhattan trees \n Ordination dissimilarities vs.\n computed distance" )

dev.off()

########## look at lidar metrics
########## still only trees for now

#trees Euclidean

n <- dim(lidar)[2]
df37 <- data.frame(Lidar_metrics=NA, Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_euclid_NMDS_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(euctr.nmds,cex = c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE)))),
title=paste(names(lidar[j]), "\n Shenandoah Trees NMDS Euclidean Ordination"), 
col = adjustcolor( "steelblue", alpha.f = 0.6), pch=20)
s1 <- surf(euctr.nmds,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

 legend(75, -70, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
 legend(85, 170, c(names(lidar)[j], "Min. value", "Max. value"), pt.cex = c(NA, min(cex1, na.rm = TRUE), max(cex1, na.rm = TRUE)),
 col = c(NA,adjustcolor( "steelblue", alpha.f = 0.6), adjustcolor( "steelblue", alpha.f = 0.6)),
pch = c(NA, 20, 20), bty="n")
 
df37[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df38 <- df37[df37[,3]>=0.3,]
df38 <- df38[order(df38[,3], decreasing = TRUE),]

write.csv(df38, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_NMDS_euclid_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df37, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_NMDS_euclid_lidar_metrics.csv", row.names = FALSE)

#trees Manhattan

n <- dim(lidar)[2]
df39 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_manhat_NMDS_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(mhttr.nmds,cex = c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE)))),
title=paste(names(lidar[j]), "\n Shenandoah Trees NMDS Manhattan Ordination"), 
col = adjustcolor( "steelblue", alpha.f = 0.6), pch=20)
s1 <- surf(euctr.nmds,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

 legend(-250, 300, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
 legend(100, 300, c(names(lidar)[j], "Min. value", "Max. value"), pt.cex = c(NA, min(cex1, na.rm = TRUE), max(cex1, na.rm = TRUE)),
 col = c(NA,adjustcolor( "steelblue", alpha.f = 0.6), adjustcolor( "steelblue", alpha.f = 0.6)),
pch = c(NA, 20, 20), bty="n")
 
df39[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df40 <- df39[df39[,3]>=0.3,]
df40 <- df40[order(df40[,3], decreasing = TRUE),]

write.csv(df40, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_NMDS_manhat_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df39, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_NMDS_manhat_lidar_metrics.csv", row.names = FALSE)

# Bray-Curtis trees

n <- dim(lidar)[2]
df41 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_braycurtis_NMDS_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(braytr.nmds,cex = c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE)))),
title=paste(names(lidar[j]), "\n Shenandoah Trees NMDS Bray-Curtis Ordination"), 
col = adjustcolor( "steelblue", alpha.f = 0.6), pch=20)
s1 <- surf(braytr.nmds,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

 legend(0.5, 0.75, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
 legend(-0.75, 0.75, c(names(lidar)[j], "Min. value", "Max. value"), pt.cex = c(NA, min(cex1, na.rm = TRUE), max(cex1, na.rm = TRUE)),
 col = c(NA,adjustcolor( "steelblue", alpha.f = 0.6), adjustcolor( "steelblue", alpha.f = 0.6)),
pch = c(NA, 20, 20), bty="n")
 
df41[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df42 <- df41[as.numeric(df41[,3])>=0.3,]
df42 <- df42[order(df42[,3], decreasing = TRUE),]

# for whatever reason it understands 9e-04 as maybe 9 or something because in results it gets it
# even if i want everything that is >= 0.3. What is very weird is that at R prompt
# 9e-04 >= 0.3 is FALSE. it seems it does not read it as.numeric from the column ..... very weird.

write.csv(df42, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_NMDS_bray_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df41, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_NMDS_bray_lidar_metrics.csv", row.names = FALSE)

# combining the 3 runs for the 3 distances 

df38$Ordination <- "Euclidean"
df40$Ordination <- "Manhattan"
df42$Ordination <- "Bray-Curtis"


mc1 <- unique(c(df38[,1], df40[,1], df42[,1]))
mc1 <- mc1[order(mc1)]

df38a <- df38[match(mc1, df38[,1]),]
df40a <- df40[match(mc1, df40[,1]),]
df42a <- df42[match(mc1, df42[,1]),]

try11 <- cbind(df38a, df40a, df42a)
try11[,1] <- mc1
try11 <- try11[,c(-6, -11)]

names(try11)[c(3, 4, 7, 8, 11, 12)] <- c("Euclidean_R_sq", "Euclidean_D_sq",
"Manhattan_R_sq", "Manhattan_D_sq", "Binary_R_sq", "Binary_D_sq", "Bray_Curtis_R_sq",
"Bray_Curtis_D_sq")

try12 <- try11[, c(1, 3, 4, 7, 8, 11, 12)]

#write.csv(try12, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_distr_compar_03_PCO_lidar_metrics.csv", row.names = FALSE)
write.csv(try12, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_NMDS_compar_03_lidar_metrics.csv", row.names = FALSE)


# Clearly Bray-Curtis does well better for NMDS than the other dis tances so i will use only Bray-
# Curtis from now on for trees+shrubs and trees+shrubs+seedlings

##############################################################################################
#
#trees + shrubs 
#
###############################################################################################

dists.bray <- vegdist(vegts,method="bray")
brayts.nmds <- nmds(dists.bray)

n <- dim(lidar)[2]
df43 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_braycurtis_NMDS_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(brayts.nmds,cex = c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE)))),
title=paste(names(lidar[j]), "\n Shenandoah Trees NMDS Bray-Curtis Ordination"), 
col = adjustcolor( "steelblue", alpha.f = 0.6), pch=20)
s1 <- surf(brayts.nmds,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

 legend(-0.9, -0.6, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
 legend(-0.8, 0.75, c(names(lidar)[j], "Min. value", "Max. value"), pt.cex = c(NA, min(cex1, na.rm = TRUE), max(cex1, na.rm = TRUE)),
 col = c(NA,adjustcolor( "steelblue", alpha.f = 0.6), adjustcolor( "steelblue", alpha.f = 0.6)),
pch = c(NA, 20, 20), bty="n")
 
df43[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df44 <- df43[as.numeric(df43[,3])>=0.3,]
df44 <- df44[order(df44[,3], decreasing = TRUE),]

write.csv(df44, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_NMDS_braycurtis_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df43, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs_NMDS_braycurtis_lidar_metrics.csv", row.names = FALSE)


###########################################################################################
#
#trees+shrubs+seedlings
#
############################################################################################

# Bray-Curtis trees & shrubs & seedlings

distss.bray <- vegdist(veg2ts,method="bray")
braytss.nmds <- nmds(distss.bray)

n <- dim(lidar)[2]
df45 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs+seedlings_braycurtis_NMDS_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
plot(braytss.nmds,cex = c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE)))),
title=paste(names(lidar[j]), "\n Shenandoah Trees NMDS Bray-Curtis Ordination"), 
col = adjustcolor( "steelblue", alpha.f = 0.6), pch=20)
s1 <- surf(braytss.nmds,lidar[,j])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

 legend(-0.7, -0.35, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
 legend(-0.6, 0.65, c(names(lidar)[j], "Min. value", "Max. value"), pt.cex = c(NA, min(cex1, na.rm = TRUE), max(cex1, na.rm = TRUE)),
 col = c(NA,adjustcolor( "steelblue", alpha.f = 0.6), adjustcolor( "steelblue", alpha.f = 0.6)),
pch = c(NA, 20, 20), bty="n")
 
df45[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df46 <- df45[as.numeric(df45[,3])>=0.3,]
df46 <- df46[order(df46[,3], decreasing = TRUE),]

write.csv(df46, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_NMDS_braycurtis_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df45, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs+seedl_NMDS_braycurtis_lidar_metrics.csv", row.names = FALSE)

# combines the 3 Bray_Cuetis NMDS runs

# trees df42
#trees+shrubs df44
#trees+shrubs+seedlings df46

df42 <- df42[,-5]
df42$veg <- "tr"
df44$veg <- "ts"
df46$veg <- "tss"


mc1 <- unique(c(df42[,1], df44[,1], df46[,1]))
mc1 <- mc1[order(mc1)]

df42a <- df42[match(mc1, df42[,1]),]
df44a <- df44[match(mc1, df44[,1]),]
df46a <- df46[match(mc1, df46[,1]),]

try13 <- cbind(df42a, df44a, df46a)
try13[,1] <- mc1
try13 <- try13[,c(-6, -11)]

names(try13)[c(3, 4, 7, 8, 11, 12)] <- c("Tr_BC_R_sq", "Tr_BC_D_sq",
"Ts_BC_R_sq", "Ts_BC_D_sq", "Tss_BC_R_sq", "Tss_BC_D_sq")

try14 <- try13[, c(1, 3, 4, 7, 8, 11, 12)]

write.csv(try14, "G:\\CDI2017\\Veg_anal_table\\Shen_allveg_NMDS_compar_03_lidar_metrics.csv", row.names = FALSE)

# making the plots only for lidar metrics that have R^2 >= 0.3

# trees

n <- dim(df42)[1]

nm.tr <- df42[,1]

 ind1 <- c()
 for (i in 1:n){
ind1[i]<- which(colnames(lidar)==nm.tr[i])

}


for (i in seq(1, n, 6)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_braycurtis_NMDS_03_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,6), 2, 3, byrow=TRUE))

for (j in i:(i+5)){
if (j <= n){
plot(braytr.nmds,cex = c(1+(2*(lidar[,ind1[j]]-min(lidar[,ind1[j]], na.rm = TRUE))/(max(lidar[,ind1[j]], na.rm = TRUE)-min(lidar[,ind1[j]], na.rm = TRUE)))),
title=paste(names(lidar[ind1[j]]), "\n Shenandoah Trees NMDS Bray-Curtis Ordination"), 
col = adjustcolor( "steelblue", alpha.f = 0.6), pch=20)
s1 <- surf(braytr.nmds,lidar[,ind1[j]])

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))
cex1 =  c(1+(2*(lidar[,ind1[j]]-min(lidar[,ind1[j]], na.rm = TRUE))/(max(lidar[,ind1[j]], na.rm = TRUE)-min(lidar[,ind1[j]], na.rm = TRUE))))

 legend(0.75, 0.75, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4))), bty="n")
 
 legend(-1, 0.75, c(names(lidar)[ind1[j]], "Min. value", "Max. value"), pt.cex = c(NA, min(cex1, na.rm = TRUE), max(cex1, na.rm = TRUE)),
 col = c(NA,adjustcolor( "steelblue", alpha.f = 0.6), adjustcolor( "steelblue", alpha.f = 0.6)),
pch = c(NA, 20, 20), bty="n")
 
}
}
dev.off()
}

####################################################################################
#
#           Correspondence Analysis and Detrended Correspondence Analysis
#
######################################################################################

# PCA is specifically an eigenanalysis of a correlation or covariance matrix. PCO is an eigenanalysis 
# of a broad variety of symmetric dissimilarity/distance matrices. CA is an eigenanalysis of a 
# Chi-square distance matrix. A Chi-square distance matrix is defined by the deviation from 
# expectation. Similar to the Chi-square statistic, it is defined as

# (observed - expected) / sqrt(expected)

# where observed and expected are abundances of species in sample plots. Species which occur in a 
# sample plot in an abundance greater than expected have positive values, and species which occur 
# less then expected or which are absent have negative values. Expected values are based on the row 
# and column sums of the vegetation matrix.

# In contrast to the ordination methods we have employed so far, CA provides a configuration for 
# species as well as sample plots in the ordination. That is, species centroids (the hypothetical 
# mode of their distribution along the eigenvectors) can be plotted as well as the locations of 
# the plots. This dual nature is one of the characteristics that first attracted ecologists to CA. 

source("F:\\R_scripts\\coresp_anal_labsdv.r")

ca.tr <- ca(veg) # trees
ca.ts <- ca(vegts) # trees + shrubs
ca.tss <- ca(veg2ts) # trees + shrubs + seedlings

plot(ca.tr,scaling=1, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title = "Shenandoah Trees Correspondence Analysis")

specid(ca.tr,scaling=1,names(veg), col = "forestgreen")
plotid(ca.tr,scaling=1,dimnames(veg)[[1]], cex = 0.8)

layout(matrix(seq(1,3), 1, 3, byrow=TRUE))
#trees
plot(ca.tr,scaling=1, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title = "Shenandoah Trees \n Correspondence Analysis")

specid(ca.tr,scaling=1,names(veg), col = "forestgreen")
plotid(ca.tr,scaling=1,dimnames(veg)[[1]], cex = 0.8)

#trees + shrubs
plot(ca.ts,scaling=1, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title = "Shenandoah Trees & Shrubs \n Correspondence Analysis")

specid(ca.ts,scaling=1,names(veg), col = "forestgreen")
plotid(ca.ts,scaling=1,dimnames(veg)[[1]], cex = 0.8)

#trees+shrubs+seedlings
plot(ca.tss,scaling=1, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title = "Shenandoah Trees & Shrubs & Seedlings \n Correspondence Analysis")

specid(ca.tss,scaling=1,names(veg), col = "forestgreen")
plotid(ca.tss,scaling=1,dimnames(veg)[[1]], cex = 0.8)

# do the same but with vegetation at least in 10% or more presence in plots

#only trees
veg.site <- apply(veg, 2, function(x) y=length(x[x!=0]))
veg.tr <- veg[, which(veg.site >= dim(veg)[1]*0.1)]

ca.tr01 <- ca(veg.tr)

#trees and shrubs
vegts.site <- apply(vegts, 2, function(x) y=length(x[x!=0]))
vegts.ts <- vegts[, which(vegts.site >= dim(vegts)[1]*0.1)]

ca.ts01 <- ca(vegts.ts) 

# trees+shrubs+seedlings

veg2ts.site <- apply(veg2ts, 2, function(x) y=length(x[x!=0]))
veg2ts.tss <- veg2ts[, which(veg2ts.site >= dim(veg2ts)[1]*0.1)]

ca.tss01 <- ca(veg2ts.tss) # trees + shrubs + seedlings

layout(matrix(seq(1,3), 1, 3, byrow=TRUE))
#trees
plot(ca.tr01,scaling=1, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title = "Shenandoah Trees \n Presence >= 10% Plots \n Correspondence Analysis")

specid(ca.tr01,scaling=1,names(veg), col = "forestgreen")
plotid(ca.tr01,scaling=1,dimnames(veg)[[1]], cex = 0.8)

#trees + shrubs
plot(ca.ts01,scaling=1, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title = "Shenandoah Trees & Shrubs \n Presence >= 10% Plots\n Correspondence Analysis")

specid(ca.ts01,scaling=1,names(veg), col = "forestgreen")
plotid(ca.ts01,scaling=1,dimnames(veg)[[1]], cex = 0.8)

#trees+shrubs+seedlings
plot(ca.tss01,scaling=1, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title = "Shenandoah Trees & Shrubs & Seedlings \n Presence >= 10% Plots \n Correspondence Analysis")

specid(ca.tss01,scaling=1,names(veg), col = "forestgreen")
plotid(ca.tss01,scaling=1,dimnames(veg)[[1]], cex = 0.8)

########### looking at lidar metrics on the CA ordination

plot(ca.tr,scaling=2, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title = "Shenandoah Trees \n Correspondence Analysis")

s1 <- surf(ca.tr,lidar[,j],scaling=2, col = "steelblue")

n <- dim(lidar)[2]
df47 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_CA_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
if (length(which(is.na(lidar[,j]))) !=0) {
a1 <- which(is.na(lidar[,j]))

plot(ca.tr,scaling=2, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title=paste(names(lidar[j]), "\n Shenandoah Trees Correspondence Analysis")) 

veg.site <- apply(veg[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg.tr1 <- veg[c(-a1),][, which(veg.site > 0)]
ca.tr1 <- ca(veg.tr1)
lidar1 <- lidar[-a1, j]
s1 <- surf(ca.tr1,lidar1, scaling=2, col = "steelblue")

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-4, 5, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4)), "Plots","Genus name" ), bty="n",
 pt.cex = c(NA, NA, NA, 2, 2), pch = c(NA, NA, NA, 20, 17), 
 col = c(NA, NA, NA,adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)))
 
 } else {
 plot(ca.tr,scaling=2, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title=paste(names(lidar[j]), "\n Shenandoah Trees Correspondence Analysis")) 

s1 <- surf(ca.tr,lidar[,j], scaling=2, col = "steelblue")

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-4, 5, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4)), "Plots","Genus name" ), bty="n",
 pt.cex = c(NA, NA, NA, 2, 2), pch = c(NA, NA, NA, 20, 17), 
 col = c(NA, NA, NA,adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)))
 
 }
 df47[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df48 <- df47[as.numeric(df47[,3])>=0.3,]
df48 <- df48[order(df48[,3], decreasing = TRUE),]

write.csv(df48, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_CA_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df47, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_CA_lidar_metrics.csv", row.names = FALSE)

### trees+shrubs CA

n <- dim(lidar)[2]
df49 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_CA_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
if (length(which(is.na(lidar[,j]))) !=0) {
a1 <- which(is.na(lidar[,j]))

plot(ca.ts,scaling=2, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs \n Correspondence Analysis")) 

vegts.site <- apply(vegts[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg.ts1 <- vegts[c(-a1),][, which(vegts.site > 0)]
ca.ts1 <- ca(veg.ts1)
lidar1 <- lidar[-a1, j]
s1 <- surf(ca.ts1,lidar1, scaling=2, col = "steelblue")

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(2, -3, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4)), "Plots","Genus name" ), bty="n",
 pt.cex = c(NA, NA, NA, 2, 2), pch = c(NA, NA, NA, 20, 17), 
 col = c(NA, NA, NA,adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)))
 
 } else {
 plot(ca.tr,scaling=2, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title=paste(names(lidar[j]), "\n Shenandoah Trees $ Shrubs \n Correspondence Analysis")) 

s1 <- surf(ca.ts,lidar[,j], scaling=2, col = "steelblue")

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-4, 5, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4)), "Plots","Genus name" ), bty="n",
 pt.cex = c(NA, NA, NA, 2, 2), pch = c(NA, NA, NA, 20, 17), 
 col = c(NA, NA, NA,adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)))
 
 }
 df49[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df50 <- df49[as.numeric(df49[,3])>=0.3,]
df50 <- df50[order(df50[,3], decreasing = TRUE),]

write.csv(df50, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_CA_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df49, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs_CA_lidar_metrics.csv", row.names = FALSE)

# trees+shrubs+seedlings CA

n <- dim(lidar)[2]
df51 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_seedl_CA_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
if (length(which(is.na(lidar[,j]))) !=0) {
a1 <- which(is.na(lidar[,j]))

plot(ca.tss,scaling=2, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title=paste(names(lidar[j]), "\n Shenandoah Trees & Shrubs & Seedlings \n Correspondence Analysis")) 

veg2ts.site <- apply(veg2ts[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg.tss1 <- veg2ts[c(-a1),][, which(veg2ts.site > 0)]
ca.tss1 <- ca(veg.tss1)
lidar1 <- lidar[-a1, j]
s1 <- surf(ca.tss1,lidar1, scaling=2, col = "steelblue")

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-4, 6, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4)), "Plots","Genus name" ), bty="n",
 pt.cex = c(NA, NA, NA, 2, 2), pch = c(NA, NA, NA, 20, 17), 
 col = c(NA, NA, NA,adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)))
 
 } else {
 plot(ca.tss,scaling=2, pltpch = 20, pltcol = adjustcolor( "brown", alpha.f = 0.6),pltcex = 2,
spcpch = 17, spccol = adjustcolor( "forestgreen", alpha.f = 0.6), spccex = 2,
title=paste(names(lidar[j]), "\n Shenandoah Trees $ Shrubs & Seedlings \n Correspondence Analysis")) 

s1 <- surf(ca.tss,lidar[,j], scaling=2, col = "steelblue")

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-4, 6, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4)), "Plots","Genus name" ), bty="n",
 pt.cex = c(NA, NA, NA, 2, 2), pch = c(NA, NA, NA, 20, 17), 
 col = c(NA, NA, NA,adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)))
 
 }
 df51[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df52 <- df51[as.numeric(df51[,3])>=0.3,]
df52 <- df52[order(df52[,3], decreasing = TRUE),]

write.csv(df52, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_CA_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df51, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs+seedl_CA_lidar_metrics.csv", row.names = FALSE)

########## combining the 3 runs

mc1 <- unique(c(df48[,1], df50[,1], df52[,1]))
mc1 <- mc1[order(mc1)]

df48a <- df48[match(mc1, df48[,1]),]
df50a <- df50[match(mc1, df50[,1]),]
df52a <- df52[match(mc1, df52[,1]),]

try15 <- cbind(df48a, df50a, df52a)
try15[,1] <- mc1
try15 <- try15[,c(-5, -9)]

names(try15)[c(3, 4, 6,7, 9, 10)] <- c("Tr_CA_R_sq", "Tr_CA_D_sq",
"Ts_CA_R_sq", "Ts_CA_D_sq", "Tss_CA_R_sq", "Tss_CA_D_sq")

try16 <- try15[, c(1, 3, 4, 6,7, 9, 10)]

write.csv(try16, "G:\\CDI2017\\Veg_anal_table\\Shen_allveg_CA_compar_03_lidar_metrics.csv", row.names = FALSE)

#####################################################################################
#
#                   Detrended Correspondence Analysis
#
#########################################################################################

library(vegan)

veg.dca <- decorana(veg, iweigh=TRUE)
veg.dca.plt <- plot(veg.dca, type = "n")
points(veg.dca.plt$species, pch = 17, cex = 2, col = adjustcolor( "forestgreen", alpha.f = 0.6))
points(veg.dca.plt$sites, pch = 20, cex = 1, col = adjustcolor( "brown", alpha.f = 0.6))
title(main = "Shenandoah Trees \n Detrended Correspondence Analysis")

identify(veg.dca.plt,'species', col = "forestgreen")

# all 3 types of veget
#trees
layout(matrix(seq(1,3), 1, 3, byrow=TRUE))
veg.dca <- decorana(veg, iweigh=TRUE)
veg.dca.plt <- plot(veg.dca, type = "n")
points(veg.dca.plt$species, pch = 17, cex = 2.5, col = adjustcolor( "forestgreen", alpha.f = 0.6))
points(veg.dca.plt$sites, pch = 20, cex = 2, col = adjustcolor( "brown", alpha.f = 0.6))
title(main = "Shenandoah Trees \n Detrended Correspondence Analysis")

identify(veg.dca.plt,'species', col = "forestgreen", cex=1)

# trees + shrubs

vegts.dca <- decorana(vegts, iweigh=TRUE)
vegts.dca.plt <- plot(vegts.dca, type = "n")
points(vegts.dca.plt$species, pch = 17, cex = 2.5, col = adjustcolor( "forestgreen", alpha.f = 0.6))
points(vegts.dca.plt$sites, pch = 20, cex = 2, col = adjustcolor( "brown", alpha.f = 0.6))
title(main = "Shenandoah Trees & Shrubs \n Detrended Correspondence Analysis")

identify(vegts.dca.plt,'species', col = "forestgreen", cex=1)

# trees + shrubs + seedlings

vegtss.dca <- decorana(veg2ts, iweigh=TRUE)
vegtss.dca.plt <- plot(vegtss.dca, type = "n")
points(vegtss.dca.plt$species, pch = 17, cex = 2.5, col = adjustcolor( "forestgreen", alpha.f = 0.6))
points(vegtss.dca.plt$sites, pch = 20, cex = 2, col = adjustcolor( "brown", alpha.f = 0.6))
title(main = "Shenandoah Trees & Shrubs & Seedlings \n Detrended Correspondence Analysis")

identify(vegtss.dca.plt,'species', col = "forestgreen", cex=1)

########### trees and lidar metrics DCA ##############################

n <- dim(lidar)[2]
df53 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_DCA_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){

veg.dca.plt <- plot(veg.dca, type = "n")
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

points(veg.dca.plt$species, pch = 17, cex = 2.5, col = adjustcolor( "forestgreen", alpha.f = 0.6))
points(veg.dca.plt$sites, pch = 20, cex = cex1, col = adjustcolor( "brown", alpha.f = 0.6))
title(main = paste(names(lidar)[j], "\n Shenandoah Trees \n Detrended Correspondence Analysis"))

s1 <- ordisurf(veg.dca,lidar[,j], col = "blue", add = T)

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-2.3, -1.5, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4)),"Genus name",
names(lidar)[j], "Min. value", "Max. value" ), bty="n",
 pt.cex = c(NA, NA, NA, 2, NA,  min(cex1, na.rm = TRUE), max(cex1, na.rm = TRUE)), 
 pch = c(NA, NA, NA, 17, NA, 20, 20), 
 col = c(NA, NA, NA,adjustcolor( "forestgreen", alpha.f = 0.6), NA, 
 adjustcolor( "brown", alpha.f = 0.6), adjustcolor( "brown", alpha.f = 0.6)))
 
 df53[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df54 <- df53[as.numeric(df53[,3])>=0.3,]
df54 <- df54[order(df54[,3], decreasing = TRUE),]

write.csv(df54, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_CA_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df53, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_CA_lidar_metrics.csv", row.names = FALSE)

# Trees + Shrubs DCA

n <- dim(lidar)[2]
df55 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_DCA_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){

vegts.dca.plt <- plot(vegts.dca, type = "n")
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

points(vegts.dca.plt$species, pch = 17, cex = 2.5, col = adjustcolor( "forestgreen", alpha.f = 0.6))
points(vegts.dca.plt$sites, pch = 20, cex = cex1, col = adjustcolor( "brown", alpha.f = 0.6))
title(main = paste(names(lidar)[j], "\n Shenandoah Trees & Shrubs \n Detrended Correspondence Analysis"))

s1 <- ordisurf(vegts.dca,lidar[,j], col = "blue", add = T)

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-4, -1.5, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4)),"Genus name",
names(lidar)[j], "Min. value", "Max. value" ), bty="n",
 pt.cex = c(NA, NA, NA, 2, NA,  min(cex1, na.rm = TRUE), max(cex1, na.rm = TRUE)), 
 pch = c(NA, NA, NA, 17, NA, 20, 20), 
 col = c(NA, NA, NA,adjustcolor( "forestgreen", alpha.f = 0.6), NA, 
 adjustcolor( "brown", alpha.f = 0.6), adjustcolor( "brown", alpha.f = 0.6)))
 
 df55[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df56 <- df55[as.numeric(df55[,3])>=0.3,]
df56 <- df56[order(df56[,3], decreasing = TRUE),]

write.csv(df56, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_CA_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df55, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs_CA_lidar_metrics.csv", row.names = FALSE)

# Trees + Snrubd + Seedlings

n <- dim(lidar)[2]
df57 <- data.frame(Lidar_metrics=NA, GAM_Family=NA, R_square=NA, Deviance_Expl=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs+seedl_DCA_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){

vegtss.dca.plt <- plot(vegtss.dca, type = "n")
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

points(vegtss.dca.plt$species, pch = 17, cex = 2.5, col = adjustcolor( "forestgreen", alpha.f = 0.6))
points(vegtss.dca.plt$sites, pch = 20, cex = cex1, col = adjustcolor( "brown", alpha.f = 0.6))
title(main = paste(names(lidar)[j], "\n Shenandoah Trees & Shrubs & Seedlings \n Detrended Correspondence Analysis"))

s1 <- ordisurf(vegtss.dca,lidar[,j], col = "blue", add = T)

famname <- summary(s1)$family[[1]]
famn1 <- paste0(toupper(substr(famname, 1, 1)), substr(famname, 2, nchar(famname)))

 legend(-3, -2, c(paste0("Family: ",famn1), paste0("R^2 = ", abs(round(summary(s1)$r.sq,4))), 
 paste0("Dev.Expl: ", round(summary(s1)$dev.expl,4)),"Genus name",
names(lidar)[j], "Min. value", "Max. value" ), bty="n",
 pt.cex = c(NA, NA, NA, 2, NA,  min(cex1, na.rm = TRUE), max(cex1, na.rm = TRUE)), 
 pch = c(NA, NA, NA, 17, NA, 20, 20), 
 col = c(NA, NA, NA,adjustcolor( "forestgreen", alpha.f = 0.6), NA, 
 adjustcolor( "brown", alpha.f = 0.6), adjustcolor( "brown", alpha.f = 0.6)))
 
 df57[(j-1),] <- c(names(lidar)[j],famn1, round(summary(s1)$r.sq,4), round(summary(s1)$dev.expl,4)) 
 
}
}
dev.off()
}

df58 <- df57[as.numeric(df57[,3])>=0.3,]
df58 <- df58[order(df58[,3], decreasing = TRUE),]

write.csv(df58, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_CA_03_lidar_metrics.csv", row.names = FALSE)
write.csv(df57, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs+seedl_CA_lidar_metrics.csv", row.names = FALSE)

# lots of critiques for DCA algorithms so now we have canonical correspondence analysis CCA

###########################################################
#
#                  Canonical Correspondence Analysis
#
#########################################################

# trees; CA rez. = df48

df48b <- df48[as.numeric(df48[,4])>=0.4,]
nm1 <- df48b[,1]

 ind1 <- c()
 for (i in 1:length(nm1)){
ind1[i]<- which(colnames(lidar)==nm1[i])

}

ind1
#  25 39 17 29 16 40 28

# "Mean_STATS_MAX"  "Mean_HAG_MAX"    "Mean_HCENT_P98"  "Mean_STATS_SDEV" "Mean_HCENT_P90"  
# "Mean_HAG_SDT"    "Mean_STATS_QAV" 

STATS_MAX <- lidar[,ind1[1]]
HAG_MAX <- lidar[,ind1[2]]
HCENT_P98 <- lidar[,ind1[3]]
STATS_SDEV <- lidar[,ind1[4]]
HCENT_P90<- lidar[,ind1[5]]
HAG_SDT<- lidar[,ind1[6]]
STATS_QAV<- lidar[,ind1[7]]

veg.cca <- cca(veg ~ STATS_MAX + HAG_MAX + HCENT_P98 + STATS_SDEV+HCENT_P90+HAG_SDT+STATS_QAV)
veg.cca

# Call: cca(formula = veg ~ Mean_STATS_MAX + Mean_HAG_MAX + Mean_HCENT_P98 + Mean_STATS_SDEV)

              # Inertia Proportion Rank
# Total         5.06684    1.00000     
# Constrained   0.37540    0.07409    4
# Unconstrained 4.69145    0.92591   36
# Inertia is mean squared contingency coefficient 

# Eigenvalues for constrained axes:
   # CCA1    CCA2    CCA3    CCA4 
# 0.26316 0.05585 0.03959 0.01679 

# Eigenvalues for unconstrained axes:
   # CA1    CA2    CA3    CA4    CA5    CA6    CA7    CA8 
# 0.5250 0.4633 0.3884 0.3151 0.2990 0.2824 0.2443 0.2357 
# (Showed only 8 of all 36 unconstrained eigenvalues)

# not very successful
# Only 0.37540/5.06684 or 0.07409 of the total variability was captured in the CCA. 

round(summary(veg.cca)$concont[[1]],4)
                           # CCA1       CCA2       CCA3       CCA4
# Eigenvalue            0.2631624 0.05585147 0.03958991 0.01679377
# Proportion Explained  0.7010200 0.14878000 0.10546000 0.04474000
# Cumulative Proportion 0.7010200 0.84980000 0.95526000 1.00000000

chi <- c(veg.cca$tot.chi, veg.cca$CCA$tot.chi, veg.cca$CA$tot.chi)
props <- chi/chi[1]
tbl <- cbind(chi, props)
rownames(tbl) <- c("Total","Constrained", "Unconstrained")
 colnames(tbl) <- c("Inertia", "Proportion")
 
source("F:\\R_scripts\\plot_cca1_for_labdsv.r")

vegcca.plot <- plot.cca1(veg.cca,choices=c(1,2), pch.sites = 16, col.sites = adjustcolor( "brown", alpha.f = 0.6),
cex.sites = 1, pch.spec = 17, col.spec = adjustcolor( "forestgreen", alpha.f = 0.6), cex.spec = 2,
xlim = c(-5, 15))
title(main = paste("Shenandoah Trees \n Canonical Correspondence Analysis"))

text(3,-3, paste(capture.output(round(tbl,4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

# text(3,-5, paste(capture.output(round(summary(veg.cca)$concont[[1]],4)), collapse='\n'), 
# pos=4,family = "mono",cex = 0.7)

veg.rsq <- RsquareAdj(veg.cca)

legend(10, -5, c(paste("R.sq. =", round(veg.rsq$r.squared,4)), paste("Adj.R.sq. =", round(veg.rsq$adj.r.squared,4)),
"Sites", "Genus"), pch = c(NA, NA, 16, 17), col = c(NA, NA, adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)),
pt.cex=c(NA, NA,1, 2), bty="n") 

# trees+shrubs CCA df50

df50b <- df50[as.numeric(df50[,4])>=0.4,]
nm1 <- df50b[,1]

 ind1 <- c()
 for (i in 1:length(nm1)){
ind1[i]<- which(colnames(lidar)==nm1[i])

}

ind1
# [1]  25 39 17 29 40

# "Mean_STATS_MAX"  "Mean_HAG_MAX"    "Mean_HCENT_P98"  "Mean_STATS_SDEV" "Mean_HAG_SDT" 
STATS_MAX <- lidar[,ind1[1]]
HAG_MAX <- lidar[,ind1[2]]
HCENT_P98 <- lidar[,ind1[3]]
STATS_SDEV <- lidar[,ind1[4]]
HAG_SDT <- lidar[,ind1[5]]

vegts.cca <- cca(vegts ~ STATS_MAX + HAG_MAX + HCENT_P98 + STATS_SDEV+ HAG_SDT)
vegts.cca

chi.ts <- c(vegts.cca$tot.chi, vegts.cca$CCA$tot.chi, vegts.cca$CA$tot.chi)
props.ts <- chi.ts/chi.ts[1]
tbl.ts <- cbind(chi.ts, props.ts)
rownames(tbl.ts) <- c("Total","Constrained", "Unconstrained")
 colnames(tbl.ts) <- c("Inertia", "Proportion")
 
# source("F:\\R_scripts\\plot_cca1_for_labdsv.r")

vegtscca.plot <- plot.cca1(vegts.cca,choices=c(1,2), pch.sites = 16, col.sites = adjustcolor( "brown", alpha.f = 0.6),
cex.sites = 1, pch.spec = 17, col.spec = adjustcolor( "forestgreen", alpha.f = 0.6), cex.spec = 2,
xlim = c(-5, 15))
title(main = paste("Shenandoah Trees & Shrubs \n Canonical Correspondence Analysis"))

text(5,-2, paste(capture.output(round(tbl.ts,4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

text(2.3,-5, paste(capture.output(round(summary(vegts.cca)$concont[[1]],4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

vegts.rsq <- RsquareAdj(vegts.cca)

legend(10, -6, c(paste("R.sq. =", round(vegts.rsq$r.squared,4)), paste("Adj.R.sq. =", round(vegts.rsq$adj.r.squared,4)),
"Sites", "Genus"), pch = c(NA, NA, 16, 17), col = c(NA, NA, adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)),
pt.cex=c(NA, NA,1, 2), bty="n") 

# trees+shrubs+seedlings

df52b <- df52[as.numeric(df52[,4])>=0.4,]
nm1 <- df52b[,1]

 ind1 <- c()
 for (i in 1:length(nm1)){
ind1[i]<- which(colnames(lidar)==nm1[i])

}

ind1
# [1] 25 39 17 29

# "Mean_STATS_MAX"  "Mean_HAG_MAX"    "Mean_HCENT_P98"  "Mean_STATS_SDEV"
STATS_MAX <- lidar[,ind1[1]]
HAG_MAX <- lidar[,ind1[2]]
HCENT_P98 <- lidar[,ind1[3]]
STATS_SDEV <- lidar[,ind1[4]]

vegtss.cca <- cca(veg2ts ~ STATS_MAX + HAG_MAX + HCENT_P98 + STATS_SDEV)
vegtss.cca

chi.tss <- c(vegtss.cca$tot.chi, vegtss.cca$CCA$tot.chi, vegtss.cca$CA$tot.chi)
props.tss <- chi.tss/chi.tss[1]
tbl.tss <- cbind(chi.tss, props.tss)
rownames(tbl.tss) <- c("Total","Constrained", "Unconstrained")
 colnames(tbl.tss) <- c("Inertia", "Proportion")
 
# source("F:\\R_scripts\\plot_cca1_for_labdsv.r")

vegtscca.plot <- plot.cca1(vegtss.cca,choices=c(1,2), pch.sites = 16, col.sites = adjustcolor( "brown", alpha.f = 0.6),
cex.sites = 1, pch.spec = 17, col.spec = adjustcolor( "forestgreen", alpha.f = 0.6), cex.spec = 2,
xlim = c(-5, 15))
title(main = paste("Shenandoah Trees & Shrubs & Seedlings\n Canonical Correspondence Analysis"))

text(5,-1.5, paste(capture.output(round(tbl.tss,4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

text(5,-3.5, paste(capture.output(round(summary(vegtss.cca)$concont[[1]],4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

vegtss.rsq <- RsquareAdj(vegtss.cca)

legend(10, -5, c(paste("R.sq. =", round(vegtss.rsq$r.squared,4)), paste("Adj.R.sq. =", round(vegtss.rsq$adj.r.squared,4)),
"Sites", "Genus"), pch = c(NA, NA, 16, 17), col = c(NA, NA, adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)),
pt.cex=c(NA, NA,1, 2), bty="n") 

# all 3 together - very dismal results

jpeg(paste0("G:\\CDI2017\\Imag\\Shen_CCA_lidar_metrics_all_veg.jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,3), 1, 3, byrow=TRUE))
#trees
vegcca.plot <- plot.cca1(veg.cca,choices=c(1,2), pch.sites = 16, col.sites = adjustcolor( "brown", alpha.f = 0.6),
cex.sites = 1, pch.spec = 17, col.spec = adjustcolor( "forestgreen", alpha.f = 0.6), cex.spec = 2,
xlim = c(-5, 15))
title(main = paste("Shenandoah Trees \n Canonical Correspondence Analysis"))

text(3,-3, paste(capture.output(round(tbl,4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

legend(10, -5, c(paste("R.sq. =", round(veg.rsq$r.squared,4)), paste("Adj.R.sq. =", round(veg.rsq$adj.r.squared,4)),
"Sites", "Genus"), pch = c(NA, NA, 16, 17), col = c(NA, NA, adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)),
pt.cex=c(NA, NA,1, 2), bty="n") 

#trees+shrubs
egtscca.plot <- plot.cca1(vegts.cca,choices=c(1,2), pch.sites = 16, col.sites = adjustcolor( "brown", alpha.f = 0.6),
cex.sites = 1, pch.spec = 17, col.spec = adjustcolor( "forestgreen", alpha.f = 0.6), cex.spec = 2,
xlim = c(-5, 15))
title(main = paste("Shenandoah Trees & Shrubs \n Canonical Correspondence Analysis"))

text(5,-2, paste(capture.output(round(tbl.ts,4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

text(2.3,-5, paste(capture.output(round(summary(vegts.cca)$concont[[1]],4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

legend(10, -6, c(paste("R.sq. =", round(vegts.rsq$r.squared,4)), paste("Adj.R.sq. =", round(vegts.rsq$adj.r.squared,4)),
"Sites", "Genus"), pch = c(NA, NA, 16, 17), col = c(NA, NA, adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)),
pt.cex=c(NA, NA,1, 2), bty="n") 

# trees+shrubs+seedlings

vegtscca.plot <- plot.cca1(vegtss.cca,choices=c(1,2), pch.sites = 16, col.sites = adjustcolor( "brown", alpha.f = 0.6),
cex.sites = 1, pch.spec = 17, col.spec = adjustcolor( "forestgreen", alpha.f = 0.6), cex.spec = 2,
xlim = c(-5, 15))
title(main = paste("Shenandoah Trees & Shrubs & Seedlings\n Canonical Correspondence Analysis"))

text(5,-1.5, paste(capture.output(round(tbl.tss,4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

text(5,-3.5, paste(capture.output(round(summary(vegtss.cca)$concont[[1]],4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

legend(10, -5, c(paste("R.sq. =", round(vegtss.rsq$r.squared,4)), paste("Adj.R.sq. =", round(vegtss.rsq$adj.r.squared,4)),
"Sites", "Genus"), pch = c(NA, NA, 16, 17), col = c(NA, NA, adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)),
pt.cex=c(NA, NA,1, 2), bty="n") 

dev.off ()

# adding a categorical variable to the analysis - in this case the geology+aspect+height category
# i will do it only for trees

if (interactive())     tabg<-choose.files(filters = exts)
geol <- read.csv(tabg)
ELU_ID <- geol[,2] # the table is ordered by SiteID

veg1.cca <- cca(veg ~ STATS_MAX + HAG_MAX + HCENT_P98 + STATS_SDEV+HCENT_P90+HAG_SDT+STATS_QAV+ELU_ID)
veg1.cca

chi1 <- c(veg1.cca$tot.chi, veg1.cca$CCA$tot.chi, veg1.cca$CA$tot.chi)
props1 <- chi1/chi1[1]
tbl1 <- cbind(chi1, props1)
rownames(tbl1) <- c("Total","Constrained", "Unconstrained")
 colnames(tbl1) <- c("Inertia", "Proportion")
 
source("F:\\R_scripts\\plot_cca1_for_labdsv.r")

vegcca.plot <- plot.cca1(veg1.cca,choices=c(1,2), pch.sites = 16, col.sites = adjustcolor( "brown", alpha.f = 0.6),
cex.sites = 1, pch.spec = 17, col.spec = adjustcolor( "forestgreen", alpha.f = 0.6), cex.spec = 2,
xlim = c(-10, 5))
title(main = paste("Shenandoah Trees \n Ecological Land Unit \n Canonical Correspondence Analysis"))

text(-10,-1, paste(capture.output(round(tbl1,4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

# text(3,-5, paste(capture.output(round(summary(veg.cca)$concont[[1]],4)), collapse='\n'), 
# pos=4,family = "mono",cex = 0.7)

veg1.rsq <- RsquareAdj(veg1.cca)

legend(-10, -2, c(paste("R.sq. =", round(veg1.rsq$r.squared,4)), paste("Adj.R.sq. =", round(veg1.rsq$adj.r.squared,4)),
"Sites", "Genus", "Factor Constraints Centroids" ), pch = c(NA, NA, 16, 17,4), col = c(NA, NA, adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6), "blue"),
pt.cex=c(NA, NA,1, 2, 1), bty="n") 

centr <- summary(veg1.cca)$centroids

rownames(centr) <- str_sub(rownames(centr), 7, 14)

identify(centr[,1], centr[,2], labels = rownames(centr), col = "blue")

# trees without and with factor constraints

layout(matrix(seq(1,2), 1, 2, byrow=TRUE))

vegcca.plot <- plot.cca1(veg.cca,choices=c(1,2), pch.sites = 16, col.sites = adjustcolor( "brown", alpha.f = 0.6),
cex.sites = 1, pch.spec = 17, col.spec = adjustcolor( "forestgreen", alpha.f = 0.6), cex.spec = 2,
xlim = c(-5, 15))
title(main = paste("Shenandoah Trees \n Canonical Correspondence Analysis"))

text(3,-3, paste(capture.output(round(tbl,4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

# text(3,-5, paste(capture.output(round(summary(veg.cca)$concont[[1]],4)), collapse='\n'), 
# pos=4,family = "mono",cex = 0.7)

#veg.rsq <- RsquareAdj(veg.cca)

legend(8, -5, c(paste("R.sq. =", round(veg.rsq$r.squared,4)), paste("Adj.R.sq. =", round(veg.rsq$adj.r.squared,4)),
"Sites", "Genus"), pch = c(NA, NA, 16, 17), col = c(NA, NA, adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6)),
pt.cex=c(NA, NA,1, 2), bty="n") 

vegcca.plot <- plot.cca1(veg1.cca,choices=c(1,2), pch.sites = 16, col.sites = adjustcolor( "brown", alpha.f = 0.6),
cex.sites = 1, pch.spec = 17, col.spec = adjustcolor( "forestgreen", alpha.f = 0.6), cex.spec = 2,
xlim = c(-10, 5))
title(main = paste("Shenandoah Trees \n Ecological Land Unit \n Canonical Correspondence Analysis"))

text(-10,-1, paste(capture.output(round(tbl1,4)), collapse='\n'), 
pos=4,family = "mono",cex = 0.7)

# text(3,-5, paste(capture.output(round(summary(veg.cca)$concont[[1]],4)), collapse='\n'), 
# pos=4,family = "mono",cex = 0.7)

#veg1.rsq <- RsquareAdj(veg1.cca)

legend(-10, -2, c(paste("R.sq. =", round(veg1.rsq$r.squared,4)), paste("Adj.R.sq. =", round(veg1.rsq$adj.r.squared,4)),
"Sites", "Genus", "Factor Constraints Centroids" ), pch = c(NA, NA, 16, 17,4), col = c(NA, NA, adjustcolor( "brown", alpha.f = 0.6),adjustcolor( "forestgreen", alpha.f = 0.6), "blue"),
pt.cex=c(NA, NA,1, 2, 1), bty="n") 

identify(centr[,1], centr[,2], labels = rownames(centr), col = "blue")
##################################################################################
#
#               Fuzzy Set Ordination
#
##################################################################################

# FSO is the first technique that directly incorporates the environmental data into the calculation of the 
# configuration of the points in the ordination.
# Each FSO is an estimate of the environmental characteristics we would expect for a site based on 
# its vegetation composition.

library(fso)

# trees FSO

disbc.veg <- dsvdis(veg,"bray/curtis")

lidj.fso <- fso(lidar[,j],disbc.veg, permute=1000)

plot(lidj.fso,title=paste(names(lidar)[j],"Shenandoah Trees \n Fuzzy Set Ordination"),
xlab=names(lidar)[j],ylab=paste("Apparent", names(lidar)[j]))

#getAnywhere(plot.fso)

source("F:\\R_scripts\\plot_fso1_labdsv.r")

plot.fso1(lidj.fso,title=paste(names(lidar)[j],"Shenandoah Trees \n Fuzzy Set Ordination"),
xlab=names(lidar)[j],ylab=paste("Apparent", names(lidar)[j]), cex.r=1.5, col.r="red")

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[j], ylab = paste("Apparent", names(lidar)[j]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

# let's do it for all the metrics and see which has a higher r correlation

n <- dim(lidar)[2]
df59 <- data.frame(Lidar_metrics=NA, class_r=NA, rob_r = NA, p_val=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_FSO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
if (length(which(is.na(lidar[,j]))) !=0) {
a1 <- which(is.na(lidar[,j]))

veg.site <- apply(veg[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg.tr1 <- veg[c(-a1),][, which(veg.site > 0)]
disbc.veg1 <- dsvdis(veg.tr1,"bray/curtis")
lidar1 <- lidar[-a1, j]
lidj.fso <- fso(lidar1,disbc.veg1, permute=1000)
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[j], ylab = paste("Apparent", names(lidar)[j]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[j],"\n Shenandoah Trees \n Fuzzy Set Ordination"))

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[j], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 } else {
disbc.veg <- dsvdis(veg,"bray/curtis")
lidj.fso <- fso(lidar[,j],disbc.veg, permute=1000)
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[j], ylab = paste("Apparent", names(lidar)[j]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[j],"\n Shenandoah Trees \n Fuzzy Set Ordination"))

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[j], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 }
 df59[(j-1),] <- c(names(lidar)[j],round(cor.vegfso$cor.cla,4), round(cor.vegfso$cor.rob,4),round(lidj.fso$p,4)) 
 }
}
dev.off()
}

df60 <- df59[as.numeric(df59[,3])>=0.5,]
df60 <- df60[order(df60[,3], decreasing = TRUE),]

write.csv(df60, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_FSO_05_lidar_metrics.csv", row.names = FALSE)
write.csv(df59, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees_FSO_lidar_metrics.csv", row.names = FALSE)

# making the plots only for lidar metrics that have rob.corr >= 0.55

# trees

df60a <- df60[as.numeric(df60[,3])>=0.55,] 

n <- dim(df60a)[1]

nm.tr <- df60a[,1]

 ind1 <- c()
 for (i in 1:n){
ind1[i]<- which(colnames(lidar)==nm.tr[i])

}

jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees_FSO_lidar_metrics_robcorr_055.jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in 1:n){

if (length(which(is.na(lidar[,ind1[j]]))) !=0) {
a1 <- which(is.na(lidar[,ind1[j]]))

veg.site <- apply(veg[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg.tr1 <- veg[c(-a1),][, which(veg.site > 0)]
disbc.veg1 <- dsvdis(veg.tr1,"bray/curtis")
lidar1 <- lidar[-a1, ind1[j]]
lidj.fso <- fso(lidar1,disbc.veg1, permute=1000)
cex1 =  c(1+(2*(lidar[,ind1[j]]-min(lidar[,ind1[j]], na.rm = TRUE))/(max(lidar[,ind1[j]], na.rm = TRUE)-min(lidar[,ind1[j]], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[ind1[j]], ylab = paste("Apparent", names(lidar)[ind1[j]]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[ind1[j]],"- Shenandoah Trees \n Fuzzy Set Ordination"), cex.main=1.5)

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[ind1[j]], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 } else {
disbc.veg <- dsvdis(veg,"bray/curtis")
lidj.fso <- fso(lidar[,ind1[j]],disbc.veg, permute=1000)
cex1 =  c(1+(2*(lidar[,ind1[j]]-min(lidar[,ind1[j]], na.rm = TRUE))/(max(lidar[,ind1[j]], na.rm = TRUE)-min(lidar[,ind1[j]], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[ind1[j]], ylab = paste("Apparent", names(lidar)[ind1[j]]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[ind1[j]],"- Shenandoah Trees \n Fuzzy Set Ordination"), cex.main=1.5)

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[ind1[j]], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 }
 
}

dev.off()

###################################
# trees + shrubs FSO
####################################

n <- dim(lidar)[2]
df61 <- data.frame(Lidar_metrics=NA, class_r=NA, rob_r = NA, p_val=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_FSO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
if (length(which(is.na(lidar[,j]))) !=0) {
a1 <- which(is.na(lidar[,j]))

vegts.site <- apply(vegts[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg.ts1 <- vegts[c(-a1),][, which(vegts.site > 0)]
disbc.vegts1 <- dsvdis(veg.ts1,"bray/curtis")
lidar1 <- lidar[-a1, j]
lidj.fso <- fso(lidar1,disbc.vegts1, permute=1000)
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[j], ylab = paste("Apparent", names(lidar)[j]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[j],"\n Shenandoah Trees & Shrubs \n Fuzzy Set Ordination"))

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[j], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 } else {
disbc.vegts <- dsvdis(vegts,"bray/curtis")
lidj.fso <- fso(lidar[,j],disbc.vegts, permute=1000)
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[j], ylab = paste("Apparent", names(lidar)[j]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[j],"\n Shenandoah Trees & Shrubs\n Fuzzy Set Ordination"))

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[j], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 }
 df61[(j-1),] <- c(names(lidar)[j],round(cor.vegfso$cor.cla,4), round(cor.vegfso$cor.rob,4),round(lidj.fso$p,4)) 
 }
}
dev.off()
}

df62 <- df61[as.numeric(df61[,3])>=0.5,]
df62 <- df62[order(df62[,3], decreasing = TRUE),]

write.csv(df62, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_FSO_05_lidar_metrics.csv", row.names = FALSE)
write.csv(df61, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs_FSO_lidar_metrics.csv", row.names = FALSE)

# making the plots only for lidar metrics that have rob.corr >= 0.575

# trees + shrubs

df62a <- df62[as.numeric(df62[,3])>=0.575,] 

n <- dim(df62a)[1]

nm.tr <- df62a[,1]

 ind1 <- c()
 for (i in 1:n){
ind1[i]<- which(colnames(lidar)==nm.tr[i])

}

jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs_FSO_lidar_metrics_robcorr_055.jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in 1:n){

if (length(which(is.na(lidar[,ind1[j]]))) !=0) {
a1 <- which(is.na(lidar[,ind1[j]]))

vegts.site <- apply(vegts[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg.ts1 <- vegts[c(-a1),][, which(vegts.site > 0)]
disbc.vegts1 <- dsvdis(veg.ts1,"bray/curtis")
lidar1 <- lidar[-a1, ind1[j]]
lidj.fso <- fso(lidar1,disbc.vegts1, permute=1000)
cex1 =  c(1+(2*(lidar[,ind1[j]]-min(lidar[,ind1[j]], na.rm = TRUE))/(max(lidar[,ind1[j]], na.rm = TRUE)-min(lidar[,ind1[j]], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[ind1[j]], ylab = paste("Apparent", names(lidar)[ind1[j]]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[ind1[j]],"- Shenandoah Trees & Shrubs \n Fuzzy Set Ordination"), cex.main=1.5)

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[ind1[j]], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 } else {
disbc.vegts <- dsvdis(vegts,"bray/curtis")
lidj.fso <- fso(lidar[,ind1[j]],disbc.vegts, permute=1000)
cex1 =  c(1+(2*(lidar[,ind1[j]]-min(lidar[,ind1[j]], na.rm = TRUE))/(max(lidar[,ind1[j]], na.rm = TRUE)-min(lidar[,ind1[j]], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[ind1[j]], ylab = paste("Apparent", names(lidar)[ind1[j]]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[ind1[j]],"- Shenandoah Trees & Shrubs \n Fuzzy Set Ordination"), cex.main=1.5)

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[ind1[j]], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 }
 
}

dev.off()

##################################################
# trees + shrubs + seedlings FSO
##################################################

n <- dim(lidar)[2]
df63 <- data.frame(Lidar_metrics=NA, class_r=NA, rob_r = NA, p_val=NA)

for (i in seq(2, n, 8)){ 
jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs+seedl_FSO_lidar_metrics_",i,".jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in i:(i+7)){
if (j <= n){
if (length(which(is.na(lidar[,j]))) !=0) {
a1 <- which(is.na(lidar[,j]))

vegtss.site <- apply(veg2ts[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg.tss1 <- veg2ts[c(-a1),][, which(vegtss.site > 0)]
disbc.vegtss1 <- dsvdis(veg.tss1,"bray/curtis")
lidar1 <- lidar[-a1, j]
lidj.fso <- fso(lidar1,disbc.vegtss1, permute=1000)
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[j], ylab = paste("Apparent", names(lidar)[j]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[j],"\n Shenandoah Trees & Shrubs & Seedlings \n Fuzzy Set Ordination"))

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[j], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 } else {
disbc.vegtss <- dsvdis(veg2ts,"bray/curtis")
lidj.fso <- fso(lidar[,j],disbc.vegtss, permute=1000)
cex1 =  c(1+(2*(lidar[,j]-min(lidar[,j], na.rm = TRUE))/(max(lidar[,j], na.rm = TRUE)-min(lidar[,j], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[j], ylab = paste("Apparent", names(lidar)[j]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[j],"\n Shenandoah Trees & Shrubs & Seedlings \n Fuzzy Set Ordination"))

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[j], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 }
 df63[(j-1),] <- c(names(lidar)[j],round(cor.vegfso$cor.cla,4), round(cor.vegfso$cor.rob,4),round(lidj.fso$p,4)) 
 }
}
dev.off()
}

df64 <- df63[as.numeric(df63[,3])>=0.5,]
df64 <- df64[order(df64[,3], decreasing = TRUE),]

write.csv(df64, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_FSO_05_lidar_metrics.csv", row.names = FALSE)
write.csv(df63, "G:\\CDI2017\\Veg_anal_table\\Shen_all_trees+shrubs+seedl_FSO_lidar_metrics.csv", row.names = FALSE)

# making the plots only for lidar metrics that have rob.corr >= 0.583

# trees + shrubs

df64a <- df64[df64[,3]>=0.583,] 

n <- dim(df64a)[1]

nm.tr <- df64a[,1]

 ind1 <- c()
 for (i in 1:n){
ind1[i]<- which(colnames(lidar)==nm.tr[i])

}

jpeg(paste0("G:\\CDI2017\\Imag\\Shen_trees+shrubs+seedl_FSO_lidar_metrics_robcorr_0583.jpg"), width=20, 
height=10,units="in", quality = 100, res=200)

layout(matrix(seq(1,8), 2, 4, byrow=TRUE))

for (j in 1:n){

if (length(which(is.na(lidar[,ind1[j]]))) !=0) {
a1 <- which(is.na(lidar[,ind1[j]]))

vegtss.site <- apply(veg2ts[c(-a1),], 2, function(x) y=length(x[x!=0]))

veg.tss1 <- veg2ts[c(-a1),][, which(vegtss.site > 0)]
disbc.vegtss1 <- dsvdis(veg.tss1,"bray/curtis")
lidar1 <- lidar[-a1, ind1[j]]
lidj.fso <- fso(lidar1,disbc.vegtss1, permute=1000)
cex1 =  c(1+(2*(lidar[,ind1[j]]-min(lidar[,ind1[j]], na.rm = TRUE))/(max(lidar[,ind1[j]], na.rm = TRUE)-min(lidar[,ind1[j]], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[ind1[j]], ylab = paste("Apparent", names(lidar)[ind1[j]]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[ind1[j]],"\n Shenandoah Trees & Shrubs & Seedlings \n Fuzzy Set Ordination"), cex.main=1.5)

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[ind1[j]], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 } else {
disbc.vegtss <- dsvdis(veg2ts,"bray/curtis")
lidj.fso <- fso(lidar[,ind1[j]],disbc.vegtss, permute=1000)
cex1 =  c(1+(2*(lidar[,ind1[j]]-min(lidar[,ind1[j]], na.rm = TRUE))/(max(lidar[,ind1[j]], na.rm = TRUE)-min(lidar[,ind1[j]], na.rm = TRUE))))

cor.vegfso <- cor.plot(lidj.fso$data,lidj.fso$mu, quan = 0.5, alpha = 0.025, 
xlab = names(lidar)[ind1[j]], ylab = paste("Apparent", names(lidar)[ind1[j]]), 
xlim1=FALSE, ylim1=FALSE, title.r=FALSE,
tcol1="blue", tcol2="red", ecol1="blue", ecol2="red", elty1=2, elty2=1, 
elwd1=2, elwd2=2, cex.axis=1.5, cex.lab=1.5, cex.main=3, cex = cex1, pch = 16,
col =  adjustcolor( "steelblue", alpha.f = 0.6))

title(main=paste(names(lidar)[ind1[j]],"\n Shenandoah Trees & Shrubs & Seedlings \n Fuzzy Set Ordination"), cex.main=1.5)

xl <- round(cor.vegfso$xlim[2]*2/3,2)
yl <- round( cor.vegfso$ylim[1]+(cor.vegfso$ylim[2]-cor.vegfso$ylim[1])/3, 2)

 legend(xl,yl, c(paste0("Cla.Cor = ",round(cor.vegfso$cor.cla,3)), 
 paste0("Rob.Cor = ",round(cor.vegfso$cor.rob,3)),
 paste0("p-value = ", round(lidj.fso$p,3)),
 names(lidar)[ind1[j]], "Min. val.", "Max. val"), 
 bty="n", pt.cex = c(NA, NA, NA, NA, min(cex1), max(cex1)), pch = c(NA, NA, NA, NA,16, 16), 
 col = c(NA, NA, NA, NA, adjustcolor( "steelblue", alpha.f = 0.6),
 adjustcolor( "steelblue", alpha.f = 0.6)),
 text.col = c("blue", "red", "black", "black", "black", "black"))
 
 }
 
}

dev.off()

########## combining the 3 runs for SFO

mc1 <- unique(c(df60[,1], df62[,1], df64[,1]))
mc1 <- mc1[order(mc1)]

df60a <- df60[match(mc1, df60[,1]),]
df62a <- df62[match(mc1, df62[,1]),]
df64a <- df64[match(mc1, df64[,1]),]

try17 <- cbind(df60a, df62a, df64a)
try17[,1] <- mc1
try17 <- try17[,c(-5, -9)]

names(try17)[seq(2, 10)] <- c("Tr_Class_Corr", "Tr_Rob_Corr","Tr_p_val",
"Ts_Class_Corr", "Ts_Rob_Corr","Ts_p_val", "Tss_Class_Corr", "Tss_Rob_Corr", "Tss_p_val")

write.csv(try17, "G:\\CDI2017\\Veg_anal_table\\Shen_allveg_FSO_compar_05_lidar_metrics.csv", row.names = FALSE)

####################################################################################
#        A Priori hypotheses and variable screening with muiltivariate FSO
#####################################################################################

# FSO has proven very useful for rapid screening of potential variables and step-wise model fits.
# Roberts 1986

# trees

disbc.veg <- dsvdis(veg,"bray/curtis")
# does not work if i have NA in data ..... so let's eliminate any metric with NA and see how
# many i remain with .....

a2 <- c()

n <- dim(lidar)[2]
for (i in 2:n){
a2[i] <- length(which(is.na(lidar[,i])))

}

a3 <- which(a2==0) # 32 metrics out of 40 remained

veg.mfso <- step.mfso(disbc.veg,start=NULL,lidar[,a3], numitr=1000) # lazy function - it is slow

vegrez.mfso <- data.frame(lidar_metrics = veg.mfso$variable, delta_corr = veg.mfso$delta_cor, pval = veg.mfso$p_val)
vegrez.mfso <- vegrez.mfso[order(vegrez.mfso[,2], decreasing = TRUE),]
vegrez05.mfso <- vegrez.mfso[vegrez.mfso[,3] <= 0.05,]

write.csv(vegrez.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_MFSO_lidar_metrics.csv", row.names = FALSE)
write.csv(vegrez05.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_MFSO_pval_005_lidar_metrics.csv", row.names = FALSE)

# The results suggest that  Mean_STATS_MAX gives the best result, resulting in a correlation 
# between the differences in fuzzy elevation and the Bray-Curtis dissimilarity of 0.6043. 
# It is only moderately statistically significant after 1000 permutation (p=0.018), but it is often 
# the case that a single variable is insufficient to structure a high-dimensional dissimilarity 
# matrix. To go to the next step, move Mean_STATS_MAX from the add data.frame to the start data.frame 
# and rerun the function. In this case the "baseline" is Mean_STATS_MAX = 0.6043236 and not 0 as before.

a4 <- which(names(lidar)=="Mean_STATS_MAX")
a5 <- a3[-which(a3==a4)]

veg1.mfso<-step.mfso(disbc.veg,start=data.frame(lidar[,a4]),lidar[,a5], numitr=1000)

vegrez1.mfso <- data.frame(lidar_metrics = veg1.mfso$variable, delta_corr = veg1.mfso$delta_cor, pval = veg1.mfso$p_val)
vegrez1.mfso <- vegrez1.mfso[order(vegrez1.mfso[,2], decreasing = TRUE),]
veg1rez05.mfso <- vegrez1.mfso[vegrez1.mfso[,3] <= 0.05,]

veg1rez05.mfso

   # lidar_metrics delta_corr  pval
# 28  Mean_HAG_FRS 0.02963115 0.024
# 27   Mean_HAG_BE 0.02942677 0.016
# 25 Mean_VEG_DCAN 0.01443895 0.050

write.csv(vegrez1.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_Mean_STATS_MAX_MFSO_lidar_metrics.csv", row.names = FALSE)
write.csv(veg1rez05.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees__Mean_STATS_MAX_MFSO_pval_005_lidar_metrics.csv", row.names = FALSE)

# After adding Mean_STATS_MAX to the model the baseline went to  0.6043236 from 0 previously, and now 
# variables are considered in addition to that value. Mean_HAG_FRS, Mean_HAG_BE and Mean_VEG_DCAN 
# show the strongest effects (0.02963115, 0.02942677, 0.01443895, respectively) that are statistically 
# significant (p.val <= 0.05) after 1000 permutations.

# what happens if i move  Mean_HAG_FRS in start data too in addition to Mean_STATS_MAX ?

a6 <- which(names(lidar)=="Mean_HAG_FRS")
a7 <- a5[-which(a5==a6)]

veg2.mfso<-step.mfso(disbc.veg,start=data.frame(lidar[,c(a4, a6)]),lidar[,a7], numitr=1000)

vegrez2.mfso <- data.frame(lidar_metrics = veg2.mfso$variable, delta_corr = veg2.mfso$delta_cor, pval = veg2.mfso$p_val)
vegrez2.mfso <- vegrez2.mfso[order(vegrez2.mfso[,2], decreasing = TRUE),]

# this time the baseline is: 0.6339547 and no other lidar metrics contributes statistically significant
# to improving the goodness-of-fit.

write.csv(vegrez2.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees_Mean_STATS_MAX_Mean_HAG_FRS_MFSO_lidar_metrics.csv", row.names = FALSE)

###################################################
# trees + shrubs MFSO
####################################################

disbc.vegts <- dsvdis(vegts,"bray/curtis")
# does not work if i have NA in data ..... so let's eliminate any metric with NA and see how
# many i remain with .....

a2 <- c()

n <- dim(lidar)[2]
for (i in 2:n){
a2[i] <- length(which(is.na(lidar[,i])))

}

a3 <- which(a2==0) # 32 metrics out of 40 remained

vegts.mfso <- step.mfso(disbc.vegts,start=NULL,lidar[,a3], numitr=1000) # lazy function - it is slow

vegtsrez.mfso <- data.frame(lidar_metrics = vegts.mfso$variable, delta_corr = vegts.mfso$delta_cor, pval = vegts.mfso$p_val)
vegtsrez.mfso <- vegtsrez.mfso[order(vegtsrez.mfso[,2], decreasing = TRUE),]
vegtsrez05.mfso <- vegtsrez.mfso[vegtsrez.mfso[,3] <= 0.05,]

     # lidar_metrics delta_corr  pval
# 18  Mean_STATS_MAX  0.6276230 0.042
# 30    Mean_HAG_MAX  0.6256132 0.038
# 21 Mean_STATS_SDEV  0.6234421 0.033
# 13  Mean_HCENT_P98  0.6215597 0.038
# 12  Mean_HCENT_P90  0.6101448 0.047

write.csv(vegtsrez.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_MFSO_lidar_metrics.csv", row.names = FALSE)
write.csv(vegtsrez05.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_MFSO_pval_005_lidar_metrics.csv", row.names = FALSE)

# adding Mean_STATS_MAX to the start point

a4 <- which(names(lidar)=="Mean_STATS_MAX")
a5 <- a3[-which(a3==a4)]

vegts1.mfso<-step.mfso(disbc.vegts,start=data.frame(lidar[,a4]),lidar[,a5], numitr=1000)

vegtsrez1.mfso <- data.frame(lidar_metrics = vegts1.mfso$variable, delta_corr = vegts1.mfso$delta_cor, pval = vegts1.mfso$p_val)
vegtsrez1.mfso <- vegtsrez1.mfso[order(vegtsrez1.mfso[,2], decreasing = TRUE),]
vegts1rez05.mfso <- vegtsrez1.mfso[vegtsrez1.mfso[,3] <= 0.05,]

vegts1rez05.mfso

   # lidar_metrics delta_corr  pval
# 28  Mean_HAG_FRS 0.03793734 0.003
# 27   Mean_HAG_BE 0.03766169 0.005

write.csv(vegtsrez1.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_Mean_STATS_MAX_MFSO_lidar_metrics.csv", row.names = FALSE)
write.csv(vegts1rez05.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_Mean_STATS_MAX_MFSO_pval_005_lidar_metrics.csv", row.names = FALSE)

# what happens if i move  Mean_HAG_FRS in start data too in addition to Mean_STATS_MAX ?

a6 <- which(names(lidar)=="Mean_HAG_FRS")
a7 <- a5[-which(a5==a6)]

vegts2.mfso<-step.mfso(disbc.vegts,start=data.frame(lidar[,c(a4, a6)]),lidar[,a7], numitr=1000)

vegtsrez2.mfso <- data.frame(lidar_metrics = vegts2.mfso$variable, delta_corr = vegts2.mfso$delta_cor, pval = vegts2.mfso$p_val)
vegtsrez2.mfso <- vegtsrez2.mfso[order(vegtsrez2.mfso[,2], decreasing = TRUE),]

# this time the baseline is: 0.6339547 and no other lidar metrics contributes statistically significant
# to improving the goodness-of-fit.

write.csv(vegtsrez2.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_Mean_STATS_MAX_Mean_HAG_FRS_MFSO_lidar_metrics.csv", row.names = FALSE)

#####################################################
# trees+shrubs+seedlings
#####################################################

disbc.vegtss <- dsvdis(veg2ts,"bray/curtis")
# does not work if i have NA in data ..... so let's eliminate any metric with NA and see how
# many i remain with .....

a2 <- c()

n <- dim(lidar)[2]
for (i in 2:n){
a2[i] <- length(which(is.na(lidar[,i])))

}

a3 <- which(a2==0) # 32 metrics out of 40 remained

vegtss.mfso <- step.mfso(disbc.vegtss,start=NULL,lidar[,a3], numitr=1000) # lazy function - it is slow

vegtssrez.mfso <- data.frame(lidar_metrics = vegtss.mfso$variable, delta_corr = vegtss.mfso$delta_cor, pval = vegtss.mfso$p_val)
vegtssrez.mfso <- vegtssrez.mfso[order(vegtssrez.mfso[,2], decreasing = TRUE),]
vegtssrez05.mfso <- vegtssrez.mfso[vegtssrez.mfso[,3] <= 0.05,]

     # lidar_metrics delta_corr  pval
# 18  Mean_STATS_MAX  0.7177506 0.038
# 13  Mean_HCENT_P98  0.7133726 0.041
# 21 Mean_STATS_SDEV  0.7131500 0.040
# 31    Mean_HAG_SDT  0.7090028 0.047

write.csv(vegtssrez.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_MFSO_lidar_metrics.csv", row.names = FALSE)
write.csv(vegtssrez05.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs+seedl_MFSO_pval_005_lidar_metrics.csv", row.names = FALSE)

# adding Mean_STATS_MAX to the start point

a4 <- which(names(lidar)=="Mean_STATS_MAX")
a5 <- a3[-which(a3==a4)]

vegtss1.mfso<-step.mfso(disbc.vegtss,start=data.frame(lidar[,a4]),lidar[,a5], numitr=1000)

vegtssrez1.mfso <- data.frame(lidar_metrics = vegtss1.mfso$variable, delta_corr = vegtss1.mfso$delta_cor, pval = vegtss1.mfso$p_val)
vegtssrez1.mfso <- vegtssrez1.mfso[order(vegtssrez1.mfso[,2], decreasing = TRUE),]
vegtss1rez05.mfso <- vegtssrez1.mfso[vegtssrez1.mfso[,3] <= 0.05,]

vegtss1rez05.mfso

   # lidar_metrics delta_corr  pval
# 28  Mean_HAG_FRS 0.04013025 0.001
# 27   Mean_HAG_BE 0.03989080 0.001
# 25 Mean_VEG_DCAN 0.01254176 0.026
# baseline = 0.7177506 

write.csv(vegtssrez1.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_Mean_STATS_MAX_MFSO_lidar_metrics.csv", row.names = FALSE)
write.csv(vegtss1rez05.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_Mean_STATS_MAX_MFSO_pval_005_lidar_metrics.csv", row.names = FALSE)

# what happens if i move  Mean_HAG_FRS in start data too in addition to Mean_STATS_MAX ?

a6 <- which(names(lidar)=="Mean_HAG_FRS")
a7 <- a5[-which(a5==a6)]

vegtss2.mfso<-step.mfso(disbc.vegtss,start=data.frame(lidar[,c(a4, a6)]),lidar[,a7], numitr=1000)

vegtssrez2.mfso <- data.frame(lidar_metrics = vegtss2.mfso$variable, delta_corr = vegtss2.mfso$delta_cor, pval = vegtss2.mfso$p_val)
vegtssrez2.mfso <- vegtssrez2.mfso[order(vegtssrez2.mfso[,2], decreasing = TRUE),]

# this time the baseline is:  0.7578808 and no other lidar metrics contributes statistically significant
# to improving the goodness-of-fit.

write.csv(vegtssrez2.mfso, "G:\\CDI2017\\Veg_anal_table\\Shen_trees+shrubs_Mean_STATS_MAX_Mean_HAG_FRS_MFSO_lidar_metrics.csv", row.names = FALSE)

save.image("G:\\CDI2017\\Scripts\\R\\SHEN_veg_mosaic_anal.RData")


