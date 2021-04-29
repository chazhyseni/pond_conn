if (!require("ade4")) install.packages("ade4")
if (!require("betapart")) install.packages("betapart")
if (!require("ecodist")) install.packages("ecodist")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("Hotelling")) install.packages("Hotelling")
if (!require("maps")) install.packages("maps")
if (!require("PopGenReport")) install.packages("PopGenReport")
if (!require("prettymapr")) install.packages("prettymapr")
if (!require("qgraph")) install.packages("qgraph")
if (!require("raster")) install.packages("raster")
if (!require("rasterVis")) install.packages("rasterVis")
if (!require("rgdal")) install.packages("rgdal")
if (!require("rstatix")) install.packages("rstatix")
if (!require("tibble")) install.packages("tibble")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("usedist")) install.packages("usedist")
if (!require("vegan")) install.packages("vegan")
if (!require("vegan3d")) install.packages("vegan3d")


set.seed(27319562)

#setwd("~/Data")
data <- "https://raw.githubusercontent.com/chazhyseni/pond_conn/master/Data/"
output <- "https://raw.githubusercontent.com/chazhyseni/pond_conn/master/Output/"

####coordinates#### 
xy <- read.csv(paste0(data,"Coordinates.csv"),header=T)
names(xy) <- c('x','y')
coordinates(xy) <- ~ x + y
proj4string(xy) <- "+init=epsg:4326"
coords <- xy

####envtl.data#### 
env <- read.csv(paste0(data,"EnvData.csv"),header=T)

####abundance#### 
abund.spp <- read.csv(paste0(data,"SpeciesData.csv"),header=T)

####richness#### 
rich.spp <- abund.spp
rich.spp[rich.spp < 1] = 0
rich.spp[rich.spp > 0] = 1
rich.rmv.cols <- which(colSums(rich.spp) == 0)
if(length(rich.rmv.cols) != 0) rich.spp <- rich.spp[,-rich.rmv.cols]


par(mfrow=c(2,2),fg="gray70",mar=c(4,4,4,1),cex.lab=1.2,cex.axis=1.1,pty='m',bty='o')

bray1.wisc <- vegdist(wisconsin(abund.spp),method="bray")
bray2.wisc <- sqrt(vegdist(wisconsin(abund.spp),method="bray"))
bray3.wisc <- vegdist(wisconsin(abund.spp),method="bray")
bray3.wisc <- (bray3.wisc-min(bray3.wisc))/(max(bray3.wisc)-min(bray3.wisc))
bray4.wisc <- log(vegdist(wisconsin(abund.spp),method="bray")+1)
 
plot(hclust(bray1.wisc),labels=F,ann=F,main="Bray-Curtis: wisconsin")
plot(hclust(bray2.wisc),labels=F,ann=F,main="Bray-Curtis: sqrt(wisconsin)")
plot(hclust(bray3.wisc),labels=F,ann=F,main="Bray-Curtis: minmax(wisconsin)")
plot(hclust(bray4.wisc),labels=F,ann=F,main="Bray-Curtis: log(wisconsin+1)")
hist(bray1.wisc,xlab="",main="Bray-Curtis: wisconsin")
hist(bray2.wisc,xlab="",main="Bray-Curtis: sqrt(wisconsin)")
hist(bray3.wisc,xlab="",main="Bray-Curtis: minmax(wisconsin)")
hist(bray4.wisc,xlab="",main="Bray-Curtis: log(wisconsin)")



###final
dist.abund.spp <- vegdist(wisconsin(abund.spp),method="bray")
dist.abund.spp <- (dist.abund.spp-min(dist.abund.spp))/(max(dist.abund.spp)-min(dist.abund.spp))
#dist.abund.spp <- sqrt(dist.abund.spp)
#dist.abund.spp <- log(dist.abund.spp+1)


#setwd("~/Output")

bl <- read.table(paste0(output,"ResistanceDistance_Blue.txt"),header=T,row.names=1)
gr <- read.table(paste0(output,"ResistanceDistance_Green.txt"),header=T,row.names=1)
blgr <- read.table(paste0(output,"ResistanceDistance_BlueGreen.txt"),header=T,row.names=1)
blDist <- as.dist(as.matrix(bl))
grDist <- as.dist(as.matrix(gr))
blgrDist <- as.dist(as.matrix(blgr))
blMat <- as.matrix(blDist)
grMat <- as.matrix(grDist)
blgrMat <- as.matrix(blgrDist)
blMat <- blMat/max(blMat)
grMat <- grMat/max(grMat)
blgrMat <- blgrMat/max(blgrMat)
blDist <- as.dist(blMat)
grDist <- as.dist(grMat)
blgrDist <- as.dist(blgrMat)


pointdistgeo <- as.matrix(as.dist(pointDistance(coords,lonlat=T)/1000))

par(mfrow=c(1,1),fg="gray70",mar=c(5,5,5,1),cex.lab=1.2,cex.axis=1.1,pty='m',bty='o')

####Env. Louvain Clustering####
envclust <- c(2,2,1,1,1,1,3,3,1,1,2,2,1,3,1,1,3,1,3,1,3,3,2,2,1,2,3,1,2,1,2,3,1,1,1,3,1,2,1,3,3,2,1,1,2,2,2,1,3,2,3,2,2,2,2,3,3,1,1,3,2,3,1,1,1,2,3,1,2,2,1,3)

cols <- colorRampPalette(c('black','purple','darkred','red2','goldenrod1','green4','steelblue','gray','white'))
len.col <- length(unique(envclust))
col.envclust <- rev(cols(len.col))
col2.envclust <- c("yellow2", "dark red", "green3")


