
####Maps####


setwd("~/Data")
lc17 <- raster("S2GLC_Europe_2017_10m_Stkhlm.tif")
lc17_latlon <- projectRaster(lc17, crs="+init=epsg:4326", method="ngb")


par(mfrow=c(1,1),fg="gray50",pty='m',bty='o',mar=c(4,4,4,4),cex.main=1.3,cex.axis=1.1,cex.lab=1.2)

lc17[lc17 == 0] = NA
lc17[lc17 == 255] = NA

lc17_latlon[lc17_latlon == 0] = NA
lc17_latlon[lc17_latlon == 255] = NA


alt.col1 <- c(
       rgb(210,0,0,max=255),
       rgb(253,211,39,max=255),
       #rgb(176,91,16,max=255),
       rgb(35,152,0,max=255),
       rgb(8,98,0,max=255),
       rgb(249,150,39,max=255),
       #rgb(141,139,0,max=255),
       #rgb(95,53,6,max=255),
       rgb(149,107,196,max=255),
       rgb(77,37,106,max=255),
       rgb(154,154,154,max=255),
       #rgb(106,255,255,max=255),
       rgb(20,69,249,max=255))
alt.col2 <- c("#ff2600",
             "#f0cd04",#"#f0cd04",
             "#177546","#177546",
             "#b2cd45",#"#b2cd45","#b2cd45",
             "#a17eb2","#a17eb2",
             "#b6b6b6",#"#b6b6b6",
             "#0185e7")

plot(lc17_latlon,alpha=0.7,
     #col = alt.col1,
     col = alt.col2,
     xlim=c(17.66,18.52),ylim=c(59.1,59.5),xlab="Longitude",ylab="Latitude",legend=F,main="Land Cover with Sampling Sites")
addnortharrow("bottomright",scale=0.62,padin=c(0.25,0.15))
addscalebar(plotunit="km",label.cex=1,plotepsg=4326)
points(xy,bg="white",pch=21,cex=2.2,lwd=2.5,col="gray40")





cols <- colorRampPalette(c('darkred','salmon1','goldenrod1','green4','steelblue','plum2','gray95'))
heatcols <- colorRampPalette(c('darkred','red3','red1','gray95'))

setwd("~/Output")
curr.dens <- stack()
for(i in 1:3) curr.dens <- addLayer(curr.dens, raster(list.files(pattern="*.asc")[i]))

setwd("~/Data")
res.surf <- stack()
for(i in 1:3) res.surf <- addLayer(res.surf, raster(list.files(pattern="*.asc")[i]))

proj4string(curr.dens) = proj4string(res.surf) <- "+init=epsg:3035"
curr.dens <- projectRaster(curr.dens, crs="+init=epsg:4326", method="ngb")
res.surf <- projectRaster(res.surf, crs="+init=epsg:4326", method="ngb")

y.lab <- c("Latitude","","Latitude")
curr.labs <- c("Connectivity: Blue", "Connectivity: Blue + Green","Connectivity: Green")
res.labs <- c("Resistance: Blue", "Resistance: Blue + Green", "Resistance: Green")


par(mfrow=c(2,2),fg="gray50",pty='m',bty='o',mar=c(4,4,4,0.8),cex.main=1.3,cex.axis=1.1,cex.lab=1.2)

for(i in 1:3){
    plot(curr.dens[[i]]/max(na.omit(values(curr.dens[[i]]))),col=rev(heatcols(15)),xlim=c(17.66,18.52),ylim=c(59.1,59.5),main=curr.labs[i],xlab="Longitude",ylab=y.lab[i])
    addnortharrow("bottomright",scale=0.52,padin=c(0.25,0.15),text.col="white")
    addscalebar(plotunit="km",plotepsg=4326,label.col="white",label.cex=1)
    points(xy, pch=16, col="black", cex=0.85)
}


par(mfrow=c(2,2),fg="gray50",pty='m',bty='o',mar=c(4,4,4,0.8),cex.main=1.3,cex.axis=1.1,cex.lab=1.2)

for(i in 1:3){
    plot(res.surf[[i]]/max(na.omit(values(res.surf[[i]]))),col=rev(cols(11)),xlim=c(17.66,18.52),ylim=c(59.1,59.5),main=res.labs[i],xlab="Longitude",ylab=y.lab[i])
    addnortharrow("bottomright",scale=0.52,padin=c(0.25,0.15))
    addscalebar(plotunit="km",plotepsg=4326,,label.cex=1)
}



par(mfrow=c(1,1),fg="gray50",pty='m',bty='o',mar=c(4,4,4,4),cex.main=1.3,cex.axis=1.1,cex.lab=1.2)

blgrconn <- disaggregate(curr.dens[[2]], 10)
plot(blgrconn/max(na.omit(values(blgrconn))),col=rev(heatcols(11)),xlim=c(17.66,18.52),ylim=c(59.1,59.5),main=curr.labs[2],xlab="Longitude",ylab=y.lab[2])
addnortharrow("bottomright",scale=0.62,padin=c(0.25,0.15))
addscalebar(plotunit="km",plotepsg=4326,,label.cex=1)
points(xy, pch=21, col="black", bg=col2.envclust[envclust], cex=1.3)


    



blgr.cols <- colorRampPalette(c('dark blue','steelblue3','seagreen','gray90'))
blgrconn <- blgrconn/max(na.omit(values(blgrconn)))

par(mfrow=c(2,2),fg="gray50",pty='m',bty='o',mar=c(4.5,4.5,4.5,1),cex.main=1.3,cex.axis=1.1,cex.lab=1.2)

plot(blgrconn,col=rev(blgr.cols(11)),xlim=c(17.66,18.52),ylim=c(59.1,59.5),main="Blue+Green Connectivity",
     xlab="Longitude",ylab="Latitude")
addnortharrow("bottomright",scale=0.58,padin=c(0.25,0.15))
addscalebar(plotunit="km",plotepsg=4326,,label.cex=1)
points(xy, pch=21, col="black", bg=col2.envclust[envclust], lwd=2, cex=blgr.conn)

plot(blgrconn,col=rev(blgr.cols(11)),xlim=c(17.66,18.52),ylim=c(59.1,59.5),main="Species Richness",
     xlab="Longitude",ylab="")
addnortharrow("bottomright",scale=0.58,padin=c(0.25,0.15))
addscalebar(plotunit="km",plotepsg=4326,,label.cex=1)
points(xy, pch=21, col="black", bg=col2.envclust[envclust], lwd=2, cex=S/10)

plot(blgrconn,col=rev(blgr.cols(11)),xlim=c(17.66,18.52),ylim=c(59.1,59.5),main="Species Evenness",
     xlab="Longitude",ylab="Latitude")
addnortharrow("bottomright",scale=0.58,padin=c(0.25,0.15))
addscalebar(plotunit="km",plotepsg=4326,,label.cex=1)
points(xy, pch=21, col="black", bg=col2.envclust[envclust], lwd=2, cex=J)





bl.cols <- colorRampPalette(c('dark blue','steelblue3','gray90'))
blconn <- disaggregate(curr.dens[[1]], 10)
blconn <- blconn/max(na.omit(values(blconn)))

gr.cols <- colorRampPalette(c('dark green','seagreen4','gray90'))
grconn <- disaggregate(curr.dens[[3]], 10)
grconn <- grconn/max(na.omit(values(grconn)))

par(mfrow=c(2,2),fg="gray50",pty='m',bty='o',mar=c(4.5,4.5,4.5,1),cex.main=1.3,cex.axis=1.1,cex.lab=1.2)

plot(blconn,col=rev(bl.cols(11)),xlim=c(17.66,18.52),ylim=c(59.1,59.5),main="Current Density: Blue",
     xlab="Longitude",ylab="Latitude")
addnortharrow("bottomright",scale=0.54,padin=c(0.25,0.15))
addscalebar(plotunit="km",plotepsg=4326,,label.cex=1)
points(xy, pch=21, col="black", bg="white", cex=0.75)

plot(grconn,col=rev(gr.cols(11)),xlim=c(17.66,18.52),ylim=c(59.1,59.5),main="Current Density: Green",
     xlab="Longitude",ylab="")
addnortharrow("bottomright",scale=0.54,padin=c(0.25,0.15))
addscalebar(plotunit="km",plotepsg=4326,,label.cex=1)
points(xy, pch=21, col="black", bg="white", cex=0.75)

plot(blgrconn,col=rev(blgr.cols(11)),xlim=c(17.66,18.52),ylim=c(59.1,59.5),main="Current Density: Blue+Green",
     xlab="Longitude",ylab="Latitude")
addnortharrow("bottomright",scale=0.54,padin=c(0.25,0.15))
addscalebar(plotunit="km",plotepsg=4326,,label.cex=1)
points(xy, pch=21, col="black", bg="white", cex=0.75)
