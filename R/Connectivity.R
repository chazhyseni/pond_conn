####RUN AFTER Input.R#####

####Force-Directed Graph####
#library(qgraph)
#setwd("~/Output")
#pdf('FDgraph_SppDist.pdf', width=10, height=10)
#qgraph(dist.abund.spp, groups=as.factor(envclust), color=col2.envclust[envclust], legend=F, labels=F, layout="spring", vsize=3, title="Community Dissimilarity w/ Env. Clusters")
#dev.off()
#pdf('FDgraph_blDist.pdf', width=10, height=10)
#qgraph(blDist, groups=as.factor(envclust), color=col2.envclust[envclust], legend=F, labels=F, layout="spring", vsize=3, title="Blue Resistance w/ Env. Clusters")
#dev.off()
#pdf('FDgraph_grDist.pdf', width=10, height=10)
#qgraph(grDist, groups=as.factor(envclust), color=col2.envclust[envclust], legend=F, labels=F, layout="spring", vsize=3, title="Green Resistance w/ Env. Clusters")
#dev.off()
#pdf('FDgraph_blgrDist.pdf', width=10, height=10)
#qgraph(blgrDist, groups=as.factor(envclust), color=col2.envclust[envclust], legend=F, labels=F, layout="spring", vsize=3, title="Blue+Green Resistance w/ Env. Clusters")
#dev.off()

#pdf('FDgraph_SppSim.pdf', width=10, height=10)
#qgraph(1/dist.abund.spp, groups=as.factor(envclust), color=col2.envclust[envclust], legend=F, labels=F, layout="spring", vsize=3, title="Community Similarity w/ Env. Clusters")
#dev.off()
#pdf('FDgraph_blConn.pdf', width=10, height=10)
#qgraph(1/blDist, groups=as.factor(envclust), color=col2.envclust[envclust], legend=F, labels=F, layout="spring", vsize=3, title="Blue Connectivity w/ Env. Clusters")
#dev.off()
#pdf('FDgraph_grConn.pdf', width=10, height=10)
#qgraph(1/grDist, groups=as.factor(envclust), color=col2.envclust[envclust], legend=F, labels=F, layout="spring", vsize=3, title="Green Connectivity w/ Env. Clusters")
#dev.off()
#pdf('FDgraph_blgrConn.pdf', width=10, height=10)
#qgraph(1/blgrDist, groups=as.factor(envclust), color=col2.envclust[envclust], legend=F, labels=F, layout="spring", vsize=3, title="Blue+Green Connectivity w/ Env. Clusters")
#dev.off()


####Distance Between Groups####
library(usedist)
blD.env <- dist_groups(blDist,envclust)
grD.env <- dist_groups(grDist,envclust)
blgrD.env <- dist_groups(blgrDist,envclust)
 
bl.avg.12 <- mean(blD.env[which(blD.env$Label == "Between 1 and 2"),]$Distance)
bl.avg.13 <- mean(blD.env[which(blD.env$Label == "Between 1 and 3"),]$Distance)
bl.avg.23 <- mean(blD.env[which(blD.env$Label == "Between 2 and 3"),]$Distance)

gr.avg.12 <- mean(grD.env[which(grD.env$Label == "Between 1 and 2"),]$Distance)
gr.avg.13 <- mean(grD.env[which(grD.env$Label == "Between 1 and 3"),]$Distance)
gr.avg.23 <- mean(grD.env[which(grD.env$Label == "Between 2 and 3"),]$Distance)

blgr.avg.12 <- mean(blgrD.env[which(blgrD.env$Label == "Between 1 and 2"),]$Distance)
blgr.avg.13 <- mean(blgrD.env[which(blgrD.env$Label == "Between 1 and 3"),]$Distance)
blgr.avg.23 <- mean(blgrD.env[which(blgrD.env$Label == "Between 2 and 3"),]$Distance)


between.grp.means <- 
rbind(c(bl.avg.12,bl.avg.13,bl.avg.23), 
      c(gr.avg.12,gr.avg.13,gr.avg.23), 
      c(blgr.avg.12,blgr.avg.13,blgr.avg.23))

colnames(between.grp.means) <- c("Between 1 and 2", "Between 1 and 3", "Between 2 and 3")
rownames(between.grp.means) <- c("Blue", "Green", "Blue+Green")

between.grp.means

#######################################################
###Differences in Connectivity between Envtl Cluster###
#######################################################

blD.wit <- blD.env[which(blD.env$Label == "Within 1" | blD.env$Label == "Within 2" | blD.env$Label == "Within 3"),]
grD.wit <- grD.env[which(grD.env$Label == "Within 1" | grD.env$Label == "Within 2" | grD.env$Label == "Within 3"),]
blgrD.wit <- blgrD.env[which(blgrD.env$Label == "Within 1" | blgrD.env$Label == "Within 2" | blgrD.env$Label == "Within 3"),]
blD.wit$Label <- droplevels(blD.wit$Label)
grD.wit$Label <- droplevels(grD.wit$Label)
blgrD.wit$Label <- droplevels(blgrD.wit$Label)

par(mfrow=c(2,2),fg="gray50",pty='m',bty='o',mar=c(4,4,4,0.8),cex.main=1.3,cex.axis=1.1,cex.lab=1.2)

boxplot(1/Distance~Label, data=blD.wit, varwidth=T, boxwex=0.5, cex=1.3, pch=20, xlab="Connectivity within Environmental Cluster", ylab="Blue Connectivity",col=col2.envclust)
boxplot(1/Distance~Label,data=grD.wit, varwidth=T, boxwex=0.5, cex=1.3, pch=20, xlab="Connectivity within Environmental Cluster", ylab="Green Connectivity",col=col2.envclust)
boxplot(1/Distance~Label,data=blgrD.wit, varwidth=T, boxwex=0.5, cex=1.3, pch=20, xlab="Connectivity within Environmental Cluster", ylab="Blue+Green Connectivity",col=col2.envclust)

plot(TukeyHSD(aov(1/Distance~Label,data=blD.wit)))
plot(TukeyHSD(aov(1/Distance~Label,data=grD.wit)))
plot(TukeyHSD(aov(1/Distance~Label,data=blgrD.wit)))




