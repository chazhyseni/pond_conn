####RUN AFTER Input.R#####
####RUN AFTER Connectivity.R#####

set.seed(731956211)

####DBRDA####
Geo <- scores(pcnm(pointdistgeo))
Geo.df <- as.data.frame(Geo)
pco.bl <- pco(blDist)$vectors[,1:69]
pco.bl.df <- as.data.frame(pco.bl)
pco.gr <- pco(grDist)$vectors[,1:69]
pco.gr.df <- as.data.frame(pco.gr)
pco.blgr <- pco(blgrDist)$vectors[,1:69]
pco.blgr.df <- as.data.frame(pco.blgr)

dbrda.geopcnm <- capscale(dist.abund.spp~.,data=Geo.df)
an.geopcnm <- anova(dbrda.geopcnm,by="mar")
i <- 0.25
while(i > 0.05){
	sig.geopcnm <- which(an.geopcnm$`Pr(>F)` < i)
	if(length(sig.geopcnm) > 1){ 
	  Geo <- Geo[,sig.geopcnm]
	}
	else{
	  Geo <- Geo
	}
	Geo.df <- as.data.frame(Geo)
	dbrda.geopcnm <- capscale(dist.abund.spp~.,data=Geo.df)
	an.geopcnm <- anova(dbrda.geopcnm,by="mar")
	i = i - 0.02
}
sig.geopcnm <- which(an.geopcnm$`Pr(>F)` < 0.05)
if(length(sig.geopcnm) != 0) Geo <- Geo[,sig.geopcnm]
Geo.df <- as.data.frame(Geo)
colnames(Geo.df) <- gsub("PCNM","Geo",colnames(Geo.df))
dbrda.geopcnm <- capscale(dist.abund.spp~.,data=Geo.df)
anova(dbrda.geopcnm,by="mar")

dbrda.blpco <- capscale(dist.abund.spp~.,data=pco.bl.df)
an.blpco <- anova(dbrda.blpco,by="mar")
i <- 0.25
while(i > 0.05){
	sig.blpco <- which(an.blpco$`Pr(>F)` < i)
	if(length(sig.blpco) != 0) pco.bl <- pco.bl[,sig.blpco]
	pco.bl.df <- as.data.frame(pco.bl)
	dbrda.blpco <- capscale(dist.abund.spp~.,data=pco.bl.df)
	an.blpco <- anova(dbrda.blpco,by="mar")
	i = i - 0.02
}
sig.blpco <- which(an.blpco$`Pr(>F)` < 0.05)
if(length(sig.blpco) != 0) pco.bl <- pco.bl[,sig.blpco]
pco.bl.df <- as.data.frame(pco.bl)
colnames(pco.bl.df) <- gsub("V","PCo",colnames(pco.bl.df))
colnames(pco.bl.df) <- gsub("PCo","B",colnames(pco.bl.df))
dbrda.blpco <- capscale(dist.abund.spp~.,data=pco.bl.df)
anova(dbrda.blpco,by="mar")


dbrda.grpco <- capscale(dist.abund.spp~.,data=pco.gr.df)
an.grpco <- anova(dbrda.grpco,by="mar")
i <- 0.25
while(i > 0.05){
	sig.grpco <- which(an.grpco$`Pr(>F)` < i)
	if(length(sig.grpco) != 0) pco.gr <- pco.gr[,sig.grpco]
	pco.gr.df <- as.data.frame(pco.gr)
	dbrda.grpco <- capscale(dist.abund.spp~.,data=pco.gr.df)
	an.grpco <- anova(dbrda.grpco,by="mar")
	i = i - 0.02
}
sig.grpco <- which(an.grpco$`Pr(>F)` < 0.05)
if(length(sig.grpco) != 0) pco.gr <- pco.gr[,sig.grpco]
pco.gr.df <- as.data.frame(pco.gr)
colnames(pco.gr.df) <- gsub("V","PCo",colnames(pco.gr.df))
colnames(pco.gr.df) <- gsub("PCo","G",colnames(pco.gr.df))
dbrda.grpco <- capscale(dist.abund.spp~.,data=pco.gr.df)
anova(dbrda.grpco,by="mar")

dbrda.blgrpco <- capscale(dist.abund.spp~.,data=pco.blgr.df)
an.blgrpco <- anova(dbrda.blgrpco,by="mar")
i <- 0.25
while(i > 0.05){
	sig.blgrpco <- which(an.blgrpco$`Pr(>F)` < i)
	if(length(sig.blgrpco) != 0) pco.blgr <- pco.blgr[,sig.blgrpco]
	pco.blgr.df <- as.data.frame(pco.blgr)
	dbrda.blgrpco <- capscale(dist.abund.spp~.,data=pco.blgr.df)
	an.blgrpco <- anova(dbrda.blgrpco,by="mar")
	i = i - 0.02
}
sig.blgrpco <- which(an.blgrpco$`Pr(>F)` < 0.05)
if(length(sig.blgrpco) != 0) pco.blgr <- pco.blgr[,sig.blgrpco]
pco.blgr.df <- as.data.frame(pco.blgr)
colnames(pco.blgr.df) <- gsub("V","PCo",colnames(pco.blgr.df))
colnames(pco.blgr.df) <- gsub("PCo","BG",colnames(pco.blgr.df))
dbrda.blgrpco <- capscale(dist.abund.spp~.,data=pco.blgr.df)
anova(dbrda.blgrpco,by="mar")


dbrda.blpco.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=pco.bl.df)
an.blCgeo <- anova(dbrda.blpco.Cgeo,by="mar")
i <- 0.25
while(i > 0.05){
	sig.blCgeo <- which(an.blCgeo$`Pr(>F)` < i)
	if(length(sig.blCgeo) != 0) pco.blCgeo <- pco.bl[,sig.blCgeo]
	pco.blCgeo.df <- as.data.frame(pco.bl)
	dbrda.blpco.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=pco.blCgeo.df)
	an.blCgeo <- anova(dbrda.blpco.Cgeo,by="mar")
	i = i - 0.02
}
sig.blCgeo <- which(an.blCgeo$`Pr(>F)` < 0.05)
if(length(sig.blCgeo) != 0) pco.bl <- pco.bl[,sig.blCgeo]
pco.blCgeo.df <- as.data.frame(pco.bl)
colnames(pco.blCgeo.df) <- gsub("V","PCo",colnames(pco.blCgeo.df))
colnames(pco.blCgeo.df) <- gsub("PCo","B",colnames(pco.blCgeo.df))
dbrda.blpco.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=pco.blCgeo.df)
anova(dbrda.blpco.Cgeo,by="mar")


dbrda.grpco.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=pco.gr.df)
an.grCgeo <- anova(dbrda.grpco.Cgeo,by="mar")
i <- 0.25
while(i > 0.05){
	sig.grCgeo <- which(an.grCgeo$`Pr(>F)` < i)
	if(length(sig.grCgeo) != 0) pco.grCgeo <- pco.gr[,sig.grCgeo]
	pco.grCgeo.df <- as.data.frame(pco.gr)
	dbrda.grpco.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=pco.grCgeo.df)
	an.grCgeo <- anova(dbrda.grpco.Cgeo,by="mar")
	i = i - 0.02
}
sig.grCgeo <- which(an.grCgeo$`Pr(>F)` < 0.05)
if(length(sig.grCgeo) != 0) pco.gr <- pco.gr[,sig.grCgeo]
pco.grCgeo.df <- as.data.frame(pco.gr)
colnames(pco.grCgeo.df) <- gsub("V","PCo",colnames(pco.grCgeo.df))
colnames(pco.grCgeo.df) <- gsub("PCo","G",colnames(pco.grCgeo.df))
dbrda.grpco.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=pco.grCgeo.df)
anova(dbrda.grpco.Cgeo,by="mar")


dbrda.blgrpco.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=pco.blgr.df)
an.blgrCgeo <- anova(dbrda.blgrpco.Cgeo,by="mar")
i <- 0.25
while(i > 0.05){
	sig.blgrCgeo <- which(an.blgrCgeo$`Pr(>F)` < i)
	if(length(sig.blgrCgeo) != 0) pco.blgrCgeo <- pco.blgr[,sig.blgrCgeo]
	pco.blgrCgeo.df <- as.data.frame(pco.blgr)
	dbrda.blgrpco.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=pco.blgrCgeo.df)
	an.blgrCgeo <- anova(dbrda.blgrpco.Cgeo,by="mar")
	i = i - 0.02
}
sig.blgrCgeo <- which(an.blgrCgeo$`Pr(>F)` < 0.05)
if(length(sig.blgrCgeo) != 0) pco.blgr <- pco.blgr[,sig.blgrCgeo]
pco.blgrCgeo.df <- as.data.frame(pco.blgr)
colnames(pco.blgrCgeo.df) <- gsub("V","PCo",colnames(pco.blgrCgeo.df))
colnames(pco.blgrCgeo.df) <- gsub("PCo","BG",colnames(pco.blgrCgeo.df))
dbrda.blgrpco.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=pco.blgrCgeo.df)
anova(dbrda.blgrpco.Cgeo,by="mar")

	

dbrda.blpco.Cgeo
dbrda.grpco.Cgeo
dbrda.blgrpco.Cgeo

pc.env <- princomp(env)
loadings(pc.env)
summary(pc.env)
env.df <- as.data.frame(scores(pc.env))

dbrda.envCblgrCgeo <- capscale(dist.abund.spp~.+Condition(pco.blgrCgeo),data=env.df)
an.envCblgrCgeo <- anova(dbrda.envCblgrCgeo,by="mar")
envCblgrCgeo <- env.df
i <- 0.25
while(i > 0.05){
	sig.envCblgrCgeo <- which(an.envCblgrCgeo$`Pr(>F)` < i)
	if(length(sig.envCblgrCgeo) > 1){ 
	  envCblgrCgeo <- envCblgrCgeo[,sig.envCblgrCgeo]
	}
	else{
	  envCblgrCgeo <- envCblgrCgeo
	}
	envCblgrCgeo.df <- as.data.frame(envCblgrCgeo)
	dbrda.envCblgrCgeo <- capscale(dist.abund.spp~.+Condition(pco.blgrCgeo),data=envCblgrCgeo.df)
	an.envCblgrCgeo <- anova(dbrda.envCblgrCgeo,by="mar")
	i = i - 0.02
}
anova(dbrda.envCblgrCgeo,by="mar")

dbrda.envCgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=env.df)
an.envCgeo <- anova(dbrda.envCgeo,by="mar")
envCgeo <- env.df
i <- 0.25
while(i > 0.05){
	sig.envCgeo <- which(an.envCgeo$`Pr(>F)` < i)
	if(length(sig.envCgeo) > 1){ 
	  envCgeo <- envCgeo[,sig.envCgeo]
	}
	else{
	  envCgeo <- envCgeo
	}
	envCgeo.df <- as.data.frame(envCgeo)
	dbrda.envCgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=envCgeo.df)
	an.envCgeo <- anova(dbrda.envCgeo,by="mar")
	i = i - 0.02
}
sig.envCgeo <- which(an.envCgeo$`Pr(>F)` < 0.05)
if(length(sig.envCgeo) != 0) envCgeo <- envCgeo[,sig.envCgeo]
envCgeo.df <- as.data.frame(envCgeo)
colnames(envCgeo.df) <- gsub("Comp.","PC",colnames(envCgeo.df))
colnames(envCgeo.df) <- gsub("PC","Env",colnames(envCgeo.df))
dbrda.envCgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=envCgeo.df)
anova(dbrda.envCgeo,by="mar")

dbrda.env <- capscale(dist.abund.spp~.,data=env.df)
an.env <- anova(dbrda.env,by="mar")
env.sig <- env.df
i <- 0.25
while(i > 0.05){
	sig.env <- which(an.env$`Pr(>F)` < i)
	if(length(sig.env) > 1){ 
	  env.sig <- env.sig[,sig.env]
	}
	else{
	  env.sig <- env.sig
	}
	env.sig.df <- as.data.frame(env.sig)
	dbrda.env <- capscale(dist.abund.spp~.,data=env.sig.df)
	an.env <- anova(dbrda.env,by="mar")
	i = i - 0.02
}
sig.env <- which(an.env$`Pr(>F)` < 0.05)
if(length(sig.env) != 0) env.sig <- env.sig[,sig.env]
env.sig.df <- as.data.frame(env.sig)
colnames(env.sig.df) <- gsub("Comp.","PC",colnames(env.sig.df))
colnames(env.sig.df) <- gsub("PC","Env",colnames(env.sig.df))
dbrda.env <- capscale(dist.abund.spp~.,data=env.sig.df)
anova(dbrda.env,by="mar")


dbrda.blgrenvCgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=cbind(env.sig.df,pco.blgr.df))
an.blgrenvCgeo <- anova(dbrda.blgrenvCgeo,by="mar")
blgrenvCgeo <- cbind(env.sig.df,pco.blgr.df)
i <- 0.25
while(i > 0.05){
	sig.blgrenvCgeo <- which(an.blgrenvCgeo$`Pr(>F)` < i)
	if(length(sig.blgrenvCgeo) != 0) blgrenvCgeo <- blgrenvCgeo[,sig.blgrenvCgeo]
	blgrenvCgeo.df <- as.data.frame(blgrenvCgeo)
	dbrda.blgrenvCgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=blgrenvCgeo.df)
	an.blgrenvCgeo <- anova(dbrda.blgrenvCgeo,by="mar")
	i = i - 0.02
}
sig.blgrenvCgeo <- which(an.blgrenvCgeo$`Pr(>F)` < 0.05)
if(length(sig.blgrenvCgeo) != 0) blgrenvCgeo <- blgrenvCgeo[,sig.blgrenvCgeo]
blgrenvCgeo.df <- as.data.frame(blgrenvCgeo)
dbrda.blgrenvCgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=blgrenvCgeo.df)
anova(dbrda.blgrenvCgeo,by="mar")




par(mfrow=c(1,1),fg="gray50",pty='m',bty='o',mar=c(4,4,4,4),cex.main=1.5,cex.axis=1.2,cex.lab=1.3)

mod1 <- varpart(dist.abund.spp, ~., Geo, pco.bl, pco.gr, data=env.sig.df)
mod1
plot(mod1, Xnames=c("Env","Geo","Blue","Green"),bg=c(2,1,4,3),digits=2)


mod2 <- varpart(dist.abund.spp, ~., pco.bl, pco.gr, pco.blgr, data=env.sig.df)
mod2
plot(mod2, Xnames=c("Env","Blue","Green","Blue+Green"),bg=c(2,4,3,5),digits=2)


mod3 <- varpart(dist.abund.spp, ~., Geo, pco.blgr, data=env.sig.df)
mod3
plot(mod3, Xnames=c("Env","Geo","Blue+Green"),bg=c(2,1,5), digits=2)







#### BLGR CONDITIONED ON GEO #### 

dbrda.blgrenvCgeo<-capscale(dist.abund.spp~.+Condition(Geo),data=blgrenvCgeo.df,comm=wisconsin(abund.spp))
mod.rn = rownames(dbrda.blgrenvCgeo$CCA$v)
mod.rn[which(rowSums(abs(dbrda.blgrenvCgeo$CCA$v[,1:2]))<0.26)]=rep("",length(mod.rn[which(rowSums(abs(dbrda.blgrenvCgeo$CCA$v[,1:2]))<0.26)]))

hotT2env.blgrenvCgeo.12 <- hotelling.test(scores(dbrda.blgrenvCgeo)$sites~envclust,pair=c(1,2), perm=T, B=10000)
hotT2env.blgrenvCgeo.13 <- hotelling.test(scores(dbrda.blgrenvCgeo)$sites~envclust,pair=c(1,3), perm=T, B=10000)
hotT2env.blgrenvCgeo.23 <- hotelling.test(scores(dbrda.blgrenvCgeo)$sites~envclust,pair=c(2,3), perm=T, B=10000)
hotT2env.blgrenvCgeo.pvals <- c(hotT2env.blgrenvCgeo.12$pval,hotT2env.blgrenvCgeo.13$pval,hotT2env.blgrenvCgeo.23$pval)
names(hotT2env.blgrenvCgeo.pvals) <- c("Gr.AS","Gr.Fo","AS.Fo")

hotT2env.blgrenvCgeo.pvals

XaxisLabels<-paste("Axis 1 (",round(dbrda.blgrenvCgeo$CCA$eig[1]/sum(dbrda.blgrenvCgeo$CCA$eig),3)*100,"%)",sep="")
YaxisLabels<-paste("Axis 2 (",round(dbrda.blgrenvCgeo$CCA$eig[2]/sum(dbrda.blgrenvCgeo$CCA$eig),3)*100,"%)",sep="")
ZaxisLabels<-paste("Axis 3 (",round(dbrda.blgrenvCgeo$CCA$eig[3]/sum(dbrda.blgrenvCgeo$CCA$eig),3)*100,"%)",sep="")
cent1.12<-colMeans(dbrda.blgrenvCgeo$CCA$wa[ which(envclust == 1),1:2])
cent2.12<-colMeans(dbrda.blgrenvCgeo$CCA$wa[ which(envclust == 2),1:2])
cent3.12<-colMeans(dbrda.blgrenvCgeo$CCA$wa[ which(envclust == 3),1:2])

par(mfrow=c(1,1),fg="gray70",mar=c(4,4,4,1),cex.lab=1.2,cex.axis=1.1,pty='m',bty='o')

plot(dbrda.blgrenvCgeo$CCA$wa[,1:2],type="n",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5), main="BG + Env | Geo", xlab=XaxisLabels,ylab=YaxisLabels)
arrows(0,0,dbrda.blgrenvCgeo$CCA$v[,1]*0.72,dbrda.blgrenvCgeo$CCA$v[,2]*0.72,length=0.1,angle=15,
        cex=0.4,lwd=1.6,col="steelblue1")
text(dbrda.blgrenvCgeo$CCA$v[,1:2]*0.76,labels=mod.rn,cex=0.7,lwd=2,col="steelblue1")
arrows(0,0,dbrda.blgrenvCgeo$CCA$biplot[,1]*0.52,dbrda.blgrenvCgeo$CCA$biplot[,2]*0.52,length=0.1,angle=15,
        cex=1.1,lwd=2,col="red3")
text(dbrda.blgrenvCgeo$CCA$biplot[,1:2]*0.55,labels=rownames(dbrda.blgrenvCgeo$CCA$biplot),cex=1.1,lwd=2,col="red3")
points(dbrda.blgrenvCgeo$CCA$wa[,1:2],bg=col2.envclust[envclust],pch=22,cex=1.3,col="gray40")
legend(legend=sort(unique(envclust)),
       pch=22,pt.bg=col2.envclust,col=rep("gray40",6),pt.cex=rep(1.5,3),text.col="black",list(x=0.45,y=0.5),bty='n')
points(rbind(cent1.12,cent2.12,cent3.12),bg=col2.envclust,pch=22,cex=2.2,lwd=3.2,col="gray10")
hull<-ordihull(dbrda.blgrenvCgeo, groups=envclust, col=col2.envclust,draw="none")
lines(hull[[1]]/6.7,col=col2.envclust[1],lwd=1.5)
lines(hull[[2]]/6.7,col=col2.envclust[2],lwd=1.5)
lines(hull[[3]]/6.7,col=col2.envclust[3],lwd=1.5)


pl <- ordiplot3d(dbrda.blgrenvCgeo, scaling = "symmetric", angle=30, type="n", xlab=XaxisLabels, ylab=YaxisLabels, zlab=ZaxisLabels)
points(pl, bg=col2.envclust[envclust], pch=22, cex=1.5, lwd=2, col="gray40")




#### DBRDA: CONDITIONED ON GEO #### 


set.seed(4437813)

dbrda.bl.env.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=cbind(env.sig.df,pco.bl.df))
an.bl.env.Cgeo <- anova(dbrda.bl.env.Cgeo,by="mar")
bl.env.Cgeo <- cbind(env.sig.df,pco.bl.df)
i <- 0.25
while(i > 0.05){
    sig.bl.env.Cgeo <- which(an.bl.env.Cgeo$`Pr(>F)` < i)
    if(length(sig.bl.env.Cgeo) != 0) bl.env.Cgeo <- bl.env.Cgeo[,sig.bl.env.Cgeo]
    bl.env.Cgeo.df <- as.data.frame(bl.env.Cgeo)
    dbrda.bl.env.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=bl.env.Cgeo.df)
    an.bl.env.Cgeo <- anova(dbrda.bl.env.Cgeo,by="mar")
    i = i - 0.02
}
sig.bl.env.Cgeo <- which(an.bl.env.Cgeo$`Pr(>F)` < 0.05)
if(length(sig.bl.env.Cgeo) != 0) bl.env.Cgeo <- bl.env.Cgeo[,sig.bl.env.Cgeo]
bl.env.Cgeo.df <- as.data.frame(bl.env.Cgeo)
dbrda.bl.env.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=bl.env.Cgeo.df)
anova(dbrda.bl.env.Cgeo,by="mar")


dbrda.gr.env.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=cbind(env.sig.df,pco.gr.df))
an.gr.env.Cgeo <- anova(dbrda.gr.env.Cgeo,by="mar")
gr.env.Cgeo <- cbind(env.sig.df,pco.gr.df)
i <- 0.25
while(i > 0.05){
    sig.gr.env.Cgeo <- which(an.gr.env.Cgeo$`Pr(>F)` < i)
    if(length(sig.gr.env.Cgeo) != 0) gr.env.Cgeo <- gr.env.Cgeo[,sig.gr.env.Cgeo]
    gr.env.Cgeo.df <- as.data.frame(gr.env.Cgeo)
    dbrda.gr.env.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=gr.env.Cgeo.df)
    an.gr.env.Cgeo <- anova(dbrda.gr.env.Cgeo,by="mar")
    i = i - 0.02
}
sig.gr.env.Cgeo <- which(an.gr.env.Cgeo$`Pr(>F)` < 0.05)
if(length(sig.gr.env.Cgeo) != 0) gr.env.Cgeo <- gr.env.Cgeo[,sig.gr.env.Cgeo]
gr.env.Cgeo.df <- as.data.frame(gr.env.Cgeo)
dbrda.gr.env.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=gr.env.Cgeo.df)
anova(dbrda.gr.env.Cgeo,by="mar")




dbrda.bl.gr.env.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=cbind(env.sig.df,pco.bl.df,pco.gr.df))
an.bl.gr.env.Cgeo <- anova(dbrda.bl.gr.env.Cgeo,by="mar")
bl.gr.env.Cgeo <- cbind(env.sig.df,pco.bl.df,pco.gr.df)
i <- 0.25
while(i > 0.05){
    sig.bl.gr.env.Cgeo <- which(an.bl.gr.env.Cgeo$`Pr(>F)` < i)
    if(length(sig.bl.gr.env.Cgeo) != 0) bl.gr.env.Cgeo <- bl.gr.env.Cgeo[,sig.bl.gr.env.Cgeo]
    bl.gr.env.Cgeo.df <- as.data.frame(bl.gr.env.Cgeo)
    dbrda.bl.gr.env.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=bl.gr.env.Cgeo.df)
    an.bl.gr.env.Cgeo <- anova(dbrda.bl.gr.env.Cgeo,by="mar")
    i = i - 0.02
}
sig.bl.gr.env.Cgeo <- which(an.bl.gr.env.Cgeo$`Pr(>F)` < 0.05)
if(length(sig.bl.gr.env.Cgeo) != 0) bl.gr.env.Cgeo <- bl.gr.env.Cgeo[,sig.bl.gr.env.Cgeo]
bl.gr.env.Cgeo.df <- as.data.frame(bl.gr.env.Cgeo)
dbrda.bl.gr.env.Cgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=bl.gr.env.Cgeo.df)
anova(dbrda.bl.gr.env.Cgeo,by="mar")




dbrda.blgrenvCgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=cbind(env.sig.df,pco.blgr.df))
an.blgrenvCgeo <- anova(dbrda.blgrenvCgeo,by="mar")
blgrenvCgeo <- cbind(env.sig.df,pco.blgr.df)
i <- 0.25
while(i > 0.05){
	sig.blgrenvCgeo <- which(an.blgrenvCgeo$`Pr(>F)` < i)
	if(length(sig.blgrenvCgeo) != 0) blgrenvCgeo <- blgrenvCgeo[,sig.blgrenvCgeo]
	blgrenvCgeo.df <- as.data.frame(blgrenvCgeo)
	dbrda.blgrenvCgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=blgrenvCgeo.df)
	an.blgrenvCgeo <- anova(dbrda.blgrenvCgeo,by="mar")
	i = i - 0.02
}
sig.blgrenvCgeo <- which(an.blgrenvCgeo$`Pr(>F)` < 0.05)
if(length(sig.blgrenvCgeo) != 0) blgrenvCgeo <- blgrenvCgeo[,sig.blgrenvCgeo]
blgrenvCgeo.df <- as.data.frame(blgrenvCgeo)
dbrda.blgrenvCgeo <- capscale(dist.abund.spp~.+Condition(Geo),data=blgrenvCgeo.df)
anova(dbrda.blgrenvCgeo,by="mar")





#### PLOTS: CONDITIONED ON GEO #### 



par(mfrow=c(2,2),fg="gray70",mar=c(4,4,4,1),cex.lab=1.2,cex.axis=1.1,pty='m',bty='o')


dbrda.bl.env.Cgeo<-capscale(dist.abund.spp~.+Condition(Geo),data=bl.env.Cgeo.df,comm=wisconsin(abund.spp))

mod.rn = rownames(dbrda.bl.env.Cgeo$CCA$v)
mod.rn[which(rowSums(abs(dbrda.bl.env.Cgeo$CCA$v[,1:2]))<0.26)]=rep("",length(mod.rn[which(rowSums(abs(dbrda.bl.env.Cgeo$CCA$v[,1:2]))<0.26)]))

XaxisLabels<-paste("Axis 1 (",round(dbrda.bl.env.Cgeo$CCA$eig[1]/sum(dbrda.bl.env.Cgeo$CCA$eig),3)*100,"%)",sep="")
YaxisLabels<-paste("Axis 2 (",round(dbrda.bl.env.Cgeo$CCA$eig[2]/sum(dbrda.bl.env.Cgeo$CCA$eig),3)*100,"%)",sep="")
ZaxisLabels<-paste("Axis 3 (",round(dbrda.bl.env.Cgeo$CCA$eig[3]/sum(dbrda.bl.env.Cgeo$CCA$eig),3)*100,"%)",sep="")
cent1.12<-colMeans(dbrda.bl.env.Cgeo$CCA$wa[ which(envclust == 1),1:2])
cent2.12<-colMeans(dbrda.bl.env.Cgeo$CCA$wa[ which(envclust == 2),1:2])
cent3.12<-colMeans(dbrda.bl.env.Cgeo$CCA$wa[ which(envclust == 3),1:2])

plot(dbrda.bl.env.Cgeo$CCA$wa[,1:2],type="n",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5), main="(B + Env) | Geo", xlab=XaxisLabels,ylab=YaxisLabels)
arrows(0,0,dbrda.bl.env.Cgeo$CCA$v[,1]*0.72,dbrda.bl.env.Cgeo$CCA$v[,2]*0.72,length=0.1,angle=15,
        cex=0.4,lwd=1.6,col="steelblue1")
text(dbrda.bl.env.Cgeo$CCA$v[,1:2]*0.77,labels=mod.rn,cex=0.7,lwd=2,col="steelblue1")
arrows(0,0,dbrda.bl.env.Cgeo$CCA$biplot[,1]*0.52,dbrda.bl.env.Cgeo$CCA$biplot[,2]*0.52,length=0.1,angle=15,
        cex=1.1,lwd=2,col="red3")
text(dbrda.bl.env.Cgeo$CCA$biplot[,1:2]*0.57,labels=rownames(dbrda.bl.env.Cgeo$CCA$biplot),cex=1.1,lwd=2,col="red3")
points(dbrda.bl.env.Cgeo$CCA$wa[,1:2],bg=col2.envclust[envclust],pch=22,cex=1.3,col="gray40")
legend(legend=c("G","A","F"),
       pch=22,pt.bg=col2.envclust,col=rep("gray40",6),pt.cex=rep(1.5,3),text.col="black",list(x=0.42,y=0.5),bty='n')
points(rbind(cent1.12,cent2.12,cent3.12),bg=col2.envclust,pch=22,cex=2.2,lwd=3.2,col="gray10")
hull<-ordihull(dbrda.bl.env.Cgeo, groups=envclust, col=col2.envclust,draw="none")
lines(hull[[1]]/6.7,col=col2.envclust[1],lwd=1.5)
lines(hull[[2]]/6.7,col=col2.envclust[2],lwd=1.5)
lines(hull[[3]]/6.7,col=col2.envclust[3],lwd=1.5)


dbrda.gr.env.Cgeo<-capscale(dist.abund.spp~.+Condition(Geo),data=gr.env.Cgeo.df,comm=wisconsin(abund.spp))

mod.rn = rownames(dbrda.gr.env.Cgeo$CCA$v)
mod.rn[which(rowSums(abs(dbrda.gr.env.Cgeo$CCA$v[,1:2]))<0.26)]=rep("",length(mod.rn[which(rowSums(abs(dbrda.gr.env.Cgeo$CCA$v[,1:2]))<0.26)]))

XaxisLabels<-paste("Axis 1 (",round(dbrda.gr.env.Cgeo$CCA$eig[1]/sum(dbrda.gr.env.Cgeo$CCA$eig),3)*100,"%)",sep="")
YaxisLabels<-paste("Axis 2 (",round(dbrda.gr.env.Cgeo$CCA$eig[2]/sum(dbrda.gr.env.Cgeo$CCA$eig),3)*100,"%)",sep="")
ZaxisLabels<-paste("Axis 3 (",round(dbrda.gr.env.Cgeo$CCA$eig[3]/sum(dbrda.gr.env.Cgeo$CCA$eig),3)*100,"%)",sep="")
cent1.12<-colMeans(dbrda.gr.env.Cgeo$CCA$wa[ which(envclust == 1),1:2])
cent2.12<-colMeans(dbrda.gr.env.Cgeo$CCA$wa[ which(envclust == 2),1:2])
cent3.12<-colMeans(dbrda.gr.env.Cgeo$CCA$wa[ which(envclust == 3),1:2])

plot(dbrda.gr.env.Cgeo$CCA$wa[,1:2],type="n",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5), main="(G + Env) | Geo", xlab=XaxisLabels,ylab=YaxisLabels)
arrows(0,0,dbrda.gr.env.Cgeo$CCA$v[,1]*0.72,dbrda.gr.env.Cgeo$CCA$v[,2]*0.72,length=0.1,angle=15,
        cex=0.4,lwd=1.6,col="steelblue1")
text(dbrda.gr.env.Cgeo$CCA$v[,1:2]*0.77,labels=mod.rn,cex=0.7,lwd=2,col="steelblue1")
arrows(0,0,dbrda.gr.env.Cgeo$CCA$biplot[,1]*0.52,dbrda.gr.env.Cgeo$CCA$biplot[,2]*0.52,length=0.1,angle=15,
        cex=1.1,lwd=2,col="red3")
text(dbrda.gr.env.Cgeo$CCA$biplot[,1:2]*0.57,labels=rownames(dbrda.gr.env.Cgeo$CCA$biplot),cex=1.1,lwd=2,col="red3")
points(dbrda.gr.env.Cgeo$CCA$wa[,1:2],bg=col2.envclust[envclust],pch=22,cex=1.3,col="gray40")
legend(legend=c("G","A","F"),
       pch=22,pt.bg=col2.envclust,col=rep("gray40",6),pt.cex=rep(1.5,3),text.col="black",list(x=0.42,y=0.5),bty='n')
points(rbind(cent1.12,cent2.12,cent3.12),bg=col2.envclust,pch=22,cex=2.2,lwd=3.2,col="gray10")
hull<-ordihull(dbrda.gr.env.Cgeo, groups=envclust, col=col2.envclust,draw="none")
lines(hull[[1]]/6.7,col=col2.envclust[1],lwd=1.5)
lines(hull[[2]]/6.7,col=col2.envclust[2],lwd=1.5)
lines(hull[[3]]/6.7,col=col2.envclust[3],lwd=1.5)




dbrda.bl.gr.env.Cgeo<-capscale(dist.abund.spp~.+Condition(Geo),data=bl.gr.env.Cgeo.df,comm=wisconsin(abund.spp))

mod.rn = rownames(dbrda.bl.gr.env.Cgeo$CCA$v)
mod.rn[which(rowSums(abs(dbrda.bl.gr.env.Cgeo$CCA$v[,1:2]))<0.26)]=rep("",length(mod.rn[which(rowSums(abs(dbrda.bl.gr.env.Cgeo$CCA$v[,1:2]))<0.26)]))

XaxisLabels<-paste("Axis 1 (",round(dbrda.bl.gr.env.Cgeo$CCA$eig[1]/sum(dbrda.bl.gr.env.Cgeo$CCA$eig),3)*100,"%)",sep="")
YaxisLabels<-paste("Axis 2 (",round(dbrda.bl.gr.env.Cgeo$CCA$eig[2]/sum(dbrda.bl.gr.env.Cgeo$CCA$eig),3)*100,"%)",sep="")
ZaxisLabels<-paste("Axis 3 (",round(dbrda.bl.gr.env.Cgeo$CCA$eig[3]/sum(dbrda.bl.gr.env.Cgeo$CCA$eig),3)*100,"%)",sep="")
cent1.12<-colMeans(dbrda.bl.gr.env.Cgeo$CCA$wa[ which(envclust == 1),1:2])
cent2.12<-colMeans(dbrda.bl.gr.env.Cgeo$CCA$wa[ which(envclust == 2),1:2])
cent3.12<-colMeans(dbrda.bl.gr.env.Cgeo$CCA$wa[ which(envclust == 3),1:2])

plot(dbrda.bl.gr.env.Cgeo$CCA$wa[,1:2],type="n",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5), main="(B + G + Env) | Geo", xlab=XaxisLabels,ylab=YaxisLabels)
arrows(0,0,dbrda.bl.gr.env.Cgeo$CCA$v[,1]*0.72,dbrda.bl.gr.env.Cgeo$CCA$v[,2]*0.72,length=0.1,angle=15,
        cex=0.4,lwd=1.6,col="steelblue1")
text(dbrda.bl.gr.env.Cgeo$CCA$v[,1:2]*0.77,labels=mod.rn,cex=0.7,lwd=2,col="steelblue1")
arrows(0,0,dbrda.bl.gr.env.Cgeo$CCA$biplot[,1]*0.52,dbrda.bl.gr.env.Cgeo$CCA$biplot[,2]*0.52,length=0.1,angle=15,
        cex=1.1,lwd=2,col="red3")
text(dbrda.bl.gr.env.Cgeo$CCA$biplot[,1:2]*0.57,labels=rownames(dbrda.bl.gr.env.Cgeo$CCA$biplot),cex=1.1,lwd=2,col="red3")
points(dbrda.bl.gr.env.Cgeo$CCA$wa[,1:2],bg=col2.envclust[envclust],pch=22,cex=1.3,col="gray40")
legend(legend=c("G","A","F"),
       pch=22,pt.bg=col2.envclust,col=rep("gray40",6),pt.cex=rep(1.5,3),text.col="black",list(x=0.42,y=0.5),bty='n')
points(rbind(cent1.12,cent2.12,cent3.12),bg=col2.envclust,pch=22,cex=2.2,lwd=3.2,col="gray10")
hull<-ordihull(dbrda.bl.gr.env.Cgeo, groups=envclust, col=col2.envclust,draw="none")
lines(hull[[1]]/6.7,col=col2.envclust[1],lwd=1.5)
lines(hull[[2]]/6.7,col=col2.envclust[2],lwd=1.5)
lines(hull[[3]]/6.7,col=col2.envclust[3],lwd=1.5)




dbrda.blgrenvCgeo<-capscale(dist.abund.spp~.+Condition(Geo),data=blgrenvCgeo.df,comm=wisconsin(abund.spp))

mod.rn = rownames(dbrda.blgrenvCgeo$CCA$v)
mod.rn[which(rowSums(abs(dbrda.blgrenvCgeo$CCA$v[,1:2]))<0.26)]=rep("",length(mod.rn[which(rowSums(abs(dbrda.blgrenvCgeo$CCA$v[,1:2]))<0.26)]))

XaxisLabels<-paste("Axis 1 (",round(dbrda.blgrenvCgeo$CCA$eig[1]/sum(dbrda.blgrenvCgeo$CCA$eig),3)*100,"%)",sep="")
YaxisLabels<-paste("Axis 2 (",round(dbrda.blgrenvCgeo$CCA$eig[2]/sum(dbrda.blgrenvCgeo$CCA$eig),3)*100,"%)",sep="")
ZaxisLabels<-paste("Axis 3 (",round(dbrda.blgrenvCgeo$CCA$eig[3]/sum(dbrda.blgrenvCgeo$CCA$eig),3)*100,"%)",sep="")
cent1.12<-colMeans(dbrda.blgrenvCgeo$CCA$wa[ which(envclust == 1),1:2])
cent2.12<-colMeans(dbrda.blgrenvCgeo$CCA$wa[ which(envclust == 2),1:2])
cent3.12<-colMeans(dbrda.blgrenvCgeo$CCA$wa[ which(envclust == 3),1:2])

plot(dbrda.blgrenvCgeo$CCA$wa[,1:2],type="n",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5), main="(BG + Env) | Geo", xlab=XaxisLabels,ylab=YaxisLabels)
arrows(0,0,dbrda.blgrenvCgeo$CCA$v[,1]*0.72,dbrda.blgrenvCgeo$CCA$v[,2]*0.72,length=0.1,angle=15,
        cex=0.4,lwd=1.6,col="steelblue1")
text(dbrda.blgrenvCgeo$CCA$v[,1:2]*0.77,labels=mod.rn,cex=0.7,lwd=2,col="steelblue1")
arrows(0,0,dbrda.blgrenvCgeo$CCA$biplot[,1]*0.52,dbrda.blgrenvCgeo$CCA$biplot[,2]*0.52,length=0.1,angle=15,
        cex=1.1,lwd=2,col="red3")
text(dbrda.blgrenvCgeo$CCA$biplot[,1:2]*0.57,labels=rownames(dbrda.blgrenvCgeo$CCA$biplot),cex=1.1,lwd=2,col="red3")
points(dbrda.blgrenvCgeo$CCA$wa[,1:2],bg=col2.envclust[envclust],pch=22,cex=1.3,col="gray40")
legend(legend=c("G","A","F"),
       pch=22,pt.bg=col2.envclust,col=rep("gray40",6),pt.cex=rep(1.5,3),text.col="black",list(x=0.42,y=0.5),bty='n')
points(rbind(cent1.12,cent2.12,cent3.12),bg=col2.envclust,pch=22,cex=2.2,lwd=3.2,col="gray10")
hull<-ordihull(dbrda.blgrenvCgeo, groups=envclust, col=col2.envclust,draw="none")
lines(hull[[1]]/6.7,col=col2.envclust[1],lwd=1.5)
lines(hull[[2]]/6.7,col=col2.envclust[2],lwd=1.5)
lines(hull[[3]]/6.7,col=col2.envclust[3],lwd=1.5)











#### DBRDA: CONDITIONED ON ENV+GEO #### 


set.seed(21673419)

dbrda.bl.Cenvgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=pco.bl.df)
an.bl.Cenvgeo <- anova(dbrda.bl.Cenvgeo,by="mar")
bl.Cenvgeo <- pco.bl.df
i <- 0.25
while(i > 0.05){
    sig.bl.Cenvgeo <- which(an.bl.Cenvgeo$`Pr(>F)` < i)
    if(length(sig.bl.Cenvgeo) != 0) bl.Cenvgeo <- bl.Cenvgeo[,sig.bl.Cenvgeo]
    bl.Cenvgeo.df <- as.data.frame(bl.Cenvgeo)
    dbrda.bl.Cenvgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=bl.Cenvgeo.df)
    an.bl.Cenvgeo <- anova(dbrda.bl.Cenvgeo,by="mar")
    i = i - 0.02
}
sig.bl.Cenvgeo <- which(an.bl.Cenvgeo$`Pr(>F)` < 0.05)
if(length(sig.bl.Cenvgeo) != 0) bl.Cenvgeo <- bl.Cenvgeo[,sig.bl.Cenvgeo]
bl.Cenvgeo.df <- as.data.frame(bl.Cenvgeo)
dbrda.bl.Cenvgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=bl.Cenvgeo.df)
anova(dbrda.bl.Cenvgeo,by="mar")


dbrda.gr.Cenvgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=pco.gr.df)
an.gr.Cenvgeo <- anova(dbrda.gr.Cenvgeo,by="mar")
gr.Cenvgeo <- pco.gr.df
i <- 0.25
while(i > 0.05){
    sig.gr.Cenvgeo <- which(an.gr.Cenvgeo$`Pr(>F)` < i)
    if(length(sig.gr.Cenvgeo) != 0) gr.Cenvgeo <- gr.Cenvgeo[,sig.gr.Cenvgeo]
    gr.Cenvgeo.df <- as.data.frame(gr.Cenvgeo)
    dbrda.gr.Cenvgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=gr.Cenvgeo.df)
    an.gr.Cenvgeo <- anova(dbrda.gr.Cenvgeo,by="mar")
    i = i - 0.02
}
sig.gr.Cenvgeo <- which(an.gr.Cenvgeo$`Pr(>F)` < 0.05)
if(length(sig.gr.Cenvgeo) != 0) gr.Cenvgeo <- gr.Cenvgeo[,sig.gr.Cenvgeo]
gr.Cenvgeo.df <- as.data.frame(gr.Cenvgeo)
dbrda.gr.Cenvgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=gr.Cenvgeo.df)
anova(dbrda.gr.Cenvgeo,by="mar")




dbrda.bl.gr.Cenvgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=cbind(pco.bl.df,pco.gr.df))
an.bl.gr.Cenvgeo <- anova(dbrda.bl.gr.Cenvgeo,by="mar")
bl.gr.Cenvgeo <- cbind(pco.bl.df,pco.gr.df)
i <- 0.25
while(i > 0.05){
    sig.bl.gr.Cenvgeo <- which(an.bl.gr.Cenvgeo$`Pr(>F)` < i)
    if(length(sig.bl.gr.Cenvgeo) != 0) bl.gr.Cenvgeo <- bl.gr.Cenvgeo[,sig.bl.gr.Cenvgeo]
    bl.gr.Cenvgeo.df <- as.data.frame(bl.gr.Cenvgeo)
    dbrda.bl.gr.Cenvgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=bl.gr.Cenvgeo.df)
    an.bl.gr.Cenvgeo <- anova(dbrda.bl.gr.Cenvgeo,by="mar")
    i = i - 0.02
}
sig.bl.gr.Cenvgeo <- which(an.bl.gr.Cenvgeo$`Pr(>F)` < 0.05)
if(length(sig.bl.gr.Cenvgeo) != 0) bl.gr.Cenvgeo <- bl.gr.Cenvgeo[,sig.bl.gr.Cenvgeo]
bl.gr.Cenvgeo.df <- as.data.frame(bl.gr.Cenvgeo)
dbrda.bl.gr.Cenvgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=bl.gr.Cenvgeo.df)
anova(dbrda.bl.gr.Cenvgeo,by="mar")




dbrda.blgrenvCgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=pco.blgr.df)
an.blgrenvCgeo <- anova(dbrda.blgrenvCgeo,by="mar")
blgrenvCgeo <- pco.blgr.df
i <- 0.25
while(i > 0.05){
	sig.blgrenvCgeo <- which(an.blgrenvCgeo$`Pr(>F)` < i)
	if(length(sig.blgrenvCgeo) != 0) blgrenvCgeo <- blgrenvCgeo[,sig.blgrenvCgeo]
	blgrenvCgeo.df <- as.data.frame(blgrenvCgeo)
	dbrda.blgrenvCgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=blgrenvCgeo.df)
	an.blgrenvCgeo <- anova(dbrda.blgrenvCgeo,by="mar")
	i = i - 0.02
}
sig.blgrenvCgeo <- which(an.blgrenvCgeo$`Pr(>F)` < 0.05)
if(length(sig.blgrenvCgeo) != 0) blgrenvCgeo <- blgrenvCgeo[,sig.blgrenvCgeo]
blgrenvCgeo.df <- as.data.frame(blgrenvCgeo)
dbrda.blgrenvCgeo <- capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=blgrenvCgeo.df)
anova(dbrda.blgrenvCgeo,by="mar")




#### PLOTS: CONDITIONED ON ENV+GEO #### 


par(mfrow=c(2,2),fg="gray70",mar=c(4,4,4,1),cex.lab=1.2,cex.axis=1.1,pty='m',bty='o')


dbrda.bl.Cenvgeo<-capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=bl.Cenvgeo.df,comm=wisconsin(abund.spp))

mod.rn = rownames(dbrda.bl.Cenvgeo$CCA$v)
mod.rn[which(rowSums(abs(dbrda.bl.Cenvgeo$CCA$v[,1:2]))<0.26)]=rep("",length(mod.rn[which(rowSums(abs(dbrda.bl.Cenvgeo$CCA$v[,1:2]))<0.26)]))

XaxisLabels<-paste("Axis 1 (",round(dbrda.bl.Cenvgeo$CCA$eig[1]/sum(dbrda.bl.Cenvgeo$CCA$eig),3)*100,"%)",sep="")
YaxisLabels<-paste("Axis 2 (",round(dbrda.bl.Cenvgeo$CCA$eig[2]/sum(dbrda.bl.Cenvgeo$CCA$eig),3)*100,"%)",sep="")
ZaxisLabels<-paste("Axis 3 (",round(dbrda.bl.Cenvgeo$CCA$eig[3]/sum(dbrda.bl.Cenvgeo$CCA$eig),3)*100,"%)",sep="")
cent1.12<-colMeans(dbrda.bl.Cenvgeo$CCA$wa[ which(envclust == 1),1:2])
cent2.12<-colMeans(dbrda.bl.Cenvgeo$CCA$wa[ which(envclust == 2),1:2])
cent3.12<-colMeans(dbrda.bl.Cenvgeo$CCA$wa[ which(envclust == 3),1:2])

plot(dbrda.bl.Cenvgeo$CCA$wa[,1:2],type="n",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5), main="B | (Env + Geo)", xlab=XaxisLabels,ylab=YaxisLabels)
arrows(0,0,dbrda.bl.Cenvgeo$CCA$v[,1]*0.72,dbrda.bl.Cenvgeo$CCA$v[,2]*0.72,length=0.1,angle=15,
        cex=0.4,lwd=1.6,col="steelblue1")
text(dbrda.bl.Cenvgeo$CCA$v[,1:2]*0.77,labels=mod.rn,cex=0.7,lwd=2,col="steelblue1")
arrows(0,0,dbrda.bl.Cenvgeo$CCA$biplot[,1]*0.52,dbrda.bl.Cenvgeo$CCA$biplot[,2]*0.52,length=0.1,angle=15,
        cex=1.1,lwd=2,col="red3")
text(dbrda.bl.Cenvgeo$CCA$biplot[,1:2]*0.57,labels=rownames(dbrda.bl.Cenvgeo$CCA$biplot),cex=1.1,lwd=2,col="red3")
points(dbrda.bl.Cenvgeo$CCA$wa[,1:2],bg=col2.envclust[envclust],pch=22,cex=1.3,col="gray40")
legend(legend=c("G","A","F"),
       pch=22,pt.bg=col2.envclust,col=rep("gray40",6),pt.cex=rep(1.5,3),text.col="black",list(x=0.42,y=0.5),bty='n')
points(rbind(cent1.12,cent2.12,cent3.12),bg=col2.envclust,pch=22,cex=2.2,lwd=3.2,col="gray10")
hull<-ordihull(dbrda.bl.Cenvgeo, groups=envclust, col=col2.envclust,draw="none")
lines(hull[[1]]/6.7,col=col2.envclust[1],lwd=1.5)
lines(hull[[2]]/6.7,col=col2.envclust[2],lwd=1.5)
lines(hull[[3]]/6.7,col=col2.envclust[3],lwd=1.5)


dbrda.gr.Cenvgeo<-capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=gr.Cenvgeo.df,comm=wisconsin(abund.spp))

mod.rn = rownames(dbrda.gr.Cenvgeo$CCA$v)
mod.rn[which(rowSums(abs(dbrda.gr.Cenvgeo$CCA$v[,1:2]))<0.26)]=rep("",length(mod.rn[which(rowSums(abs(dbrda.gr.Cenvgeo$CCA$v[,1:2]))<0.26)]))

XaxisLabels<-paste("Axis 1 (",round(dbrda.gr.Cenvgeo$CCA$eig[1]/sum(dbrda.gr.Cenvgeo$CCA$eig),3)*100,"%)",sep="")
YaxisLabels<-paste("Axis 2 (",round(dbrda.gr.Cenvgeo$CCA$eig[2]/sum(dbrda.gr.Cenvgeo$CCA$eig),3)*100,"%)",sep="")
ZaxisLabels<-paste("Axis 3 (",round(dbrda.gr.Cenvgeo$CCA$eig[3]/sum(dbrda.gr.Cenvgeo$CCA$eig),3)*100,"%)",sep="")
cent1.12<-colMeans(dbrda.gr.Cenvgeo$CCA$wa[ which(envclust == 1),1:2])
cent2.12<-colMeans(dbrda.gr.Cenvgeo$CCA$wa[ which(envclust == 2),1:2])
cent3.12<-colMeans(dbrda.gr.Cenvgeo$CCA$wa[ which(envclust == 3),1:2])

plot(dbrda.gr.Cenvgeo$CCA$wa[,1:2],type="n",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5), main="G | (Env + Geo)", xlab=XaxisLabels,ylab=YaxisLabels)
arrows(0,0,dbrda.gr.Cenvgeo$CCA$v[,1]*0.72,dbrda.gr.Cenvgeo$CCA$v[,2]*0.72,length=0.1,angle=15,
        cex=0.4,lwd=1.6,col="steelblue1")
text(dbrda.gr.Cenvgeo$CCA$v[,1:2]*0.77,labels=mod.rn,cex=0.7,lwd=2,col="steelblue1")
arrows(0,0,dbrda.gr.Cenvgeo$CCA$biplot[,1]*0.52,dbrda.gr.Cenvgeo$CCA$biplot[,2]*0.52,length=0.1,angle=15,
        cex=1.1,lwd=2,col="red3")
text(dbrda.gr.Cenvgeo$CCA$biplot[,1:2]*0.57,labels=rownames(dbrda.gr.Cenvgeo$CCA$biplot),cex=1.1,lwd=2,col="red3")
points(dbrda.gr.Cenvgeo$CCA$wa[,1:2],bg=col2.envclust[envclust],pch=22,cex=1.3,col="gray40")
legend(legend=c("G","A","F"),
       pch=22,pt.bg=col2.envclust,col=rep("gray40",6),pt.cex=rep(1.5,3),text.col="black",list(x=0.42,y=0.5),bty='n')
points(rbind(cent1.12,cent2.12,cent3.12),bg=col2.envclust,pch=22,cex=2.2,lwd=3.2,col="gray10")
hull<-ordihull(dbrda.gr.Cenvgeo, groups=envclust, col=col2.envclust,draw="none")
lines(hull[[1]]/6.7,col=col2.envclust[1],lwd=1.5)
lines(hull[[2]]/6.7,col=col2.envclust[2],lwd=1.5)
lines(hull[[3]]/6.7,col=col2.envclust[3],lwd=1.5)




dbrda.bl.gr.Cenvgeo<-capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=bl.gr.Cenvgeo.df,comm=wisconsin(abund.spp))

mod.rn = rownames(dbrda.bl.gr.Cenvgeo$CCA$v)
mod.rn[which(rowSums(abs(dbrda.bl.gr.Cenvgeo$CCA$v[,1:2]))<0.26)]=rep("",length(mod.rn[which(rowSums(abs(dbrda.bl.gr.Cenvgeo$CCA$v[,1:2]))<0.26)]))

XaxisLabels<-paste("Axis 1 (",round(dbrda.bl.gr.Cenvgeo$CCA$eig[1]/sum(dbrda.bl.gr.Cenvgeo$CCA$eig),3)*100,"%)",sep="")
YaxisLabels<-paste("Axis 2 (",round(dbrda.bl.gr.Cenvgeo$CCA$eig[2]/sum(dbrda.bl.gr.Cenvgeo$CCA$eig),3)*100,"%)",sep="")
ZaxisLabels<-paste("Axis 3 (",round(dbrda.bl.gr.Cenvgeo$CCA$eig[3]/sum(dbrda.bl.gr.Cenvgeo$CCA$eig),3)*100,"%)",sep="")
cent1.12<-colMeans(dbrda.bl.gr.Cenvgeo$CCA$wa[ which(envclust == 1),1:2])
cent2.12<-colMeans(dbrda.bl.gr.Cenvgeo$CCA$wa[ which(envclust == 2),1:2])
cent3.12<-colMeans(dbrda.bl.gr.Cenvgeo$CCA$wa[ which(envclust == 3),1:2])

plot(dbrda.bl.gr.Cenvgeo$CCA$wa[,1:2],type="n",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5), main="(B + G) | (Env + Geo)", xlab=XaxisLabels,ylab=YaxisLabels)
arrows(0,0,dbrda.bl.gr.Cenvgeo$CCA$v[,1]*0.72,dbrda.bl.gr.Cenvgeo$CCA$v[,2]*0.72,length=0.1,angle=15,
        cex=0.4,lwd=1.6,col="steelblue1")
text(dbrda.bl.gr.Cenvgeo$CCA$v[,1:2]*0.77,labels=mod.rn,cex=0.7,lwd=2,col="steelblue1")
arrows(0,0,dbrda.bl.gr.Cenvgeo$CCA$biplot[,1]*0.52,dbrda.bl.gr.Cenvgeo$CCA$biplot[,2]*0.52,length=0.1,angle=15,
        cex=1.1,lwd=2,col="red3")
text(dbrda.bl.gr.Cenvgeo$CCA$biplot[,1:2]*0.57,labels=rownames(dbrda.bl.gr.Cenvgeo$CCA$biplot),cex=1.1,lwd=2,col="red3")
points(dbrda.bl.gr.Cenvgeo$CCA$wa[,1:2],bg=col2.envclust[envclust],pch=22,cex=1.3,col="gray40")
legend(legend=c("G","A","F"),
       pch=22,pt.bg=col2.envclust,col=rep("gray40",6),pt.cex=rep(1.5,3),text.col="black",list(x=0.42,y=0.5),bty='n')
points(rbind(cent1.12,cent2.12,cent3.12),bg=col2.envclust,pch=22,cex=2.2,lwd=3.2,col="gray10")
hull<-ordihull(dbrda.bl.gr.Cenvgeo, groups=envclust, col=col2.envclust,draw="none")
lines(hull[[1]]/6.7,col=col2.envclust[1],lwd=1.5)
lines(hull[[2]]/6.7,col=col2.envclust[2],lwd=1.5)
lines(hull[[3]]/6.7,col=col2.envclust[3],lwd=1.5)




dbrda.blgrenvCgeo<-capscale(dist.abund.spp~.+Condition(Geo,env.sig),data=blgrenvCgeo.df,comm=wisconsin(abund.spp))

mod.rn = rownames(dbrda.blgrenvCgeo$CCA$v)
mod.rn[which(rowSums(abs(dbrda.blgrenvCgeo$CCA$v[,1:2]))<0.26)]=rep("",length(mod.rn[which(rowSums(abs(dbrda.blgrenvCgeo$CCA$v[,1:2]))<0.26)]))

XaxisLabels<-paste("Axis 1 (",round(dbrda.blgrenvCgeo$CCA$eig[1]/sum(dbrda.blgrenvCgeo$CCA$eig),3)*100,"%)",sep="")
YaxisLabels<-paste("Axis 2 (",round(dbrda.blgrenvCgeo$CCA$eig[2]/sum(dbrda.blgrenvCgeo$CCA$eig),3)*100,"%)",sep="")
ZaxisLabels<-paste("Axis 3 (",round(dbrda.blgrenvCgeo$CCA$eig[3]/sum(dbrda.blgrenvCgeo$CCA$eig),3)*100,"%)",sep="")
cent1.12<-colMeans(dbrda.blgrenvCgeo$CCA$wa[ which(envclust == 1),1:2])
cent2.12<-colMeans(dbrda.blgrenvCgeo$CCA$wa[ which(envclust == 2),1:2])
cent3.12<-colMeans(dbrda.blgrenvCgeo$CCA$wa[ which(envclust == 3),1:2])

plot(dbrda.blgrenvCgeo$CCA$wa[,1:2],type="n",xlim=c(-0.5,0.5),ylim=c(-0.5,0.5), main="BG | (Env + Geo)", xlab=XaxisLabels,ylab=YaxisLabels)
arrows(0,0,dbrda.blgrenvCgeo$CCA$v[,1]*0.72,dbrda.blgrenvCgeo$CCA$v[,2]*0.72,length=0.1,angle=15,
        cex=0.4,lwd=1.6,col="steelblue1")
text(dbrda.blgrenvCgeo$CCA$v[,1:2]*0.77,labels=mod.rn,cex=0.7,lwd=2,col="steelblue1")
arrows(0,0,dbrda.blgrenvCgeo$CCA$biplot[,1]*0.52,dbrda.blgrenvCgeo$CCA$biplot[,2]*0.52,length=0.1,angle=15,
        cex=1.1,lwd=2,col="red3")
text(dbrda.blgrenvCgeo$CCA$biplot[,1:2]*0.57,labels=rownames(dbrda.blgrenvCgeo$CCA$biplot),cex=1.1,lwd=2,col="red3")
points(dbrda.blgrenvCgeo$CCA$wa[,1:2],bg=col2.envclust[envclust],pch=22,cex=1.3,col="gray40")
legend(legend=c("G","A","F"),
       pch=22,pt.bg=col2.envclust,col=rep("gray40",6),pt.cex=rep(1.5,3),text.col="black",list(x=0.42,y=0.5),bty='n')
points(rbind(cent1.12,cent2.12,cent3.12),bg=col2.envclust,pch=22,cex=2.2,lwd=3.2,col="gray10")
hull<-ordihull(dbrda.blgrenvCgeo, groups=envclust, col=col2.envclust,draw="none")
lines(hull[[1]]/6.7,col=col2.envclust[1],lwd=1.5)
lines(hull[[2]]/6.7,col=col2.envclust[2],lwd=1.5)
lines(hull[[3]]/6.7,col=col2.envclust[3],lwd=1.5)



