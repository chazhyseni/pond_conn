####RUN AFTER Input.R#####



####Fst in Community Ecology#####

##Fst Spp##

len.col <- length(colnames(abund.spp))
len.row <- length(rownames(abund.spp))
rs.abund <- rowSums(abund.spp)
prop.spp <- NULL
for(i in 1:len.row) prop.spp <- rbind(prop.spp, abund.spp[i,]/rs.abund[i])


n.spp <- length(colnames(prop.spp))
n.comm <- length(rownames(prop.spp))
spp <- seq(1,n.spp)
comm <- seq(1,n.comm)
var.spp <- c()
fst.spp <- c()

for(j in spp){
   for(i in comm){
	var.spp <- c(var.spp, (prop.spp[i,j] - mean(prop.spp[,j]))^2)
   }
   var.spp <- sum(var.spp)/n.comm
   var.tot <- mean(prop.spp[,j])*(1 - mean(prop.spp[,j]))
   fst.spp[j] <- var.spp/var.tot 
   var.spp <- c()
}


##Fst Comm##

n.spp <- length(colnames(prop.spp))
n.comm <- length(rownames(prop.spp))
spp <- seq(1,n.spp)
comm <- seq(1,n.comm)
var.spp <- c()
var.k <- c()

for(j in spp){
   for(i in comm){
	var.spp <- c(var.spp, (prop.spp[i,j] - mean(prop.spp[,j]))^2)
   }
   var.k <- c(var.k, sum(var.spp)/n.comm)
   var.tot <- c(var.tot, mean(prop.spp[,j])*(1 - mean(prop.spp[,j])))
   var.spp <- c()
}


fst.k.env <- c()
no.k <- length(unique(envclust))

for(k in 1:no.k){
   
   comm.k <- which(envclust==k)
   prop.k <- prop.spp[comm.k,]
   spp.k <- which(colSums(prop.k) != 0)
   fst.k.env[k] <- sum(var.k[spp.k])/sum(var.tot[spp.k])
}



names(fst.spp) <- colnames(prop.spp)
sort(fst.spp)


####Diversity Stats###

adiprt.spp.1 <- adipart(abund.spp, index="simpson", nsimul=100)
adiprt.spp.3 <- adipart(abund.spp ~ ., as.data.frame(cbind(seq(1,72),envclust)), index="simpson", nsimul=100)
mltiprt.spp <- multipart(abund.spp ~ ., as.data.frame(cbind(seq(1,72),envclust)), index="renyi", scales=1, nsimul=100, relative=TRUE)

b.disper <- betadisper(dist.abund.spp,envclust,type="centroid")
anova(b.disper)
permutest(b.disper, pairwise = TRUE, permutations = 999)
## Tukey's Honest Significant Differences
(bdisp.HSD <- TukeyHSD(b.disper))
plot(bdisp.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
plot(b.disper)

## with data ellipses instead of hulls
plot(b.disper, pch=16:18, col=col2.envclust, ellipse = TRUE, hull = FALSE, conf = 0.75) # 75% data ellipse

H <- diversity(abund.spp)
simp <- diversity(abund.spp, "simpson")
invsimp <- diversity(abund.spp, "inv")

## Species richness (S) and Pielou's evenness (J):
S <- specnumber(abund.spp) ## rowSums(abund.spp > 0) does the same...
J <- H/log(S)

## beta diversity defined as gamma/alpha - 1:
alpha.env <- tapply(specnumber(abund.spp), envclust, mean)
gamma.env <- specnumber(abund.spp, envclust)
beta.env <- gamma.env/alpha.env - 1

H.k.env <- tapply(H,as.factor(envclust),mean)
simp.k.env <- tapply(simp,as.factor(envclust),mean)
invsimp.k.env <- tapply(invsimp,as.factor(envclust),mean)
S.k.env <- specnumber(abund.spp, envclust)
J.k.env <- H.k.env/log(S.k.env)


summary(lm(H[which(S != 1)] ~ TOC[which(S != 1)], data = as.data.frame(env)))
summary(lm(H[which(S != 1)] ~ totN[which(S != 1)], data = as.data.frame(env)))
summary(lm(H[which(S != 1)] ~ totP[which(S != 1)], data = as.data.frame(env)))

summary(lm(J[which(S != 1)] ~ TOC[which(S != 1)], data = as.data.frame(env)))
summary(lm(J[which(S != 1)] ~ totN[which(S != 1)], data = as.data.frame(env)))
summary(lm(J[which(S != 1)] ~ totP[which(S != 1)], data = as.data.frame(env)))




div.k <- rbind(alpha.env, gamma.env, beta.env, H.k.env, J.k.env, fst.k.env)
rownames(div.k) <- c("alpha", "gamma", "beta", "H", "J", "Fst")
div.k



#beta div - turnover and nestedness
bprt.grs <- beta.multi.abund(betapart.core.abund(abund.spp[which(envclust==1),]),index.family="bray")
bprt.art <- beta.multi.abund(betapart.core.abund(abund.spp[which(envclust==2),]),index.family="bray")
bprt.for <- beta.multi.abund(betapart.core.abund(abund.spp[which(envclust==3),]),index.family="bray")



