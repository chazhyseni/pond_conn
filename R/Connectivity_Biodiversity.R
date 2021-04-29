detach("package:raster")


grp = seq(1,72)

comm.dist.all = dist_groups(dist.abund.spp,seq(1,72))
comm.dist = c()
for(i in grp) comm.dist = c(comm.dist, mean(comm.dist.all$Distance[which(comm.dist.all$Group1 == i | comm.dist.all$Group2 == i)]))
comm.conn = 1/comm.dist

bl.dist.all = dist_groups(blDist,seq(1,72))
bl.dist = c()
for(i in grp) bl.dist = c(bl.dist, mean(bl.dist.all$Distance[which(bl.dist.all$Group1 == i | bl.dist.all$Group2 == i)]))
bl.conn = 1/bl.dist

bl.dist.within = dist_groups(blDist,envclust)
bl.dist = c()
for(i in grp) bl.dist = c(bl.dist, mean(bl.dist.within$Distance[which(subset(bl.dist.within, Label == paste("Within", envclust[i]))$Item1 == i | subset(bl.dist.within, Label == paste("Within", envclust[i]))$Item2 == i)]))
bl.conn.within = 1/bl.dist

gr.dist.all = dist_groups(grDist,seq(1,72))
gr.dist = c()
for(i in grp) gr.dist = c(gr.dist, mean(gr.dist.all$Distance[which(gr.dist.all$Group1 == i | gr.dist.all$Group2 == i)]))
gr.conn = 1/gr.dist

gr.dist.within = dist_groups(grDist,envclust)
gr.dist = c()
for(i in grp) gr.dist = c(gr.dist, mean(gr.dist.within$Distance[which(subset(gr.dist.within, Label == paste("Within", envclust[i]))$Item1 == i | subset(gr.dist.within, Label == paste("Within", envclust[i]))$Item2 == i)]))
gr.conn.within = 1/gr.dist

blgr.dist.all = dist_groups(blgrDist,seq(1,72))
blgr.dist = c()
for(i in grp) blgr.dist = c(blgr.dist, mean(blgr.dist.all$Distance[which(blgr.dist.all$Group1 == i | blgr.dist.all$Group2 == i)]))
blgr.conn = 1/blgr.dist

blgr.dist.within = dist_groups(blgrDist,envclust)
blgr.dist = c()
for(i in grp) blgr.dist = c(blgr.dist, mean(blgr.dist.within$Distance[which(subset(blgr.dist.within, Label == paste("Within", envclust[i]))$Item1 == i | subset(blgr.dist.within, Label == paste("Within", envclust[i]))$Item2 == i)]))
blgr.conn.within = 1/blgr.dist

conn.class.mat <-
cbind(
  rbind(
    mean(bl.conn[which(envclust==1)]),
    median(bl.conn[which(envclust==1)]),
    mean(gr.conn[which(envclust==1)]),
    median(gr.conn[which(envclust==1)]),
    mean(blgr.conn[which(envclust==1)]),
    median(blgr.conn[which(envclust==1)])
  ),
  rbind(
    mean(bl.conn[which(envclust==2)]),
    median(bl.conn[which(envclust==2)]),
    mean(gr.conn[which(envclust==2)]),
    median(gr.conn[which(envclust==2)]),
    mean(blgr.conn[which(envclust==2)]),
    median(blgr.conn[which(envclust==2)])
  ),
  rbind(
    mean(bl.conn[which(envclust==3)]),
    median(bl.conn[which(envclust==3)]),
    mean(gr.conn[which(envclust==3)]),
    median(gr.conn[which(envclust==3)]),
    mean(blgr.conn[which(envclust==3)]),
    median(blgr.conn[which(envclust==3)])
  )
)
colnames(conn.class.mat) <- c("Grass", "Artif.Surf", "Forest")
rownames(conn.class.mat) <- c("Bl.Avg", "Bl.Med", "Gr.Avg", "Gr.Med", "BlGr.Avg", "BlGr.Med")



conn.data <- cbind(bl.conn, gr.conn, blgr.conn, envclust)
colnames(conn.data) <- c("Blue", "Green", "Blue+Green", "Class")

conn.data <- as_tibble(conn.data)

conn.long <- conn.data %>%
  pivot_longer(-Class, names_to = "variables", values_to = "value")

conn.long %>%
  group_by(variables, Class) %>%
  summarise(
    n = n(),
    mean = mean(value),
    sd = sd(value)
  ) %>%
  ungroup()

stat.test <-conn.long %>%
  group_by(variables) %>%
  t_test(value ~ Class, p.adjust.method = "fdr")
# Remove unnecessary columns and display the outputs
stat.test %>% select(-.y., -statistic, -df)


par(mfrow=c(1,1),fg="gray50",pty='m',bty='o',mar=c(4.5,4.5,4.5,1),cex.main=1.3,cex.axis=1.1,cex.lab=1.2)

conn.plot <- ggboxplot(
  conn.long, x = "Class", y = "value",
  fill = "Class", palette = col2.envclust, legend = "none",
  ggtheme = theme_pubr(border = FALSE)
  ) 

conn.plot + 
facet_wrap(~variables) + 
labs(title = "", x = "Pond Class", y = "Mean Connectivity", color = "Pond Class\n")

stat.test <- stat.test %>% add_xy_position(x = "Class")
conn.plot + stat_pvalue_manual(stat.test, label = "p.adj.signif")




pvals_div_conn_list <- list()

for(i in 1:3){
	H.clust <- H[which(envclust==i)]
	S.clust <- S[which(envclust==i)]
	J.clust <- J[which(envclust==i)]
	bl.conn.clust <- bl.conn[which(envclust==i)]
	gr.conn.clust <- gr.conn[which(envclust==i)]
	blgr.conn.clust <- blgr.conn[which(envclust==i)]

    if(length(which(J.clust==Inf)) != 0){    
        H.clust <- H.clust[-which(J.clust==Inf)]
        S.clust <- S.clust[-which(J.clust==Inf)]
        bl.conn.clust <- bl.conn.clust[-which(J.clust==Inf)]
        gr.conn.clust <- gr.conn.clust[-which(J.clust==Inf)]
        blgr.conn.clust <- blgr.conn.clust[-which(J.clust==Inf)] 
    }
    if(length(which(J.clust==Inf)) != 0) J.clust <- J.clust[-which(J.clust==Inf)]

	H.bl <- summary(lm(H.clust ~ bl.conn.clust))
	S.bl <- summary(lm(S.clust ~ bl.conn.clust))
	J.bl <- summary(lm(J.clust ~ bl.conn.clust))

	H.gr <- summary(lm(H.clust ~ gr.conn.clust))
	S.gr <- summary(lm(S.clust ~ gr.conn.clust))
	J.gr <- summary(lm(J.clust ~ gr.conn.clust))

	H.blgr <- summary(lm(H.clust ~ blgr.conn.clust))
	S.blgr <- summary(lm(S.clust ~ blgr.conn.clust))
	J.blgr <- summary(lm(J.clust ~ blgr.conn.clust))

	pvals_div_conn <- matrix(c(
	H.bl$coefficients[2,4],
	S.bl$coefficients[2,4],
	J.bl$coefficients[2,4],
	H.gr$coefficients[2,4],
	S.gr$coefficients[2,4],
	J.gr$coefficients[2,4],
	H.blgr$coefficients[2,4],
	S.blgr$coefficients[2,4],
	J.blgr$coefficients[2,4]),nrow=3,ncol=3,byrow=T)
	colnames(pvals_div_conn) <- c("H", "S", "J")
	rownames(pvals_div_conn) <- c("Bl", "Gr", "BlGr")
        
        pvals_div_conn_list[[i]] <- pvals_div_conn
}        
p_div_conn <- list()
p_div_conn$Grass <- pvals_div_conn_list[[1]] 
p_div_conn$Artif <- pvals_div_conn_list[[2]] 
p_div_conn$Forest <- pvals_div_conn_list[[3]] 


plot(TukeyHSD(aov(bl.conn~as.factor(envclust))))
plot(TukeyHSD(aov(gr.conn~as.factor(envclust))))
plot(TukeyHSD(aov(blgr.conn~as.factor(envclust))))


gg.allenv<-as.data.frame(cbind(envclust,S,H,J,bl.conn,bl.conn.within,blgr.conn,blgr.conn.within))
colnames(gg.allenv)<-c("cluster","S","H","J","bl.conn","bl.conn.within","blgr.conn","blgr.conn.within")
gg.allenv[,"cluster"]<-gsub("1","Grassland",gg.allenv[,"cluster"])
gg.allenv[,"cluster"]<-gsub("2","Artificial",gg.allenv[,"cluster"])
gg.allenv[,"cluster"]<-gsub("3","Forest",gg.allenv[,"cluster"])




ggplot(gg.allenv, aes(S, blgr.conn, colour=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", x = "Species Richness", y = "Blue+Green Connectivity", color = "Pond Class\n")

ggplot(gg.allenv[-which(J==Inf),], aes(J, blgr.conn, color=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", x = "Species Evenness", y = "Blue+Green Connectivity", color = "Pond Class\n")



ggplot(gg.allenv, aes(blgr.conn, S, colour=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", y = "Species Richness", x = "Blue+Green Connectivity", color = "Pond Class\n")

ggplot(gg.allenv[-which(J==Inf),], aes(blgr.conn, J, color=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", y = "Species Evenness", x = "Blue+Green Connectivity", color = "Pond Class\n")





ggplot(gg.allenv, aes(S, bl.conn, colour=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", x = "Species Richness", y = "Blue Connectivity", color = "Pond Class\n")

ggplot(gg.allenv[-which(J==Inf),], aes(J, bl.conn, color=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", x = "Species Evenness", y = "Blue Connectivity", color = "Pond Class\n")



ggplot(gg.allenv, aes(bl.conn, S, colour=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", y = "Species Richness", x = "Blue Connectivity", color = "Pond Class\n")

ggplot(gg.allenv[-which(J==Inf),], aes(bl.conn, J, color=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", y = "Species Evenness", x = "Blue Connectivity", color = "Pond Class\n")


