detach("package:raster")
library(tidyverse)

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

conn.plot <- ggboxplot(conn.long, x = "Class", y = "value",
                       fill = "Class", palette = col2.envclust, legend = "none",
                       ggtheme = theme_pubr(border = FALSE)) + 
             facet_wrap(~variables) + 
             labs(title = "", x = "Pond Class", y = "Mean Connectivity", color = "Pond Class\n")

stat.test <- stat.test %>% add_xy_position(x = "Class")

conn.plot + stat_pvalue_manual(stat.test, label = "p.adj.signif")




dendf_div_conn_list <- list()
f_div_conn_list <- list()
rsq_div_conn_list <- list()
p_div_conn_list <- list()

for(i in 1:3){
	H.clust <- H[which(envclust==i)]
	S.clust <- S[which(envclust==i)]
	J.clust <- J[which(envclust==i)]
	bl.conn.clust <- bl.conn[which(envclust==i)]
	gr.conn.clust <- gr.conn[which(envclust==i)]
	blgr.conn.clust <- blgr.conn[which(envclust==i)]

    if(length(which(!is.finite(J.clust))) != 0){    
        H.clust <- H.clust[-which(!is.finite(J.clust))]
        S.clust <- S.clust[-which(!is.finite(J.clust))]
        bl.conn.clust <- bl.conn.clust[-which(!is.finite(J.clust))]
        gr.conn.clust <- gr.conn.clust[-which(!is.finite(J.clust))]
        blgr.conn.clust <- blgr.conn.clust[-which(!is.finite(J.clust))] 
    }
    if(length(which(!is.finite(J.clust))) != 0) J.clust <- J.clust[-which(!is.finite(J.clust))]

	H.bl <- summary(lm(H.clust ~ bl.conn.clust))
	S.bl <- summary(lm(S.clust ~ bl.conn.clust))
	J.bl <- summary(lm(J.clust ~ bl.conn.clust))

	H.gr <- summary(lm(H.clust ~ gr.conn.clust))
	S.gr <- summary(lm(S.clust ~ gr.conn.clust))
	J.gr <- summary(lm(J.clust ~ gr.conn.clust))

	H.blgr <- summary(lm(H.clust ~ blgr.conn.clust))
	S.blgr <- summary(lm(S.clust ~ blgr.conn.clust))
	J.blgr <- summary(lm(J.clust ~ blgr.conn.clust))

	###df
	dendf_div_conn <- matrix(c(
		H.bl$fstatistic[3],
		S.bl$fstatistic[3],
		J.bl$fstatistic[3],
		H.gr$fstatistic[3],
		S.gr$fstatistic[3],
		J.gr$fstatistic[3],
		H.blgr$fstatistic[3],
		S.blgr$fstatistic[3],
		J.blgr$fstatistic[3]),nrow=3,ncol=3,byrow=T)
	  colnames(dendf_div_conn) <- c("H", "S", "J")
	  rownames(dendf_div_conn) <- c("Bl", "Gr", "BlGr")
	  dendf_div_conn_list[[i]] <- dendf_div_conn

	###F statistic
	f_div_conn <- matrix(c(
		H.bl$fstatistic[1],
		S.bl$fstatistic[1],
		J.bl$fstatistic[1],
		H.gr$fstatistic[1],
		S.gr$fstatistic[1],
		J.gr$fstatistic[1],
		H.blgr$fstatistic[1],
		S.blgr$fstatistic[1],
		J.blgr$fstatistic[1]),nrow=3,ncol=3,byrow=T)
	  colnames(f_div_conn) <- c("H", "S", "J")
	  rownames(f_div_conn) <- c("Bl", "Gr", "BlGr")
	  f_div_conn_list[[i]] <- f_div_conn

	###R.sq
	rsq_div_conn <- matrix(c(
		H.bl$r.squared,
		S.bl$r.squared,
		J.bl$r.squared,
		H.gr$r.squared,
		S.gr$r.squared,
		J.gr$r.squared,
		H.blgr$r.squared,
		S.blgr$r.squared,
		J.blgr$r.squared),nrow=3,ncol=3,byrow=T)
	  colnames(rsq_div_conn) <- c("H", "S", "J")
	  rownames(rsq_div_conn) <- c("Bl", "Gr", "BlGr")
	  rsq_div_conn_list[[i]] <- rsq_div_conn

	###p-val
	p_div_conn <- matrix(c(
		H.bl$coefficients[2,4],
		S.bl$coefficients[2,4],
		J.bl$coefficients[2,4],
		H.gr$coefficients[2,4],
		S.gr$coefficients[2,4],
		J.gr$coefficients[2,4],
		H.blgr$coefficients[2,4],
		S.blgr$coefficients[2,4],
		J.blgr$coefficients[2,4]),nrow=3,ncol=3,byrow=T)
	  colnames(p_div_conn) <- c("H", "S", "J")
	  rownames(p_div_conn) <- c("Bl", "Gr", "BlGr")
	  p_div_conn_list[[i]] <- p_div_conn
}

dendf_div_conn <- list()
dendf_div_conn$Grass <- dendf_div_conn_list[[1]]
dendf_div_conn$Artif <- dendf_div_conn_list[[2]]
dendf_div_conn$Forest <- dendf_div_conn_list[[3]]

f_div_conn <- list()
f_div_conn$Grass <- f_div_conn_list[[1]]
f_div_conn$Artif <- f_div_conn_list[[2]]
f_div_conn$Forest <- f_div_conn_list[[3]]

rsq_div_conn <- list()
rsq_div_conn$Grass <- rsq_div_conn_list[[1]]
rsq_div_conn$Artif <- rsq_div_conn_list[[2]]
rsq_div_conn$Forest <- rsq_div_conn_list[[3]]

p_div_conn <- list()
p_div_conn$Grass <- p_div_conn_list[[1]]
p_div_conn$Artif <- p_div_conn_list[[2]]
p_div_conn$Forest <- p_div_conn_list[[3]]


plot(TukeyHSD(aov(bl.conn~as.factor(envclust))))
plot(TukeyHSD(aov(gr.conn~as.factor(envclust))))
plot(TukeyHSD(aov(blgr.conn~as.factor(envclust))))


gg.allenv<-as.data.frame(cbind(envclust,S,H,J,bl.conn,bl.conn.within,blgr.conn,blgr.conn.within))
colnames(gg.allenv)<-c("cluster","S","H","J","bl.conn","bl.conn.within","blgr.conn","blgr.conn.within")
gg.allenv[,"cluster"]<-gsub("1","Grassland",gg.allenv[,"cluster"])
gg.allenv[,"cluster"]<-gsub("2","Artificial",gg.allenv[,"cluster"])
gg.allenv[,"cluster"]<-gsub("3","Forest",gg.allenv[,"cluster"])





ggplot(gg.allenv[-which(!is.finite(J)),], aes(S, bl.conn, colour=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", x = "Species Richness", y = "Blue Connectivity", color = "Pond Class\n")

ggplot(gg.allenv[-which(!is.finite(J)),], aes(J, bl.conn, color=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", x = "Species Evenness", y = "Blue Connectivity", color = "Pond Class\n")


ggplot(gg.allenv[-which(!is.finite(J)),], aes(bl.conn, S, colour=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", y = "Species Richness", x = "Blue Connectivity", color = "Pond Class\n")

ggplot(gg.allenv[-which(!is.finite(J)),], aes(bl.conn, J, color=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", y = "Species Evenness", x = "Blue Connectivity", color = "Pond Class\n")




ggplot(gg.allenv[-which(!is.finite(J)),], aes(S, blgr.conn, colour=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", x = "Species Richness", y = "Blue+Green Connectivity", color = "Pond Class\n")

ggplot(gg.allenv[-which(!is.finite(J)),], aes(J, blgr.conn, color=factor(cluster))) + 
geom_point(aes(size=0.9)) + 
stat_smooth(method='lm',formula=y~x,lwd=2) + 
facet_wrap(~cluster) + 
scale_color_manual(values = col2.envclust[c(2,3,1)]) + 
theme_clean(base_size=18) + 
labs(title = "", x = "Species Evenness", y = "Blue+Green Connectivity", color = "Pond Class\n")


grid.arrange(nrow = 2, ncol = 2,
	ggplot(gg.allenv[-which(!is.finite(J)),], aes(blgr.conn, S, colour=factor(cluster))) +
	geom_point(aes(size=0.4)) +
	stat_smooth(method='lm',formula=y~x,lwd=2) +
	facet_wrap(~cluster) +
	scale_color_manual(values = col2.envclust[c(2,3,1)]) +
	theme_clean(base_size=15) +
	theme(plot.background = element_rect(color = "white")) +
	theme(panel.spacing = unit(1.2, "lines")) +
	theme(legend.position="none") +
	labs(title = "", y = "Richness", x = "Blue+Green Connectivity", color = "Pond Class\n")
	,
	ggplot(gg.allenv[-which(!is.finite(J)),], aes(blgr.conn, J, color=factor(cluster))) +
	geom_point(aes(size=0.4)) +
	stat_smooth(method='lm',formula=y~x,lwd=2) +
	facet_wrap(~cluster) +
	scale_color_manual(values = col2.envclust[c(2,3,1)]) +
	theme_clean(base_size=15) +
	theme(plot.background = element_rect(color = "white")) +
	theme(panel.spacing = unit(1.2, "lines")) +
	theme(legend.position="none") +
	labs(title = "", y = "Evenness", x = "Blue+Green Connectivity", color = "Pond Class\n")
)
