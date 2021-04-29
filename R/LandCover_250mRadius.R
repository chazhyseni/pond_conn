
#setwd("~/Data")

lc17 = raster(paste0(data,"S2GLC_Europe_2017_10m_Stkhlm.tif"))

## create 250-m buffer
xy_laea = spTransform(xy, proj4string(lc17))
gbf_laea = rgeos::gBuffer(xy_laea, width = 250, quadsegs = 250L)
gbf = spTransform(gbf_laea, CRS(proj4string(xy)))

## extract values and calculate percentage
#lc17_samp_250m_radius = extract(lc17, xy_laea, buffer=250, cellnumbers=T)
lc17_samp_250m_radius = extract(lc17, xy_laea, buffer=250)
feat = sort(unique(unlist(lc17_samp_250m_radius)))
lc17_samp_250m_pct_artsurf = unlist(lapply(lc17_samp_250m_radius, function(x) if (!is.null(x)) 100*length(which(x==feat[1]))/length(x) else NA ))
lc17_samp_250m_pct_cultiv = unlist(lapply(lc17_samp_250m_radius, function(x) if (!is.null(x)) 100*length(which(x==feat[2]))/length(x) else NA ))
lc17_samp_250m_pct_forest = unlist(lapply(lc17_samp_250m_radius, function(x) if (!is.null(x)) 100*length(which(x==feat[3] | x==feat[4]))/length(x) else NA ))
lc17_samp_250m_pct_grassland = unlist(lapply(lc17_samp_250m_radius, function(x) if (!is.null(x)) 100*length(which(x==feat[5]))/length(x) else NA ))
lc17_samp_250m_pct_wetland = unlist(lapply(lc17_samp_250m_radius, function(x) if (!is.null(x)) 100*length(which(x==feat[6] | x==feat[7]))/length(x) else NA ))
lc17_samp_250m_pct_natsurf = unlist(lapply(lc17_samp_250m_radius, function(x) if (!is.null(x)) 100*length(which(x==feat[8]))/length(x) else NA ))
lc17_samp_250m_pct_wat = unlist(lapply(lc17_samp_250m_radius, function(x) if (!is.null(x)) 100*length(which(x==feat[9]))/length(x) else NA ))

