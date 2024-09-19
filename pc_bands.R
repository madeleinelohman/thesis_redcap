#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries and set up
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(clusterCrit)
library(colorspace)
library(corrplot)
library(ggfortify)
library(ggbiplot)
library(factoextra)
library(devtools)
library(rgeoda)
library(sf)
library(spatstat)
library(tidyverse)
library(terra)

setwd("/Users/madelienelohman/Desktop/thesis_redcap")

load("pca_res.RData")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assess band clustering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

want <- st_intersects(rels, new)
rels$new.clust <- unlist(want)

rels.locs <- rels %>%
  group_by(id, new.clust, BANDING_YEAR) %>%
  summarize(n = n())


new$area <- as.numeric(st_area(new))
rels.locs$area <- new$area[match(rels.locs$new.clust, new$pred)]

dens <- rels.locs %>%
  group_by(new.clust) %>%
  summarize(dens=n()/first(area))
# dens <- rels.locs %>%
#   group_by(new.clust) %>%
#   summarize(dens=first(area)/n())


new$Density <- 0
new$Density[match(dens$new.clust, new$pred)] <- dens$dens
new$dens.log <- log(new$Density)

ggplot(new) +
  geom_sf(aes(fill=Density))+
  xlim(st_bbox(grd2)[c(1,3)]) + ylim(st_bbox(grd2)[c(2, 4)]) +
  geom_sf(data=ppr_states, fill=NA, color="grey50", size=0.25) +
  geom_sf(data=rels.n, size=0.85) +
  scale_fill_continuous_sequential("Heat") +
  theme_classic() 

ggplot(new) +
  geom_sf(aes(fill=dens.log))+
  xlim(st_bbox(grd2)[c(1,3)]) + ylim(st_bbox(grd2)[c(2, 4)]) +
  geom_sf(data=ppr_states, fill=NA, color="grey50", size=0.25) +
  geom_sf(data=rels.n, size=0.85) +
  scale_fill_continuous_sequential("Heat") +
  theme_classic() 


#~~~~~~~~~~~~~~~
# By year
#~~~~~~~~~~~~~~~

#### How dense?
dens.y <- rels.locs %>%
  group_by(new.clust,BANDING_YEAR) %>%
  summarize(dens=n()/first(area))
dens.y$dens.log <- log(dens.y$dens)

for(i in unique(dens.y$new.clust)){
  want <- dens.y[which(dens.y$new.clust == i),]
  
  p <- ggplot(want, aes(x=BANDING_YEAR, y=dens)) +
    geom_point()
  
  print(p)
  Sys.sleep(0.4)
}

### How many locatins every year?
rels.locs2 <- rels.locs %>%
  group_by(new.clust, id) %>%
  summarize(avg=mean(n()))
plot(rels.locs2["avg"])  

new$mu.rels <- 0
new$mu.rels[match(rels.locs2$new.clust, new$pred)] <- rels.locs2$avg  
plot(new["mu.rels"])




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Are they clustered?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

xy <- unique(st_coordinates(rels.locs))
coords <- as.ppp(xy, W=as.owin(list(xrange=c(min(xy[,1]), max(xy[,1])),yrange=c(min(xy[,2]), max(xy[,2])))))


shoot <- envelope(coords, fun=Gest, savefuns = T)
shoot
summary(shoot)
plot(shoot)
mad.test(shoot)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Correlations with band density and PC score?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clusts <- new$geometry
region.pc <- matrix(NA, length(clusts), 5)

for(i in 1:5){
  region.pc[,i] <- unlist(st_drop_geometry(st_interpolate_aw(data[paste0("PC",i)], clusts, extensive=F)))
}


hist(new$Density)
hist(new$dens.log)

new$dens.log[which(is.infinite(new$dens.log))] <- NA

shoot <- lm(new$dens.log ~ region.pc)
summary(shoot)

plot(shoot)





