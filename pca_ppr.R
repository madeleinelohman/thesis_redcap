
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
library(tidyverse)
library(terra)

setwd("/Users/madelienelohman/Desktop/thesis_redcap")


load("map_dat.RData")
load("combined_ppr.RData")
load("banding_data/band_locs.RData")

### Prep data
all.new <- st_drop_geometry(all)
for(i in 1:ncol(all.new)){
  all.new[,i] <- as.numeric(unlist(all.new[,i]))
}
colnames(all.new)
all.new <- all.new[,-c(1,7,10, 17)]
colnames(all.new)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run PCA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### PCA
pc.res <- prcomp(~., data=all.new, scale.=TRUE, center=T)

### Look at summary
summ <- summary(pc.res)
summ

### Initial plots
fviz_eig(pc.res, addlabels = TRUE) # Eigenvalues (What explains the most variance)
# fviz_pca_var(pc.res, axes=c(1,2)) # See how variables are distributed along PC axes
# fviz_pca_biplot(pc.res) # Biplot (How do the eigenvector looked compared to data points)

### Correlation of variables and PCs
# var <- get_pca_var(pc.res)
# corrplot(var$cor, is.corr=FALSE)
# 
# ### Contribution of variables to each PC
# fviz_contrib(pc.res, choice = "var", axes = 1, top = 10)
# fviz_contrib(pc.res, choice = "var", axes = 2, top = 10)

# fviz_pca_var(pc.res, col.var = "contrib",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
# )

### Can the variables be grouped?
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
# res.km <- kmeans(var$coord, centers = 3, nstart = 25)
# grp <- as.factor(res.km$cluster)
# # Color variables by groups
# fviz_pca_var(pc.res, col.var = grp,
#              palette = c("#0073C2FF", "#EFC000FF", "#868686FF", "#FC4E07",
#                          "green", "purple"),
#              legend.title = "Cluster")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run Redcap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Put first 3 PCs into data frame and combine with geographic data
dat <- pc.res$x[,1:4]
dat <- cbind(counties.ppr, dat)

### Number of clusters to create
n.clust = 30

### Neighborhood weight matrices
queen_w <- queen_weights(dat) 

### Specific data values we want
data <- dat[,grep("PC",colnames(dat))] 


### Run Redcap!!
cr <- redcap(n.clust, queen_w, data, "fullorder-completelinkage") 
cr

#~~~~~~~~~~~~~~~~~~
# Plotting
#~~~~~~~~~~~~~~~~~~
### Get ready to plot
# Put clusters into data frame 
dat$pred <- cr$Clusters
dat$pred <- as.factor(dat$pred)

# Put together counties in the same cluster
new <- dat %>%
  group_by(pred) %>%
  summarize(cat = first(pred))

# Plot colors for each cluster
col.samples <- sample(1:n.clust, n.clust) # Randomize color assignments
# divergingx_palettes(plot = TRUE)
cols <- divergingx_hcl(n.clust, "Spectral")[col.samples]
new$cols <- cols[match(new$pred, col.samples)]

### Plot!!
ggplot(new) +
  geom_sf(aes(fill=pred))+
  xlim(st_bbox(grd2)[c(1,3)]) + ylim(st_bbox(grd2)[c(2, 4)]) +
  geom_sf(data=ppr_states, fill=NA, color="grey50", size=0.25) +
  scale_fill_manual(values=cols, aesthetics="fill") +
  theme_classic() +
  theme(legend.position="none")

ggplot(new) +
  geom_sf(aes(fill=pred))+
  xlim(st_bbox(grd2)[c(1,3)]) + ylim(st_bbox(grd2)[c(2, 4)]) +
  geom_sf(data=ppr_states, fill=NA, color="grey50", size=0.25) +
  geom_sf(data=rels.n, size=0.85) +
  scale_fill_manual(values=cols, aesthetics="fill") +
  theme_classic() +
  theme(legend.position="none")


save.image(file="pca_res.RData")


