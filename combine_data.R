
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Notes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Agriculture has to be subset in time in it's own file because it's so large



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(sf)
library(tidyverse)
library(terra)


setwd("/Users/madelienelohman/Documents/GitHub/thesis_redcap")
load("map_dat.RData")
#load("resolve_counties_strata.RData")

#counties.strata <- all.new

#~~~~~~~~~~~~~~~~~~~~~~~
# Load in data
#~~~~~~~~~~~~~~~~~~~~~~~
data.source <- "ppr_data"

for(i in 1:length(list.files(data.source))){
  load(paste0(data.source, "/", list.files(data.source)[i]))
}
years.want <- 1993:2023

strata.big <- st_union(strata)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in and manipulate different data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~
# Ponds
#~~~~~~~~~~~~~~~
### Make ponds into a long format
ponds <- p_cts %>%
  pivot_longer(cols=starts_with("yr"),
               names_to="year",
               values_to="ponds")

### Make years into a numeric
ponds$year <- as.numeric(gsub("yr","",ponds$year))

### Standardize by year
ponds <- ponds %>%
  group_by(year) %>%
  mutate(scaledPonds=scale(ponds))

### Get mean
ponds.want <- ponds %>%
  group_by(COUNTY_CODE) %>%
  summarize(ponds=mean(scaledPonds, na.rm=T))

ponds.want$COUNTY_CODE <- as.character(ponds.want$COUNTY_CODE)

#~~~~~~~~~~~~~~~
# Agriculture
#~~~~~~~~~~~~~~~
### Filter and standardize
ag <- all.ag %>%
  filter(YEAR %in% years.want) %>%
  group_by(YEAR) %>%
  mutate(sCrop=scale(CROP_ACRES),
         sFalw=scale(FALLOW_ACRES),
         sPast=scale(PASTURE_ACRES),
         sHay=scale(HAY_ACRES))

### Get mean
ag <- ag %>%
  group_by(COUNTY_CODE) %>%
  summarize(crop=mean(sCrop, na.rm=T),
            falw=mean(sFalw, na.rm=T),
            past=mean(sPast, na.rm=T),
            hay=mean(sHay, na.rm=T),) %>%
  st_drop_geometry()


#~~~~~~~~~~~~~~~
# Seasonality
#~~~~~~~~~~~~~~~
colnames(season)[1] <- "COUNTY_CODE"
season <- season[,-c(2)]
colnames(season)[2] <- "season"
season1 <- season %>%
  mutate(sSeason=scale(season)) %>%
  st_drop_geometry()
season1 <- season1[,-c(2)]


#~~~~~~~~~~~~~~~
# Land cover
#~~~~~~~~~~~~~~~
colnames(lc)[1] <- "COUNTY_CODE"
lc1 <- lc[,-c(4)]
lc1 <- lc1 %>%
  pivot_wider(names_from=class,
              values_from=prop) %>%
  st_drop_geometry()
#lc1[,2:ncol(lc1)] <- scale(lc1[,2:ncol(lc1)])


#~~~~~~~~~~~~~~~
# Combine
#~~~~~~~~~~~~~~~
all <- left_join(ponds.want, ag, by="COUNTY_CODE")
all <- left_join(all, season1, by="COUNTY_CODE")
all <- left_join(all, lc1, by="COUNTY_CODE")
# ggplot(all) + geom_sf()

### Replace NA with 0
#all <- all %>% replace(is.na(.), 0)
### Get geometry outside of data columns
all <- all[,c(1, 3, 2, 4:ncol(all))]
all <- all[,1:16]
### Get rid of extra cropland (FIGURE OUT IF WANT TO DO THIS)

### Intersect with strata
# all.s <- st_intersection(all, strata) %>%
#   mutate(intersect_area = st_area(.),
#          stratum=stratum)

# ggplot(all, aes(fill=stratum)) + 
#   geom_sf()





#save(all, all.s, file="combined.RData")
save(all, file="combined_ppr_new.RData")







# 
# ### Covariate plots
# png("plots/seasonality_cov.png", 6, 6, units="in", res=600)
# ggplot(all) +
#   geom_sf(aes(fill=seasonality))+
#   xlim(st_bbox(strata.big)[c(1,3)]) + ylim(st_bbox(strata.big)[c(2, 4)]) +
#   geom_sf(data=ppr_states, fill=NA, color="grey50", size=0.25) +
#   scale_fill_continuous_sequential(palette="Teal") +
#   theme_classic() +
#   labs(title="Seasonality", subtitle="Number of months water present", fill="Seasonality")
# #labs(title="Seasonality (Standardized)", subtitle="1993 - 2023", fill="Seasonality")
# dev.off()