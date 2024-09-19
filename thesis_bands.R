#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("beepr", "colorspace", "raster", "rstan", "sf", "tidyverse")
ipak(packages)

### Set wd
setwd("/Users/madelienelohman/Desktop/thesis_redcap/banding_data")
load('map_dat.RData')



#~~~~~~~~~~~~~~~~
# Define parameters of data
#~~~~~~~~~~~~~~~~
### Species
# Mallards
species <- "Mallard"
species.code <- "MALL"

### Years needed
start <- 1993
end <- 2023
official.end <- 2020
years <- start:end
n.years <- length(start:end)

#~~~~~~
# Spatial data
#~~~~~~
### Load in
shape <- st_union(counties.ppr)
### Determine projection
proj <- st_crs(shape)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data cleaning and subsetting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~
# Read in data
#~~~~~~~~~~~~~~~~~~
b <- read.csv("Lohman_bnds_MALL_NOPI_BWTE1974_202407080758.csv")


### Filter for species and years
# Only birds with locations
b <- b %>%
  filter(SPECIES_NAME == species & BANDING_YEAR %in% years &
           !is.na(LAT_DECIMAL_DEGREES) & !is.na(LON_DECIMAL_DEGREES))

#~~~~~~~~~~~~~~~~~~~~~~
# Releases subsetting
#~~~~~~~~~~~~~~~~~~~~~~
### Released as a 'Normal wild bird'
releases <- b %>%
  filter(BIRD_STATUS == 3)

### Type of study captured for/how
# 00: Federal numbered metal band only.
# 04: Control band - For use in conjunction with reward band studies only. 
# 70: Spotlighted. 
capt.method <- c("0","00", "04", "4", "70")
# table(releases$EXTRA_INFO_CODE)
releases <- releases %>%
  filter(EXTRA_INFO_CODE %in% capt.method)

### Type of band
#table(releases$BAND_TYPE)
# 01: aluminum\butt-end toll free
# 04: aluminum\butt-end new address
# 08: aluminum\pre-open toll free
# 11: aluminum\butt end
# 18: aluminum\pre-open
# 21: monel\butt end
# 23: monel\butt-end toll free
# 41: aluminum\butt-end (toll-free /web address)
# 51: incoloy or Stainless\butt-end
# 53: incoloy or Stainless\butt-end toll free
# W1: Aluminum butt-end web address
band.type <- c('01', '1', '04', '4', '08', '8', '11', '18', '21', '23', '41', '51',
               '53', 'W1')
# table(releases$BAND_TYPE_CODE)
releases <- releases %>%
  filter(BAND_TYPE_CODE %in% band.type)

### Only individuals banded during pre-season banding
# table(releases$EVENT_MONTH)
rel.months <- 7:9
releases <- releases %>%
  filter(BANDING_MONTH %in% rel.months)

#~~~~~~~~
# Spatially subset releases
#~~~~~~~~
### Convert to sf object
releases <- st_as_sf(releases, coords=c("LON_DECIMAL_DEGREES", "LAT_DECIMAL_DEGREES"), 
                     crs=st_crs(4269), remove=F)
### Convert to Albers projection
releases <- st_transform(releases, crs=proj)
### Find points that intersect with strata
want <- st_intersects(shape, releases)
rels <- releases[want[[1]],]

rels <- rels %>%
  group_by(geometry) %>%
  mutate(id=cur_group_id())
# ggplot(rels) +
#   geom_sf()

rels.n <- rels %>%
  group_by(id, BANDING_YEAR) %>%
  summarize(n = n())

# ggplot(rels.n, aes(color=n)) +
#   geom_sf()


save(list=c("rels", "rels.n"), file="band_locs.RData")

# ggplot(rels.n, aes(x=BANDING_YEAR, y=n, color=id)) + 
#   geom_point() +
#   geom_smooth()



# yr2yr <- rels.n %>%
#   group_by(BANDING_YEAR) %>%
#   summarize(n = sum(n))
# 
# ggplot(yr2yr, aes(x=BANDING_YEAR, y=n)) + 
#   geom_point() +
#   geom_smooth()

