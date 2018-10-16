#libraries needed
library(beepr)
library(dplyr)
library(GSIF)
library(microbenchmark)
library(randomForest)
library(raster)
library(rgdal)
library(ROCR)
library(rpart)
library(sp)

#create temporary raster files on large drive because they occupy 10-30 GB
rasterOptions()$tmpdir
#three locations depending on computer I'm using
rasterOptions(tmpdir="E:/Documents/R/temp")
rasterOptions(tmpdir="/media/Data/Documents/R/temp")
rasterOptions(tmpdir="/home/rifa1/Claire/temp")

#This file tests whether the support sets in the ensemble are significantly different in pixel coverage.
#Generate support sets
#start by generating random points within the study area.
#pull from different paths depending on computer (lines 24-29)
state<-readOGR(dsn="E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed",
               layer="ok_state_vector_smallest_pdf_3158")
state<-readOGR(dsn="/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed",
               layer="ok_state_vector_smallest_pdf_3158")
state<-readOGR(dsn="/home/rifa1/Claire",
               layer="ok_state_vector_smallest_pdf_3158")
state<-spTransform(x = state,
                   CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
)

set.seed(6798257) #set seed for random points, and later the other random processes.

#Function for random.stratified.support.sets used below.
source("source_ensemble_function_support_set_generation.R")

radius.small <- 60000 #small radius in meters. =6,000 = 60 km = 120 x 120 km boxes #200 points
radius.medium <- 100000 #med radius in meters. =100,000 = 100 km = 200 x 200 km boxes #75 points
radius.large <- 225000 #large radius in meters. =250,000 = 250 km = 500 x 500 km boxes #20 points

polys.small <- random.stratified.support.sets(numberofpoints = 100,
                                              radius.small)
polys.small.p <- unlist(polys.small[[1]])
polys.small.df <- unlist(polys.small[[2]])

polys.medium <- random.stratified.support.sets(numberofpoints = 37,
                                               radius.medium)
polys.medium.p <- unlist(polys.medium[[1]])
polys.medium.df <- unlist(polys.medium[[2]])

polys.large <- random.stratified.support.sets(numberofpoints = 12,
                                              radius.large)
polys.large.p <- unlist(polys.large[[1]])
polys.large.df <- unlist(polys.large[[2]])

#http://r-sig-geo.2731867.n2.nabble.com/Efficient-way-to-obtain-gridded-count-of-overlapping-polygons-td6034590.html
#Checking that the three scales have similar numbers of overlaps

#This file is used to rasterize the polygons at its resolution.  Any of the predictors would have worked.
nlcd_ok_utm14 <- raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/nlcd_processing/nlcd_cropped_to_ok_census/nlcd_ok_utm14_okmask.tif")
nlcd_ok_utm14 <- raster("/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/nlcd_processing/nlcd_cropped_to_ok_census/nlcd_ok_utm14_okmask.tif")
nlcd_ok_utm14 <- raster("/home/rifa1/Claire/nlcd_ok_utm14_okmask.tif")

beginCluster()
microbenchmark(countoverlapping.small <- clusterR(nlcd_ok_utm14, #raster
                                                  fun = rasterize,
                                                  args = list(x = polys.small.p,
                                                              fun = 'count')),
               times = 1)


microbenchmark(countoverlapping.medium <- clusterR(nlcd_ok_utm14, #raster
                                                   fun = rasterize,
                                                   args = list(x = polys.medium.p,
                                                               fun = 'count')),
               times = 1)

microbenchmark(countoverlapping.large <- clusterR(nlcd_ok_utm14, #raster
                                                  fun = rasterize,
                                                  args = list(x = polys.large.p,
                                                              fun = 'count')),
               times = 1)

#plot rasterized
par(mfrow=c(1,3))
plot(countoverlapping.small)
plot(state, add = TRUE)
plot(countoverlapping.medium)
plot(state, add = TRUE)
plot(countoverlapping.large)
plot(state, add = TRUE)

#plot polygons

svg(filename = "Claire/support_set_illustrations.svg",
    width = 7,
    height = 5)
par(mfrow=c(1,3))
plot(polys.small.p,
     ylab = "Small support sets")
plot(state, 
     add = TRUE)
plot(polys.medium.p,
     ylab = "Medium support sets")
plot(state,
     add = TRUE)
plot(polys.large.p,
     ylab = "Large support sets")
plot(state,
     add = TRUE)

par(mfrow=c(1,1))
dev.off()
sampling.overlaps <- spsample(state,
                              type = "regular",
                              n = 50)
plot(sampling.overlaps, add= TRUE)

overlaps.small.sample <- as.data.frame(extract(x = countoverlapping.small,
                                               y = sampling.overlaps))

colnames(overlaps.small.sample) <- c("overlapsperpixel", "set")
overlaps.small.sample$set <- "small"
summary(overlaps.small.sample$overlapsperpixel)
overlaps.medium.sample <- as.data.frame(extract(x = countoverlapping.medium,
                                                y = sampling.overlaps))
overlaps.medium.sample$set <- "medium"
colnames(overlaps.medium.sample) <- c("overlapsperpixel", "set")
summary(overlaps.medium.sample$overlapsperpixel)

overlaps.large.sample <- as.data.frame(extract(x = countoverlapping.large,
                                               y = sampling.overlaps))
overlaps.large.sample$set <- "large"
colnames(overlaps.large.sample) <- c("overlapsperpixel", "set")
summary(overlaps.large.sample$overlapsperpixel)

overlaps <- bind_rows(overlaps.small.sample,
                      overlaps.medium.sample,
                      overlaps.large.sample)

colnames(overlaps) <- c("overlapsperpixel", "set")
overlaps$set <- as.factor(overlaps$set)
endCluster()

#Test for differences.

overlaps.lm <- lm (overlapsperpixel ~ set,
                   data = overlaps)
anova(overlaps.lm)
plot(overlapsperpixel ~ set,
     data = overlaps)


library(multcomp)
anova.tukey<-glht(overlaps.lm,
                  linfct=mcp(set="Tukey"))
summary(anova.tukey)

