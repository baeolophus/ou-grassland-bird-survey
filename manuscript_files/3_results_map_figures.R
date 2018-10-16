#This file reloads all the objects saved in the course of the ensemble.
#It then creates all the figures for current-day prediction models.

#setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current")
setwd("F:/Rhubarb/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current")

#Run this file once for each species below.
SPECIES <- "NOBO"
SPECIES <- "UPSA"
SPECIES <- "HOLA"
SPECIES <- "CASP"
SPECIES <- "FISP"
SPECIES <- "LASP"
SPECIES <- "GRSP"
SPECIES <- "DICK"
SPECIES <- "EAME"
SPECIES <- "WEME"
SPECIES <- "BHCO"

#load required libraries.
library(rgdal)
library(raster)
library(rasterVis)
library(sp)

#Load rasters.
stateraster <- raster(paste0(SPECIES,
                             "/",
                             SPECIES,
                             "_products_statewide.raster.prediction.prob.tif"))
smallraster <- raster(paste0(SPECIES,
                             "/",
                             SPECIES,
                             "_small_products_ensembleweightedmosaic.tif"))
mediumraster <- raster(paste0(SPECIES,
                              "/",
                              SPECIES,
                              "_medium_products_ensembleweightedmosaic.tif"))
largeraster <- raster(paste0(SPECIES,
                             "/",
                             SPECIES,
                             "_large_products_ensembleweightedmosaic.tif"))

rasterStack <- stack(stateraster,
                     smallraster,
                     mediumraster,
                     largeraster)

#Load state outline
state<-readOGR(dsn="F:/Rhubarb/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed",
               layer="ok_state_vector_smallest_pdf_3158")

  
#Ensure UTM coordinates are not abbreviated.
options(scipen=999)

#Write a pdf for each species containing four figure panels.
#i.e. section "Current Distribution" in Results.
#Figs. 3-6 and Figs. S1-7.  

pdf(file = paste0("figures/",
                  SPECIES,
                  "_maps.pdf"),
    width = 18,
    height = 9)

levelplot(rasterStack,
          xlab = "UTM easting",
          ylab = "UTM northing",
          names.attr = c("statewide",
                         "small",
                         "medium",
                         "large"))+
  layer(sp.polygons(state))

dev.off()
options(scipen=0)
#Return number abbreviation to normal.
