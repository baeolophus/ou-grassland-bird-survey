#This file reloads all the objects saved in the course of the ensemble.

setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/ClimateChange")

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

library(dplyr)
library(ggplot2)
library(randomForest)
library(raster)
library(tidyr)

stateraster <- raster(paste0(SPECIES,
                             "/",
                             SPECIES,
                             "_products_future_statewide_raster_prediction_prob.tif"))
smallraster <- raster(paste0(SPECIES,
                             "/",
                             SPECIES,
                             "_small_future_products_ensembleweightedmosaic.tif"))
mediumraster <- raster(paste0(SPECIES,
                              "/",
                              SPECIES,
                              "_medium_future_products_ensembleweightedmosaic.tif"))
largeraster <- raster(paste0(SPECIES,
                             "/",
                             SPECIES,
                             "_large_future_products_ensembleweightedmosaic.tif"))
options(scipen=999)
pdf(file = paste0(SPECIES,
                  "/",
                  SPECIES,
                  "_maps.pdf"),
    width = 18,
    height = 9)
par(mfrow = c(2,2))

plot(stateraster,
     main = "statewide")
plot(largeraster,
     main = "large")
plot(mediumraster,
     main = "medium")
plot(smallraster,
     main = "small")

dev.off()
options(scipen=0)


#####microbenchmark
medium_micro <- readRDS("EAME/EAME_medium_future_product_microbenchmark2")
large_micro <- readRDS("EAME/EAME_large_future_product_microbenchmark2")

medium_micro$Species <- "EAME"
large_micro$Species <- "EAME"

statewide_micro <- readRDS("EAME/EAME_product_microbenchmarks_statewide_future")
statewide_micro$model <- "statewidefuture2"

write.csv(medium_micro,
          file = paste0("EAME/", "EAME",
                        "_medium_future",
                        "_products_microbenchmarks_future.csv"))

write.csv(large_micro,
          file = paste0("EAME/","EAME",
                        "_large_future",
                        "_products_microbenchmarks_future.csv"))

write.csv(statewide_micro,
          file = paste0("EAME/","EAME",
                        "_product_microbenchmarks_statewide_future.csv"))
