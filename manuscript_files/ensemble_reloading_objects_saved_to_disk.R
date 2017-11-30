#This file reloads all the objects saved in the course of the ensemble.
#It then creates all the map figures for current-day prediction models.

setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current")

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
library(dplyr)
library(ggplot2)
library(randomForest)
library(raster)
library(tidyr)

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

#loading Rdata gets the rest!
load(paste0("~/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current/",
            SPECIES,
            "/",
            SPECIES,
            "_rdata.RData"))

evalresults <- readRDS(file.path(
  SPECIES,
  paste0(
    SPECIES,
    "_products_evaluation_results"))
)
#contains AUC/RMSE for all.
eval.df <- data.frame(evalresults)

listerrornames <- c("statewide.sampling.rmse.sameyear",
                    "statewide.sampling.auc.sameyear",
                    "small.sampling.rmse.sameyear",
                    "small.sampling.auc.sameyear",
                    "medium.sampling.rmse.sameyear",
                    "medium.sampling.auc.sameyear",
                    "large.sampling.rmse.sameyear",
                    "large.sampling.auc.sameyear",
                    "statewide.sampling.rmse.diffyear",
                    "statewide.sampling.auc.diffyear",
                    "small.sampling.rmse.diffyear",
                    "small.sampling.auc.diffyear",
                    "medium.sampling.rmse.diffyear",
                    "medium.sampling.auc.diffyear",
                    "large.sampling.rmse.diffyear",
                    "large.sampling.auc.diffyear")
colnames(eval.df) <- listerrornames

eval.df.sep <- gather_(eval.df,
                      key_col = "errordescriptor",
                     value_col = "errornum",
                     gather_cols = listerrornames) %>%
  separate_(col = "errordescriptor",
            into = c("scale",
                     "sampling",
                     "errortype",
                     "year"))

eval.df.sep$scale <- as.factor(eval.df.sep$scale)
levels(eval.df.sep$scale) <- c("statewide",
                       "large",
                       "medium",
                       "small")

pdf(file = paste0(SPECIES,
                  "/",
                  SPECIES,
                  "_error.pdf"),
    width = 5,
    height = 5)
ggplot(data = eval.df.sep) + 
  geom_boxplot(mapping = aes(x = scale,
                             y = errornum),
               notch = TRUE)+
  facet_wrap(errortype ~ year,
             scales = "free_y")+
  xlab("scale") +
  ylab("error value") +
  theme_classic()
dev.off()


#Create file of top 10 important variables.
pdf(file = paste0(SPECIES,
                  "/",
                  SPECIES,
                  "_statewide_partialplots-cforest",
                  ".pdf"),
    width = 7, #plot.width,
    height = 10)#plot.height*2)

par(mfrow = c(5,2))
for (i in 1:10) {
  partialPlot(tree.statewide,
              statewide.data, 
              varnames.cforest[i],
              xlab=varnames.cforest[i],
              main = "",
              ylab = "partial dependence")
}

dev.off()


#threshold calculations for current-day maps.  Same source file as used in future threshold calculations.
source("source_function_threshold_calculations.R")

rasterOptions()$tmpdir
rasterOptions(tmpdir="F:/temp")

small.area <- thresholds(SPECIES,
                         paste0("/",
                                SPECIES,
                                "_small_products_ensembleweightedmosaic.tif"),
                         0.5)

medium.area <- thresholds(SPECIES,
                          paste0("/",
                                 SPECIES,
                                 "_medium_products_ensembleweightedmosaic.tif"),
                          0.5)

large.area <- thresholds(SPECIES,
                         paste0("/",
                                SPECIES,
                                "_large_products_ensembleweightedmosaic.tif"),
                         0.5)

statewide.area <- thresholds(SPECIES,
                             paste0("/",
                                    SPECIES,
                                    "_products_statewide.raster.prediction.prob.tif"),
                             0.5)

current.areas <- data.frame("areakm2" = rbind(small.area,
                                             medium.area,
                                             large.area,
                                             statewide.area))

current.areas$Species <- SPECIES
current.areas$model <- "current"
current.areas$threshold <- 0.5

write.csv(current.areas,
          file = paste0(SPECIES,
                        "/",
                        SPECIES,
                        "_products_current_map_areas.csv"))
