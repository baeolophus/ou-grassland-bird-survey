#This file reloads all the objects saved in the course of the ensemble.

setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results")

SPECIES <- "NOBO"

evalresults <- readRDS(file.path(
               SPECIES,
               paste0(
               SPECIES,
                        "_products_evaluation_results"))
)
#contains AUC/RMSE for all.

library(raster)

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

#read partial plots from statewide trees.
statewidevarimp <- readRDS("EAME/EAME_statewide_products_tree_and_varimp")


#loading Rdata gets the rest!
load(paste0("~/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/",
            SPECIES,
            "/",
            SPECIES,
            "_rdata.RData"

partialPlot(tree.statewide,
            statewide.data, 
            varnames.cforest[1],
            xlab=varnames.cforest[1],
            main=paste("Partial Dependence on", varnames.cforest[1]))
