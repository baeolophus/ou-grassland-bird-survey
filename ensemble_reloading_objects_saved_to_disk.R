#This file reloads all the objects saved in the course of the ensemble.

setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results")

SPECIES <- "EAME"

evalresults <- readRDS(file.path(
               SPECIES,
               paste0(
               SPECIES,
                        "_products_evaluation_results"))
)
#contains AUC/RMSE for all.

library(raster)

stateraster <- raster(paste0(SPECIES, "/EAME_products_statewide.raster.prediction.prob.tif"))
smallraster <- raster(paste0(SPECIES, "/EAME _ small _products_ensembleweightedmosaic .tif"))
mediumraster <- raster(paste0(SPECIES, "/EAME _ medium _products_ensembleweightedmosaic .tif"))
largeraster <- raster(paste0(SPECIES, "/EAME _ large _products_ensembleweightedmosaic .tif"))

plot(smallraster)
plot(mediumraster)
plot(largeraster)
plot(stateraster)

#rasters for support sets also exist but are probably not needed.

#now the microbenchmarks
microbenchmarks <- read.csv("EAME/EAME_products_microbenchmarks.csv")
boxplot(x = microbenchmarks$model,
     y = microbenchmarks$time,
     notched = TRUE)

#support sets
supportsets <- readRDS("EAME/EAME_large_intermediates_support_list")

polygons <- readRDS("EAME/EAME_large_intermediatefile_polys")

#read partial plots from statewide trees.
statewidevarimp <- readRDS("EAME/EAME_statewide_products_tree_and_varimp")


#loading Rdata gets the rest!

partialPlot(tree.statewide,
            statewide.data, 
            varnames.cforest[1],
            xlab=varnames.cforest[1],
            main=paste("Partial Dependence on", varnames.cforest[1]))
