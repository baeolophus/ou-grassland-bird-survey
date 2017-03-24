#This file reloads all the objects saved in the course of the ensemble.

readRDS(paste0(SPECIES,
                        "-ensembleresults"))
#contains statewide tree, microbenchmark, and AUC/RMSE for all.

readRDS(all support set trees)
readRDS(polygons)

library(raster)

stateraster <- 
small raster <- 
medium raster <- 
large raster <- 

#rasters for support sets also exist but are probably not needed.