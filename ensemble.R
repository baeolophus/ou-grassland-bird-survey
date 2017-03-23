#libraries needed
library(beepr)
#library(dplyr)
library(microbenchmark)
#library(randomForest)
#library(raster)
#library(rgdal)
#library(ROCR)
#library(rpart)
#library(sp)

#create temporary raster files on large drive because they occupy 10-30 GB
rasterOptions()$tmpdir
rasterOptions(tmpdir=paste0(getwd(),
                            "/rastertemp"))


#Bring in predictor data.
source("source_ensemble_predictor_import.R")
#########################
#Responses
#Bring in whole response data set (with NA lat/long already removed) as a spatial object including presence/absence.
#Is already in utm
complete.dataset.for.sdm <- read.csv(file = "oklahomadatasetforsdm_naomit_utm.csv")

#Run the ensemble
source("source_ensemble_complete_ensemble_model.R")

complete.ensemble.model(SPECIES = "DICK")
complete.ensemble.model(SPECIES = "EAME")
complete.ensemble.model(SPECIES = "WEME")
complete.ensemble.model(SPECIES = "LASP")
complete.ensemble.model(SPECIES = "NOBO")
complete.ensemble.model(SPECIES = "GRSP")
complete.ensemble.model(SPECIES = "CASP")
complete.ensemble.model(SPECIES = "HOLA")
complete.ensemble.model(SPECIES = "BHCO")
complete.ensemble.model(SPECIES = "FISP")
complete.ensemble.model(SPECIES = "UPSA")