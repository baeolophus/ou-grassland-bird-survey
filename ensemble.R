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

#parameters for random forest models and support set sizes.
#random forest parameters
ntree <- 50
importance <- FALSE
#support set dimensions/samples
radius.small <- 60000 #small radius in meters. =6,000 = 60 km = 120 x 120 km boxes #200 points
radius.medium <- 100000 #med radius in meters. =100,000 = 100 km = 200 x 200 km boxes #75 points
radius.large <- 225000 #large radius in meters. =250,000 = 250 km = 500 x 500 km boxes #25 points
numberofpoints.small <- 200
numberofpoints.medium <- 75
numberofpoints.large <- 25
evaluation.spatial <- latlong.predictors.SPECIES.spatial #replace with evaluation dataset
cell.size <- c(10000, 10000)
n <- 10 #number of samples to take from each evaluation grid
plot.width <- 7 #svg plot dimensions in inches
plot.height <- 5

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