#libraries needed
#library(beepr)
#library(dplyr)
library(microbenchmark)
#library(randomForest)
library(raster)
#library(rgdal)
#library(ROCR)
#library(rpart)
#library(sp)

setwd("/data/grassland_ensemble")

#create temporary raster files on large drive because they occupy 10-30 GB
rasterOptions()$tmpdir
rasterOptions(tmpdir=paste0(getwd(),
                            "/rastertemp"))


#Bring in predictor data.
source("source_ensemble_predictor_import.R")

#Responses
#Bring in whole response data set (with NA lat/long already removed) as a spatial object including presence/absence.
#Is already in utm
complete.dataset.for.sdm <- read.csv(file = "oklahomadatasetforsdm_naomit_utm.csv")

#bring in the two rasters needed for predict::raster stage but aren't lumped into main predictor_stack.
#(effort is obtained from dataframes).
#file in working directory AWS
effort_length <- raster("effort/effort_length.tif")
effort_time <- raster("effort/effort_time.tif")
#local computer
#effort_length_ok_census_mask <- raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/effort_length_ok_census_mask.tif")
#effort_time_ok_census_mask <- raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/effort_time_ok_census_mask.tif")

#Create predictors_stack_with_all_variables to include a raster version of effort time and and effort length
predictors_stack_with_all_variables <- addLayer(predictors_stack,
                                                effort_length,
                                                effort_time)

#parameters for random forest models and support set sizes.
#random forest parameters
ntree <- 500
importance <- FALSE
#support set dimensions/samples
radius.small <- 60000 #small radius in meters. =6,000 = 60 km = 120 x 120 km boxes #200 points
radius.medium <- 100000 #med radius in meters. =100,000 = 100 km = 200 x 200 km boxes #75 points
radius.large <- 225000 #large radius in meters. =250,000 = 250 km = 500 x 500 km boxes #25 points
numberofpoints.small <- 100
numberofpoints.medium <- 37
numberofpoints.large <- 12

#bring in evaluation dataset and make it spatial object
evaluation.spatial <- read.csv(file = "oklahoma_evaluation_datasetforsdm_naomit_utm.csv") 
coordinates(evaluation.spatial) <- c("Longitude", "Latitude")
proj4string(evaluation.spatial) <- CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#evaluation grid size
cell.size <- c(10000, 10000)
n <- 10 #number of samples to take from each evaluation grid
plot.width <- 7 #svg plot dimensions in inches
plot.height <- 5

#email notifications
sender <- "curryclairem.mail@gmail.com"
recipients <- c("curryclairem.mail@gmail.com")

#Bring in the ensemble function newly for each species.
SPECIES <- "EAME"
source("source_ensemble_complete_ensemble_model.R")

SPECIES <- "WEME"
source("source_ensemble_complete_ensemble_model.R")

SPECIES <- "HOLA"
source("source_ensemble_complete_ensemble_model.R")

SPECIES <- "CASP"
source("source_ensemble_complete_ensemble_model.R")

SPECIES <- "FISP"
source("source_ensemble_complete_ensemble_model.R")

SPECIES <- "LASP"
source("source_ensemble_complete_ensemble_model.R")

SPECIES <- "GRSP"
source("source_ensemble_complete_ensemble_model.R")

SPECIES <- "DICK"
source("source_ensemble_complete_ensemble_model.R")

SPECIES <- "NOBO"
source("source_ensemble_complete_ensemble_model.R")

SPECIES <- "UPSA"
source("source_ensemble_complete_ensemble_model.R")

SPECIES <- "BHCO"
source("source_ensemble_complete_ensemble_model.R")

send.mail(from = sender,
          to = recipients,
          subject = paste0("Downgrade to free tier!!  Quick!",
                           SPECIES),
          body = "Downgrade server type to free tier.  Download all EBS data.  Terminate all!",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "curryclairem.mail@gmail.com",            
                      passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)

