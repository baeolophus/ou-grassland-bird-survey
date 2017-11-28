#Future bioclim using current models on futurestack raster predictions.
setwd("/data/grassland_ensemble")
library(randomForest)
library(raster)
library(mailR)
library(microbenchmark)

#create temporary raster files on large drive because they occupy 10-30 GB
rasterOptions()$tmpdir
rasterOptions(tmpdir=paste0(getwd(),
                            "/rastertemp"))


#email notifications
sender <- "curryclairem.mail@gmail.com"
recipients <- c("curryclairem.mail@gmail.com")

#load future source rasters for prediction.
#Bring in predictor data.
source("source_ensemble_predictor_import.R")
#bring in the two rasters needed for predict::raster stage but aren't lumped into main predictor_stack.
#(effort is obtained from dataframes).
#file in working directory AWS
effort_length <- raster("effort/effort_length.tif")
effort_time <- raster("effort/effort_time.tif")
time_of_day <- raster("effort/time_of_day.tif")
#Create predictors_stack_with_all_variables to include a raster version of effort time and and effort length
future_predictors_stack_with_all_variables <- addLayer(predictors_stack,
                                                       effort_length,
                                                       effort_time,
                                                       time_of_day)





#running all future species with source code.

SPECIES <- "EAME"
source("future_bioclim_rasters.R")

SPECIES <- "WEME"
source("future_bioclim_rasters.R")

SPECIES <- "HOLA"
source("future_bioclim_rasters.R")

SPECIES <- "CASP"
source("future_bioclim_rasters.R")

SPECIES <- "FISP"
source("future_bioclim_rasters.R")

SPECIES <- "LASP"
source("future_bioclim_rasters.R")

SPECIES <- "GRSP"
source("future_bioclim_rasters.R")

SPECIES <- "DICK"
source("future_bioclim_rasters.R")

SPECIES <- "NOBO"
source("future_bioclim_rasters.R")

SPECIES <- "UPSA"
source("future_bioclim_rasters.R")

SPECIES <- "BHCO"
source("future_bioclim_rasters.R")