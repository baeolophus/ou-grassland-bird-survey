#Ensemble function, calls on other functions.
library(randomForest)
library(dplyr)
library(raster)
library(rgdal)
library(microbenchmark)

complete.ensemble.model <- function (SPECIES) {
  complete.dataset.for.sdm.SPECIES<-dplyr::filter(complete.dataset.for.sdm,
                                                  SPEC==SPECIES,
                                                  month == 4 | month == 5 | month == 6 | month == 7)
  #to match transects and point counts, summer only.
  
  #make it spatial, remembering these values were converted from lat/long to UTM already in data manipulation file.
  coordinates(complete.dataset.for.sdm.SPECIES)<-c("Longitude", "Latitude")
  #make it spatial
  proj4string(complete.dataset.for.sdm.SPECIES)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  #check it worked
  proj4string(complete.dataset.for.sdm.SPECIES)
  #extract values for analysis
  predictors_stack.SPECIES<-extract(x=predictors_stack,
                                    y=complete.dataset.for.sdm.SPECIES)
  predictors_stack.SPECIES.df <- as.data.frame(predictors_stack.SPECIES)
  
  latlong.predictors.SPECIES<-cbind("presence" = as.factor(complete.dataset.for.sdm.SPECIES$presence),
                                    coordinates(complete.dataset.for.sdm.SPECIES),
                                    predictors_stack.SPECIES.df,
                                    row.names = NULL)
  
  latlong.predictors.SPECIES.spatial <-cbind("presence" = complete.dataset.for.sdm.SPECIES$presence,
                                             coordinates(complete.dataset.for.sdm.SPECIES),
                                             predictors_stack.SPECIES.df,
                                             row.names = NULL)
  #has to be spatial for function to work so re-add that
  coordinates(latlong.predictors.SPECIES.spatial) <- c("Longitude", "Latitude")
  proj4string(latlong.predictors.SPECIES.spatial)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  #free up memory
  rm(predictors_stack.SPECIES)  
  rm(predictors_stack.SPECIES.df) 
  ##################################
  #load functions
  source("source_ensemble_function_support_set_generation.R")
  source("source_ensemble_function_support_set_subsetting_trees.R")
  source("source_ensemble_function_support_set_ensemble_mosaic.R")
  source("source_ensemble_function_spatial_sampling_evaluation.R")
  ############################
  #Run small, medium, and large support set models.
  
  #small support sets
  polys.small <- random.stratified.support.sets(numberofpoints = numberofpoints.small,
                                                radius.small)
  polys.small.p <- unlist(polys.small[[1]])
  polys.small.df <- unlist(polys.small[[2]])
  microbenchmark.small <- microbenchmark(
    support.small.list <- lapply(1:numberofpoints.small,
                               FUN = spatial.support.set,
                               spatialdataset = latlong.predictors.SPECIES.spatial,
                               predictor_stack = predictors_stack,
                               polys.df = polys.small.df,
                               ntree = ntree,
                               importance = importance),
  support.small.ensemble <- ensemble.function(support.small.list),
  times = 1)
  
  #medium support sets
  polys.medium <- random.stratified.support.sets(numberofpoints = numberofpoints.medium,
                                                 radius.medium)
  polys.medium.p <- unlist(polys.medium[[1]])
  polys.medium.df <- unlist(polys.medium[[2]])
  microbenchmark.medium <- microbenchmark(
  support.medium.list <- lapply(1:numberofpoints.medium,
                                FUN = spatial.support.set,
                                spatialdataset = latlong.predictors.SPECIES.spatial,
                                predictor_stack = predictors_stack,
                                polys.df = polys.medium.df,
                                ntree = ntree,
                                importance = importance),
  
  support.medium.ensemble <- ensemble.function(support.medium.list),
  times = 1)
  
  #large support sets
  polys.large <- random.stratified.support.sets(numberofpoints = numberofpoints.large,
                                                radius.large)
  polys.large.p <- unlist(polys.large[[1]])
  polys.large.df <- unlist(polys.large[[2]])
  microbenchmark.large <- microbenchmark(
  support.large.list <- lapply(1:numberofpoints.large,
                                              FUN = spatial.support.set,
                                              spatialdataset = latlong.predictors.SPECIES.spatial,
                                              predictor_stack = predictors_stack,
                                              polys.df = polys.large.df,
                                              ntree = ntree,
                                              importance = importance),

  support.large.ensemble <- ensemble.function(support.large.list),
  times = 1)
  
  
  
  beep()#small, medium, large
  
  ############################
  #Statewide model
  
  statewide.data <- latlong.predictors.SPECIES
  statewide.data$Longitude <- NULL
  statewide.data$Latitude <- NULL
  #These two columns should be taken out because not predicting on them.
  microbenchmark.statewide <- microbenchmark (
  tree.statewide <- randomForest(presence ~ ., 
                                 data = statewide.data,
                                 ntree = ntree,
                                 importance = TRUE),
  
  beginCluster(),
  tree.statewide.raster.prediction.prob<-clusterR(predictors_stack,
                                                                 raster::predict,
                                                                 args = list(model = tree.statewide,
                                                                             type = "prob",
                                                                             progress = "text")), #3.7 hours
  
  endCluster(),
  times = 1)
  #pdf or eps of map here generated here too
  writeRaster(tree.statewide.raster.prediction.prob,
              filename = paste0("tree.statewide.raster.prediction.prob",
                                ".tif"),
              format="GTiff",
              overwrite = TRUE)

  plot(tree.statewide.raster.prediction.prob)
  
  saveRDS(tree.statewide,
          file = paste0(SPECIES,
                        "treestatewide")
          )

  varImpPlot(tree.statewide)
  print(tree.statewide)
  tree.statewide.varimp <- data.frame(importance(tree.statewide))
  
  #Go through top 30 variables in partialPlot
  #pdf
  
  partialPlot(tree.statewide, 
              statewide.data, 
              grasslands71_15cell,
              "1") #using 1 as reference ie presence
  
  #http://stats.stackexchange.com/questions/93202/odds-ratio-from-decision-tree-and-random-forest
  #http://r.789695.n4.nabble.com/randomForest-PartialPlot-reg-td2551372.html shoudl not do the logit thing actually
  #Just go for target class (I want presence ie "1") and interpret higher as more likely.
  
  #http://stackoverflow.com/questions/32606375/rmse-calculation-for-random-forest-in-r
  
  #for spatially uniform test data
  #http://stackoverflow.com/questions/32862606/taking-random-point-from-list-of-points-per-grid-square
  #Do this sampling n times (200?  250?)
  
  
  #details on how to do probability maps for classification http://evansmurphy.wixsite.com/evansspatial/random-forest-sdm
  
  #########################################
  #Once predictions made, evaluation of model.
  #example code: http://stackoverflow.com/questions/30366143/how-to-compute-roc-and-auc-under-roc-after-training-using-caret-in-r
  #and https://www.biostars.org/p/87110/
  #http://stats.idre.ucla.edu/r/faq/how-can-i-generate-bootstrap-statistics-in-r/
  #http://gsif.r-forge.r-project.org/sample.grid.html
  #http://www.stat.wisc.edu/~larget/stat302/chap3.pdf #bootstrapping/sampling
  
  
  ###############################
  #Dataset preparation
  evaluation.spatial <- latlong.predictors.SPECIES.spatial
  coordinates(evaluation.spatial) <- c("Longitude", "Latitude")
  prediction.raster<-tree.statewide.raster.prediction.prob
  
  ###############################
  #evaluate small, med, large, and statewide, then plot bootstrap distributions and calculation mean and sd for AUC and RMSE
  spatial.sampling.evaluation <- latlong.predictors.SPECIES.spatial #replace with evaluation dataset
  cell.size <- c(10000, 10000)
  n <- 10
  
  statewide.sampling.rmse <- replicate(50,
                                       expr = do.call (what = spatial.sampling.evaluation,
                                                       args = list(evaluation.spatial= latlong.predictors.SPECIES.spatial,
                                                                   cell.size,
                                                                   n,
                                                                   typeofeval = "rmse",
                                                                   prediction.raster = tree.statewide.raster.prediction.prob)))
  
  #repeat for these
  support.small.ensemble
  support.medium.ensemble
  support.large.ensemble
  
  boxplot(cbind("Small" = statewide.sampling.rmse,
                "Medium" = statewide.sampling.rmse,
                "Large" = statewide.sampling.rmse,
                "Statewide" = statewide.sampling.rmse),
          xlab = "Support set size",
          ylab = "AUC")
  
  
  #Then repeat for AUC
  
  statewide.sampling.auc <- replicate(50,
                                      expr = do.call (what = spatial.sampling.evaluation,
                                                      args = list(evaluation.spatial= latlong.predictors.SPECIES.spatial,
                                                                  cell.size,
                                                                  n,
                                                                  typeofeval = "auc",
                                                                  prediction.raster = tree.statewide.raster.prediction.prob)))
  boxplot(cbind("Small" = ,
                "Medium" = ,
                "Large" = ,
                "Statewide" = statewide.sampling.auc),
          xlab = "Support set size",
          ylab = "AUC")
  
  ####################
  #report microbenchmark values for each model
  microbenchmark.statewide$model <- "statewide"
  microbenchmark.large$model <- "large"
  microbenchmark.medium$model <- "medium"
  microbenchmark.small$model <- "small"
  
  microbenchmarks <- rbind(microbenchmark.statewide,
                           microbenchmark.large,
                           microbenchmark.medium,
                           microbenchmark.small)
  
  microbenchmarks$Species <- SPECIES
  #####################
  return(list(microbenchmarks,
         statewide.sampling.rmse,
         statewide.sampling.auc))
  
  #####################
  #Delete temporary file directory at end of species processing.
  #http://stackoverflow.com/questions/18955305/setting-an-overwriteable-temporary-file-for-rasters-in-r?noredirect=1&lq=1
  unlink(file.path(getwd(),"rastertemp"),
         recursive = TRUE)
}
