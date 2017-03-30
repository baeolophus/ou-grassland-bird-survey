#Ensemble function, calls on other functions.
library(randomForest)
library(dplyr)
library(raster)
library(rgdal)
library(mailR)
library(microbenchmark)

complete.ensemble.model <- function (SPECIES) {
  complete.dataset.for.sdm.SPECIES<-dplyr::filter(complete.dataset.for.sdm,
                                                  SPEC=="DICK", #SPECIES,
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
  send.mail(from = sender,
            to = recipients,
            subject = paste0("Your small ensemble is complete for ",
                             SPECIES),
            body = "Go download files!  Onward!",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
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
  send.mail(from = sender,
            to = recipients,
            subject = paste0("Your medium ensemble is complete for ",
                             SPECIES),
            body = "Go download files!  Onward!",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)

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
  
  send.mail(from = sender,
            to = recipients,
            subject = paste0("Your large ensemble is complete for ",
                             SPECIES),
            body = "Go download files!  Onward!",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)

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
                                                                             progress = "text")), 
  
  endCluster(),
  times = 1)
  #pdf or eps of map here generated here too
  writeRaster(tree.statewide.raster.prediction.prob,
              filename = paste0(SPECIES,
                                "tree.statewide.raster.prediction.prob",
                                ".tif"),
              format="GTiff",
              overwrite = TRUE)

  svg(file = paste0("DICK",#SPECIES,
                    "-varimpplot",
                    ".svg"), 
      width = 10,#plot.width,
      height = 8)#plot.height)
  varImpPlot(tree.statewide)
  dev.off()
  tree.statewide.varimp <- data.frame(importance(tree.statewide))
  
  #Go through top 30 variables in partialPlot
  #pdf
  #Get variable importances.
  imp <- importance(tree.statewide)
  #Order them.
  impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  #Create svg file of top 10 important variables.
  svg(file = paste0("DICK",#SPECIES,
                    "-partialplots",
                    ".svg"),
      width = 7, #plot.width,
      height = 10)#plot.height*2)

  par(mfrow = c(5,2))
  for (i in 1:10) {
    partialPlot(tree.statewide,
                statewide.data, 
                impvar[i],
                which.class = "1",
                xlab=impvar[i],
                main=paste("Partial Dependence on", impvar[i]))
  }

  dev.off()
  
  #return graphics to 1 x 1 state.
  par(mfrow = c(1,1))
  #http://stats.stackexchange.com/questions/93202/odds-ratio-from-decision-tree-and-random-forest
  #http://r.789695.n4.nabble.com/randomForest-PartialPlot-reg-td2551372.html shoudl not do the logit thing actually
  #Just go for target class (I want presence ie "1") and interpret higher as more likely.
  
  #http://stackoverflow.com/questions/32606375/rmse-calculation-for-random-forest-in-r
  
  #for spatially uniform test data
  #http://stackoverflow.com/questions/32862606/taking-random-point-from-list-of-points-per-grid-square
  #Do this sampling n times (200?  250?)
  
  
  #details on how to do probability maps for classification http://evansmurphy.wixsite.com/evansspatial/random-forest-sdm
  

  send.mail(from = sender,
            to = recipients,
            subject = paste0("Your statewide model is complete for ",
                             SPECIES),
            body = "Go download files!  Onward!",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
  #########################################
  #Once predictions made, evaluation of model.
  #example code: http://stackoverflow.com/questions/30366143/how-to-compute-roc-and-auc-under-roc-after-training-using-caret-in-r
  #and https://www.biostars.org/p/87110/
  #http://stats.idre.ucla.edu/r/faq/how-can-i-generate-bootstrap-statistics-in-r/
  #http://gsif.r-forge.r-project.org/sample.grid.html
  #http://www.stat.wisc.edu/~larget/stat302/chap3.pdf #bootstrapping/sampling
  
  
  ###############################
  ###############################
  #evaluate then plot bootstrap distributions and calculation mean and sd for AUC and RMSE
  statewide.sampling.rmse <- replicate(50,
                                       expr = do.call (what = spatial.sampling.evaluation,
                                                       args = list(evaluation.spatial,
                                                                   cell.size,
                                                                   n,
                                                                   typeofeval = "rmse",
                                                                   prediction.raster = tree.statewide.raster.prediction.prob)))
  

  #Then repeat for AUC
  statewide.sampling.auc <- replicate(50,
                                      expr = do.call (what = spatial.sampling.evaluation,
                                                      args = list(evaluation.spatial,
                                                                  cell.size,
                                                                  n,
                                                                  typeofeval = "auc",
                                                                  prediction.raster = tree.statewide.raster.prediction.prob)))
  
  #small
  #rmse
  small.sampling.rmse <- replicate(50,
                                       expr = do.call (what = spatial.sampling.evaluation,
                                                       args = list(evaluation.spatial,
                                                                   cell.size,
                                                                   n,
                                                                   typeofeval = "rmse",
                                                                   prediction.raster = support.small.ensemble)))
  
  
  #Then repeat for AUC
  small.sampling.auc <- replicate(50,
                                      expr = do.call (what = spatial.sampling.evaluation,
                                                      args = list(evaluation.spatial,
                                                                  cell.size,
                                                                  n,
                                                                  typeofeval = "auc",
                                                                  prediction.raster = support.small.ensemble)))
  
  
  #medium
  #rmse
  medium.sampling.rmse <- replicate(50,
                                   expr = do.call (what = spatial.sampling.evaluation,
                                                   args = list(evaluation.spatial,
                                                               cell.size,
                                                               n,
                                                               typeofeval = "rmse",
                                                               prediction.raster = support.medium.ensemble)))
  
  
  #Then repeat for AUC
  medium.sampling.auc <- replicate(50,
                                  expr = do.call (what = spatial.sampling.evaluation,
                                                  args = list(evaluation.spatial,
                                                              cell.size,
                                                              n,
                                                              typeofeval = "auc",
                                                              prediction.raster = support.medium.ensemble)))
  
  
  #large
  #rmse
  large.sampling.rmse <- replicate(50,
                                   expr = do.call (what = spatial.sampling.evaluation,
                                                   args = list(evaluation.spatial,
                                                               cell.size,
                                                               n,
                                                               typeofeval = "rmse",
                                                               prediction.raster = support.large.ensemble)))
  
  
  #Then repeat for AUC
  large.sampling.auc <- replicate(50,
                                  expr = do.call (what = spatial.sampling.evaluation,
                                                  args = list(evaluation.spatial,
                                                              cell.size,
                                                              n,
                                                              typeofeval = "auc",
                                                              prediction.raster = support.large.ensemble)))
  
  
  #print figures of both
  svg(file = paste0(SPECIES,
                    "-RMSE",
                    ".svg"), 
      width = plot.width,
      height = plot.height)
  boxplot(cbind("Small" = small.sampling.rmse,
                "Medium" = medium.sampling.rmse,
                "Large" = large.sampling.rmse,
                "Statewide" = statewide.sampling.rmse),
          xlab = "Support set size",
          ylab = "RMSE")
  
  dev.off()
  svg(file = paste0(SPECIES,
                    "-AUC",
                    ".svg"), 
      width = plot.width,
      height = plot.height)
  boxplot(cbind("Small" = small.sampling.auc,
                "Medium" = medium.sampling.auc,
                "Large" = large.sampling.auc,
                "Statewide" = statewide.sampling.auc),
          xlab = "Support set size",
          ylab = "AUC")
  dev.off()
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
  results <- list(microbenchmarks,
                  statewide.sampling.rmse,
                  statewide.sampling.auc,
                  small.sampling.rmse,
                  small.sampling.auc,
                  medium.sampling.rmse,
                  medium.sampling.auc,
                  large.sampling.rmse,
                  large.sampling.auc,
                  tree.statewide)
  saveRDS(results,
          file = paste0(SPECIES,
                        "-ensembleresults"))
  
  sender <- "curryclairem.mail@gmail.com"
  recipients <- c("curryclairem.mail@gmail.com")
  send.mail(from = sender,
            to = recipients,
            subject = paste0("Everything is complete for ",
                             SPECIES),
            body = "Go download any remaining files!  Onward!",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
  return(results)
  
  #####################
  #Delete temporary file directory at end of species processing.
  #http://stackoverflow.com/questions/18955305/setting-an-overwriteable-temporary-file-for-rasters-in-r?noredirect=1&lq=1
  unlink(file.path(getwd(),"rastertemp"),
         recursive = TRUE)
}
