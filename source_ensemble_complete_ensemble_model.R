#Ensemble function, calls on other functions.
library(randomForest)
library(dplyr)
library(raster)
library(rgdal)
library(mailR)
library(microbenchmark)

beginCluster()
  complete.dataset.for.sdm.SPECIES<-dplyr::filter(complete.dataset.for.sdm,
                                                  SPEC==SPECIES)
  
  #make it spatial, remembering these values were converted from lat/long to UTM already in data manipulation file.
  coordinates(complete.dataset.for.sdm.SPECIES)<-c("Longitude", "Latitude")
  #make it spatial
  proj4string(complete.dataset.for.sdm.SPECIES)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  #check it worked
  proj4string(complete.dataset.for.sdm.SPECIES)
  #extract values for analysis
  predictors_stack.SPECIES<-extract(x = predictors_stack,
                                    y = complete.dataset.for.sdm.SPECIES)
  predictors_stack.SPECIES.df <- as.data.frame(predictors_stack.SPECIES)
  
  latlong.predictors.SPECIES<-cbind("presence" = complete.dataset.for.sdm.SPECIES$presence,
                                    coordinates(complete.dataset.for.sdm.SPECIES),
                                    "effort_time_ok_census_mask" = complete.dataset.for.sdm.SPECIES$effort_time,
                                    "effort_length_ok_census_mask" = complete.dataset.for.sdm.SPECIES$effort_length,
                                    predictors_stack.SPECIES.df,
                                    row.names = NULL)
  #land covers are factors.
  latlong.predictors.SPECIES$nlcd_ok_utm14_okmask <- as.factor(latlong.predictors.SPECIES$nlcd_ok_utm14_okmask)
  latlong.predictors.SPECIES.spatial <- latlong.predictors.SPECIES
  #has to be spatial for function to work so re-add that
  coordinates(latlong.predictors.SPECIES.spatial) <- c("Longitude", "Latitude")
  proj4string(latlong.predictors.SPECIES.spatial)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  #free up memory
  rm(predictors_stack.SPECIES)  
  rm(predictors_stack.SPECIES.df) 
  rm(complete.dataset.for.sdm.SPECIES)
  gc()
  
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
  
  #create temporary raster files in a folder that can be deleted as the intermediate ones won't be needed later
  rasterOptions()$tmpdir
  rasterOptions(tmpdir=paste0(getwd(),
                              "/rastertemp/",
                              SPECIES,
                              "/small"))
  send.mail(from = sender,
            to = recipients,
            subject = paste0("Your small ensemble is starting for ",
                             SPECIES),
            body = "Save the before and after in case microbenchmark crashes.",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
   microbenchmark.small <- microbenchmark(
    support.small.list <- lapply(1:length(polys.small.df),
                               FUN = spatial.support.set,
                               spatialdataset = latlong.predictors.SPECIES.spatial,
                               predictor_stack = predictors_stack_with_all_variables,
                               polys.df = polys.small.df,
                               ntree = ntree,
                               importance = importance),
    times = 1)
  rasterOptions(tmpdir=paste0(getwd(),
                              "/rastertemp/",
                              SPECIES,
                              "/mosaic"))
  microbenchmark.small2 <- microbenchmark(support.small.ensemble <- ensemble.function(support.small.list),
    times = 1)
  unlink(file.path(getwd(),
                   "rastertemp",
                   SPECIES,
                   "small"),
         recursive = TRUE)

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
  
  rasterOptions()$tmpdir
  rasterOptions(tmpdir=paste0(getwd(),
                              "/rastertemp/",
                              SPECIES,
                              "/medium"))
  send.mail(from = sender,
            to = recipients,
            subject = paste0("Your medium ensemble is starting for ",
                             SPECIES),
            body = "Save the before and after in case microbenchmark crashes.",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  microbenchmark.medium <- microbenchmark(
  support.medium.list <- lapply(1:length(polys.medium.df),
                                FUN = spatial.support.set,
                                spatialdataset = latlong.predictors.SPECIES.spatial,
                                predictor_stack = predictors_stack_with_all_variables,
                                polys.df = polys.medium.df,
                                ntree = ntree,
                                importance = importance),
  times = 1)
  rasterOptions(tmpdir=paste0(getwd(),
                              "/rastertemp/",
                              SPECIES,
                              "/mosaic"))
  microbenchmark.medium2 <- microbenchmark(
    support.medium.ensemble <- ensemble.function(support.medium.list),
  times = 1)
  #remove medium temporary files
  unlink(file.path(getwd(),
                   "rastertemp",
                   SPECIES,
                   "medium"),
         recursive = TRUE)
 
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
  rasterOptions(tmpdir=paste0(getwd(),
                              "/rastertemp/",
                              SPECIES,
                              "/large"))
  send.mail(from = sender,
            to = recipients,
            subject = paste0("Your large ensemble is starting for ",
                             SPECIES),
            body = "Save the before and after in case microbenchmark crashes.",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  microbenchmark.large <- microbenchmark(
  support.large.list <- lapply(1:length(polys.large.df),
                                              FUN = spatial.support.set,
                                              spatialdataset = latlong.predictors.SPECIES.spatial,
                                              predictor_stack = predictors_stack_with_all_variables,
                                              polys.df = polys.large.df,
                                              ntree = ntree,
                                              importance = importance),
  times = 1)
  rasterOptions(tmpdir=paste0(getwd(),
                              "/rastertemp/",
                              SPECIES,
                              "/mosaic"))
  microbenchmark.large2 <- microbenchmark(
    support.large.ensemble <- ensemble.function(support.large.list),
  times = 1)
  #remove large temporary files
  unlink(file.path(getwd(),
                   "rastertemp",
                   SPECIES,
                   "large"),
         recursive = TRUE)

  
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

  ############################
  #Statewide model
  
  statewide.data <- latlong.predictors.SPECIES
  statewide.data$Longitude <- NULL
  statewide.data$Latitude <- NULL
  #These two columns should be taken out because not predicting on them.

  rasterOptions(tmpdir=paste0(getwd(),
                              "/rastertemp/",
                              SPECIES,
                              "/statewide"))
  send.mail(from = sender,
            to = recipients,
            subject = paste0("Your statewide model is starting for ",
                             SPECIES),
            body = "Save the before and after in case microbenchmark crashes.",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  microbenchmark.statewide <- microbenchmark (tree.statewide <- randomForest(presence ~ ., 
                                   data = statewide.data,
                                   ntree = ntree,
                                   replace = FALSE, #strobl et al. 2007
                                   importance = TRUE),
                                   times = 1)
  microbenchmark.statewide2 <- microbenchmark (
  tree.statewide.raster.prediction.prob <- raster::predict(predictors_stack_with_all_variables,
                                                           model = tree.statewide,
                                                         #  type = "prob",
                                                           progress = "text",
                                                           filename = paste0(SPECIES,
                                                                             "_tree.statewide.raster.prediction.prob",
                                                                             ".tif"),
                                                           format="GTiff",
                                                           overwrite = TRUE),
  times = 1)
  endCluster()

  svg(file = paste0(SPECIES,
                    "-varimpplot",
                    ".svg"), 
      width = 10,#plot.width,
      height = 8)#plot.height)
  varImpPlot(tree.statewide,
             scale = FALSE)
  dev.off()
  tree.statewide.varimp <- data.frame(importance(tree.statewide,
                                                 scale = FALSE))
  
  #Go through top 30 variables in partialPlot
  #pdf
  #Get variable importances.
  imp <- importance(tree.statewide, 
                    scale = FALSE)  #scale = false from strobl et al. 2007
  #Order them.
  impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  #Create svg file of top 10 important variables.
  svg(file = paste0(SPECIES,
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
  
  #Then variable importance in cforest (Strobl et al. papers on bias).
  my_cforest_control <- cforest_control(teststat = "quad",
                                        testtype = "Univ",
                                        mincriterion = 0,
                                        ntree = ntree, 
                                        mtry = floor(sqrt(ncol(statewide.data)))-1,
                                        replace = FALSE)
  
  tree.statewide.cforest <- cforest(presence ~ .,
                            data = statewide.data,
                            controls = my_cforest_control)
  
  imp.cforest <- as.data.frame(varimp(tree.statewide.cforest))
  varnames.cforest <- rownames(imp.cforest)[order(imp.cforest, decreasing=TRUE)]

  #Create svg file of top 10 important variables.
  svg(file = paste0(SPECIES,
                    "-partialplots-cforest",
                    ".svg"),
      width = 7, #plot.width,
      height = 10)#plot.height*2)
  
  par(mfrow = c(5,2))
  for (i in 1:10) {
    partialPlot(tree.statewide,
                statewide.data, 
                varnames.cforest[i],
                which.class = "1",
                xlab=varnames.cforest[i],
                main=paste("Partial Dependence on", varnames.cforest[i]))
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
  microbenchmark.statewide2$model <- "statewide2"
  microbenchmark.large2$model <- "large2"
  microbenchmark.medium2$model <- "medium2"
  microbenchmark.small2$model <- "small2"
  
  microbenchmarks <- rbind(microbenchmark.statewide,
                           microbenchmark.large,
                           microbenchmark.medium,
                           microbenchmark.small,
                           microbenchmark.statewide2,
                           microbenchmark.large2,
                           microbenchmark.medium2,
                           microbenchmark.small2)
  
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
                  tree.statewide,
                  tree.statewide.cforest,
                  imp.cforest,
                  varnames.cforest)
  saveRDS(results,
          file = paste0(SPECIES,
                        "_ensembleresults"))
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

  
  #####################
  #remove the species-specific objects.
  rm(latlong.predictors.SPECIES,
     latlong.predictors.SPECIES.spatial,
     polys.small,
     polys.small.p,
     polys.small.df,
     microbenchmark.small,
     support.small.list,
     support.small.ensemble,
     polys.medium,
     polys.medium.p,
     polys.medium.df,
     microbenchmark.medium,
     support.medium.list,
     support.medium.ensemble,
     polys.large,
     polys.large.p,
     polys.large.df,
     microbenchmark.large,
     support.large.list,
     support.large.ensemble,
     statewide.data,
     microbenchmark.statewide,
     tree.statewide,
     tree.statewide.raster.prediction.prob,
     tree.statewide.varimp,
     imp,
     impvar,
     statewide.sampling.rmse,
     statewide.sampling.auc,
     small.sampling.rmse,
     small.sampling.auc,
     medium.sampling.rmse,
     medium.sampling.auc,
     large.sampling.rmse,
     large.sampling.auc,
     microbenchmarks,
     results,
     tree.statewide.cforest,
     imp.cforest,
     varnames.cforest,
     microbenchmark.statewide2,
     microbenchmark.large2,
     microbenchmark.medium2,
     microbenchmark.small2)
  #Delete temporary file directory at end of species processing.
  #http://stackoverflow.com/questions/18955305/setting-an-overwriteable-temporary-file-for-rasters-in-r?noredirect=1&lq=1
  unlink(file.path(getwd(),"rastertemp"),
         recursive = TRUE)


