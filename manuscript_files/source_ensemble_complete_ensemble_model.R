#Ensemble function, calls on other functions.
library(randomForest)
library(caret)
library(dplyr)
library(raster)
library(rgdal)
library(mailR)
library(microbenchmark)
library(party)

beginCluster()
  complete.dataset.for.sdm.SPECIES<-dplyr::filter(complete.dataset.for.sdm,
                                                  SPEC==SPECIES,
                                                  !is.na(ebird.time))
  #show how many checklists from each data source type (ebird, pointcount, or transect)
  complete.dataset.for.sdm.SPECIES %>% group_by(datasource) %>%dplyr::summarize(n_distinct(SAMPLING_EVENT_ID))
  
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
  
  latlong.predictors.SPECIES.unsplit<-cbind(
                                              "presence" = complete.dataset.for.sdm.SPECIES$presence,
                                    coordinates(complete.dataset.for.sdm.SPECIES),
                                    "effort_time" = complete.dataset.for.sdm.SPECIES$effort_time,
                                    "effort_length" = complete.dataset.for.sdm.SPECIES$effort_length,
                                    "time_of_day" = complete.dataset.for.sdm.SPECIES$ebird.time,
                                    predictors_stack.SPECIES.df,
                                    row.names = NULL)
  #land covers are factors.
  latlong.predictors.SPECIES.unsplit$nlcd_ok_utm14_okmask <- as.factor(latlong.predictors.SPECIES.unsplit$nlcd_ok_utm14_okmask)

  #Run this line only to trim responses if predictor maps do not match up with response data.
  #latlong.predictors.SPECIES.unsplit <- na.omit(latlong.predictors.SPECIES.unsplit)
  set.seed(78)
  train <- createDataPartition(y = latlong.predictors.SPECIES.unsplit$presence,
                              times = 1,
                              p = 0.5,
                              list = FALSE)
  
 
  latlong.predictors.SPECIES <- latlong.predictors.SPECIES.unsplit[train,]
  latlong.predictors.SPECIES.eval <- latlong.predictors.SPECIES.unsplit[-train,]
  
  
  #spatial versions
  latlong.predictors.SPECIES.spatial <- latlong.predictors.SPECIES
  #has to be spatial for function to work so re-add that
  coordinates(latlong.predictors.SPECIES.spatial) <- c("Longitude", "Latitude")
  proj4string(latlong.predictors.SPECIES.spatial)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  #eval
  latlong.predictors.SPECIES.eval.spatial <- latlong.predictors.SPECIES.eval
  #has to be spatial for function to work so re-add that
  coordinates(latlong.predictors.SPECIES.eval.spatial) <- c("Longitude", "Latitude")
  proj4string(latlong.predictors.SPECIES.eval.spatial)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  
  #free up memory
  rm(predictors_stack.SPECIES)  
  rm(predictors_stack.SPECIES.df) 
  rm(complete.dataset.for.sdm.SPECIES)
  gc()
  
  ##################################
  #load functions
  source("source_ensemble_function_support_set_save_objects.R") #create_support_set
  source("source_ensemble_function_spatial_sampling_evaluation.R") #spatial.sampling.evaluation
  ############################
  
  #Run small, medium, and large and get the rasters.  
  #Other files (backup support list, cforest trees with importance, etc, are saved elsewhere.)
  #reload microbenchmarks for compilation at end.
  
  #small
  support.small.ensemble <- create_support_set(numberofpoints = numberofpoints.small,
                                                radius = radius.small,
                                                sizename = "small")
 
  small.microbenchmark1 <- readRDS(file = paste(SPECIES,
                                                 "small",
                                                 "backup_microbenchmark1",
                                                 sep = "_"))
  small.microbenchmark2 <- readRDS(file = paste(SPECIES,
                                                 "small",
                                                 "backup_microbenchmark2",
                                                 sep = "_"))
                                   
  support.medium.ensemble <- create_support_set(numberofpoints = numberofpoints.medium,
                                               radius = radius.medium,
                                               sizename = "medium")
  medium.microbenchmark1 <- readRDS(file = paste(SPECIES,
                                                 "medium",
                                                 "backup_microbenchmark1",
                                                 sep = "_"))
  medium.microbenchmark2 <- readRDS(file = paste(SPECIES,
                                                 "medium",
                                                 "backup_microbenchmark2",
                                                 sep = "_"))
  
  support.large.ensemble <- create_support_set(numberofpoints = numberofpoints.large,
                                               radius = radius.large,
                                               sizename = "large")
  large.microbenchmark1 <- readRDS(file = paste(SPECIES,
                                                 "large",
                                                 "backup_microbenchmark1",
                                                 sep = "_"))
  large.microbenchmark2 <- readRDS(file = paste(SPECIES,
                                                 "large",
                                                 "backup_microbenchmark2",
                                                 sep = "_"))
  
  gc()
  
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
  microbenchmark.statewide1 <- microbenchmark (tree.statewide <- randomForest(presence ~ ., 
                                   data = statewide.data,
                                   ntree = ntree,
                                   replace = FALSE, #strobl et al. 2007
                                   importance = TRUE),
                                   times = 1)
  microbenchmark.statewide2 <- microbenchmark (
  tree.statewide.raster.prediction.prob <- raster::predict(predictors_stack_with_all_variables,
                                                           model = tree.statewide,
                                                           progress = "text",
                                                           filename = paste0(
                                                                             SPECIES,
                                                                             "_products_statewide.raster.prediction.prob",
                                                                             ".tif"),
                                                           format="GTiff",
                                                           overwrite = TRUE),
  times = 1)
  endCluster()
  microbenchmark.statewide1$model <- "statewide1"
  microbenchmark.statewide2$model <- "statewide2"
  microbenchmarks.statewide <- rbind(microbenchmark.statewide1,
                                     microbenchmark.statewide2)
  saveRDS(microbenchmarks.statewide,
          file = paste0(SPECIES,
                        "backup_microbenchmarks_statewide"))

  
  #statewide variable importance.
  
  #plot of variable importance from RF tree.
  svg(file = paste0(SPECIES,
                    "_statewide_products_varimpplot",
                    ".svg"), 
      width = 10,#plot.width,
      height = 8)#plot.height)
  varImpPlot(tree.statewide,
             scale = FALSE)
  dev.off()
  
  #Create a dataframe of these values.
  tree.statewide.varimp <- data.frame(importance(tree.statewide,
                                                 scale = FALSE))
  
  #Go through top 10 variables in partialPlot
  #Get variable importances.
  imp <- importance(tree.statewide, 
                    scale = FALSE)  #scale = false from strobl et al. 2007
  #Order them.
  impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
  #Create svg file of top 10 important variables.
  svg(file = paste0(SPECIES,
                    "_statewide_products_partialplots",
                    ".svg"),
      width = 7, #plot.width,
      height = 10)#plot.height*2)

  par(mfrow = c(5,2))
  for (i in 1:10) {
    partialPlot(tree.statewide,
                statewide.data, 
                impvar[i],
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
                    "_statewide_products_partialplots-cforest",
                    ".svg"),
      width = 7, #plot.width,
      height = 10)#plot.height*2)
  
  par(mfrow = c(5,2))
  for (i in 1:10) {
    partialPlot(tree.statewide,
                statewide.data, 
                varnames.cforest[i],
                xlab=varnames.cforest[i],
                main=paste("Partial Dependence on", varnames.cforest[i]))
  }
  
  dev.off()
  
  #return graphics to 1 x 1 state.
  par(mfrow = c(1,1))
  
  #save variable importance results.
  varimp <- list(tree.statewide,
                 tree.statewide.cforest,
                 imp.cforest,
                 varnames.cforest)
  saveRDS(varimp,
          file = paste0(
                        SPECIES,
                        "_statewide_products_tree_and_varimp"))
  
  #http://stats.stackexchange.com/questions/93202/odds-ratio-from-decision-tree-and-random-forest
  #http://r.789695.n4.nabble.com/randomForest-PartialPlot-reg-td2551372.html shoudl not do the logit thing actually
  #Just go for target class (I want presence ie "1") and interpret higher as more likely.
  
  #http://stackoverflow.com/questions/32606375/rmse-calculation-for-random-forest-in-r
  
  #for spatially uniform test data
  #http://stackoverflow.com/questions/32862606/taking-random-point-from-list-of-points-per-grid-square
  #Do this sampling n times (200?  250?)

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
  statewide.sampling.rmse.diffyear <- replicate(50,
                                       expr = do.call (what = spatial.sampling.evaluation,
                                                       args = list(evaluation.spatial,
                                                                   cell.size,
                                                                   n,
                                                                   typeofeval = "rmse",
                                                                   prediction.raster = tree.statewide.raster.prediction.prob)))
  
  statewide.sampling.rmse.sameyear <- replicate(50,
                                       expr = do.call (what = spatial.sampling.evaluation,
                                                       args = list(latlong.predictors.SPECIES.eval.spatial,
                                                                   cell.size,
                                                                   n,
                                                                   typeofeval = "rmse",
                                                                   prediction.raster = tree.statewide.raster.prediction.prob)))
  
  
  #Then repeat for AUC
  statewide.sampling.auc.diffyear <- replicate(50,
                                      expr = do.call (what = spatial.sampling.evaluation,
                                                      args = list(evaluation.spatial,
                                                                  cell.size,
                                                                  n,
                                                                  typeofeval = "auc",
                                                                  prediction.raster = tree.statewide.raster.prediction.prob)))
  statewide.sampling.auc.sameyear <- replicate(50,
                                                expr = do.call (what = spatial.sampling.evaluation,
                                                                args = list(latlong.predictors.SPECIES.eval.spatial,
                                                                            cell.size,
                                                                            n,
                                                                            typeofeval = "auc",
                                                                            prediction.raster = tree.statewide.raster.prediction.prob)))
  
  
  #small
  #rmse
  small.sampling.rmse.diffyear <- replicate(50,
                                            expr = do.call (what = spatial.sampling.evaluation,
                                                            args = list(evaluation.spatial,
                                                                        cell.size,
                                                                        n,
                                                                        typeofeval = "rmse",
                                                                        prediction.raster = support.small.ensemble)))
  
  small.sampling.rmse.sameyear <- replicate(50,
                                            expr = do.call (what = spatial.sampling.evaluation,
                                                            args = list(latlong.predictors.SPECIES.eval.spatial,
                                                                        cell.size,
                                                                        n,
                                                                        typeofeval = "rmse",
                                                                        prediction.raster = support.small.ensemble)))
  #Then repeat for AUC
  small.sampling.auc.diffyear <- replicate(50,
                                           expr = do.call (what = spatial.sampling.evaluation,
                                                           args = list(evaluation.spatial,
                                                                       cell.size,
                                                                       n,
                                                                       typeofeval = "auc",
                                                                       prediction.raster = support.small.ensemble)))
  small.sampling.auc.sameyear <- replicate(50,
                                           expr = do.call (what = spatial.sampling.evaluation,
                                                           args = list(latlong.predictors.SPECIES.eval.spatial,
                                                                       cell.size,
                                                                       n,
                                                                       typeofeval = "auc",
                                                                       prediction.raster = support.small.ensemble)))
  
  

  
  
  #medium
  #rmse
  medium.sampling.rmse.diffyear <- replicate(50,
                                            expr = do.call (what = spatial.sampling.evaluation,
                                                            args = list(evaluation.spatial,
                                                                        cell.size,
                                                                        n,
                                                                        typeofeval = "rmse",
                                                                        prediction.raster = support.medium.ensemble)))
  
  medium.sampling.rmse.sameyear <- replicate(50,
                                            expr = do.call (what = spatial.sampling.evaluation,
                                                            args = list(latlong.predictors.SPECIES.eval.spatial,
                                                                        cell.size,
                                                                        n,
                                                                        typeofeval = "rmse",
                                                                        prediction.raster = support.medium.ensemble)))
  #Then repeat for AUC
  medium.sampling.auc.diffyear <- replicate(50,
                                           expr = do.call (what = spatial.sampling.evaluation,
                                                           args = list(evaluation.spatial,
                                                                       cell.size,
                                                                       n,
                                                                       typeofeval = "auc",
                                                                       prediction.raster = support.medium.ensemble)))
  medium.sampling.auc.sameyear <- replicate(50,
                                           expr = do.call (what = spatial.sampling.evaluation,
                                                           args = list(latlong.predictors.SPECIES.eval.spatial,
                                                                       cell.size,
                                                                       n,
                                                                       typeofeval = "auc",
                                                                       prediction.raster = support.medium.ensemble)))
  
  
  #large
  #rmse
  large.sampling.rmse.diffyear <- replicate(50,
                                   expr = do.call (what = spatial.sampling.evaluation,
                                                   args = list(evaluation.spatial,
                                                               cell.size,
                                                               n,
                                                               typeofeval = "rmse",
                                                               prediction.raster = support.large.ensemble)))
  
  large.sampling.rmse.sameyear <- replicate(50,
                                            expr = do.call (what = spatial.sampling.evaluation,
                                                            args = list(latlong.predictors.SPECIES.eval.spatial,
                                                                        cell.size,
                                                                        n,
                                                                        typeofeval = "rmse",
                                                                        prediction.raster = support.large.ensemble)))
  #Then repeat for AUC
  large.sampling.auc.diffyear <- replicate(50,
                                  expr = do.call (what = spatial.sampling.evaluation,
                                                  args = list(evaluation.spatial,
                                                              cell.size,
                                                              n,
                                                              typeofeval = "auc",
                                                              prediction.raster = support.large.ensemble)))
  large.sampling.auc.sameyear <- replicate(50,
                                            expr = do.call (what = spatial.sampling.evaluation,
                                                            args = list(latlong.predictors.SPECIES.eval.spatial,
                                                                        cell.size,
                                                                        n,
                                                                        typeofeval = "auc",
                                                                        prediction.raster = support.large.ensemble)))
  
  
  #print figures of both
  svg(file = paste0(SPECIES,
                    "products_RMSE",
                    ".svg"), 
      width = plot.width,
      height = plot.height)
  par(mfrow=c(2,1))
  boxplot(cbind(
                "Small, same years" = small.sampling.rmse.sameyear,
                "Medium, same years" = medium.sampling.rmse.sameyear,
                "Large, same years" = large.sampling.rmse.sameyear,
                "Statewide, same years" = statewide.sampling.rmse.sameyear),
          xlab = "",
          ylab = "RMSE",
          notch = TRUE)
  boxplot(cbind(
                "Small, same years" = small.sampling.rmse.sameyear,
                "Medium, same years" = medium.sampling.rmse.sameyear,
                "Large, same years" = large.sampling.rmse.sameyear,
                "Statewide, same years" = statewide.sampling.rmse.sameyear),
          xlab = "Support set size and evaluation type",
          ylab = "RMSE",
          notch = TRUE)
  dev.off()
  svg(file = paste0(SPECIES,
                    "products_AUC",
                    ".svg"), 
      width = plot.width,
      height = plot.height)
  par(mfrow=c(2,1))
  boxplot(cbind("Small, previous" = small.sampling.auc.diffyear,
                "Medium, previous" = medium.sampling.auc.diffyear,
                "Large, previous" = large.sampling.auc.diffyear,
                "Statewide, previous" = statewide.sampling.auc.diffyear
                ),
          xlab = "",
          ylab = "AUC",
          notch = TRUE)
  boxplot(cbind(
                "Small, same" = small.sampling.auc.sameyear,
                "Medium, same" = medium.sampling.auc.sameyear,
                "Large, same" = large.sampling.auc.sameyear,
                "Statewide, same" = statewide.sampling.auc.sameyear),
          xlab = "Support set size, years",
          ylab = "AUC",
          notch = TRUE)
  dev.off()
  ####################

  #Create a final microbenchmarks file.
  microbenchmarks <- rbind(microbenchmark.statewide1,
                           large.microbenchmark1,
                           medium.microbenchmark1,
                           small.microbenchmark1,
                           microbenchmark.statewide2,
                           large.microbenchmark2,
                           medium.microbenchmark2,
                           small.microbenchmark2)
  
  microbenchmarks$Species <- SPECIES
  
  write.csv(microbenchmarks,
            file = paste0(SPECIES,
                          "_products_microbenchmarks.csv"))
  #####################
  eval.results <- list(statewide.sampling.rmse.sameyear,
                  statewide.sampling.auc.sameyear,
                  small.sampling.rmse.sameyear,
                  small.sampling.auc.sameyear,
                  medium.sampling.rmse.sameyear,
                  medium.sampling.auc.sameyear,
                  large.sampling.rmse.sameyear,
                  large.sampling.auc.sameyear,
                  statewide.sampling.rmse.diffyear,
                  statewide.sampling.auc.diffyear,
                  small.sampling.rmse.diffyear,
                  small.sampling.auc.diffyear,
                  medium.sampling.rmse.diffyear,
                  medium.sampling.auc.diffyear,
                  large.sampling.rmse.diffyear,
                  large.sampling.auc.diffyear)
  saveRDS(eval.results,
          file = paste0(SPECIES,
                        "_products_evaluation_results"))
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
  #remove any remaining species-specific objects.
  rm(latlong.predictors.SPECIES,
     latlong.predictors.SPECIES.spatial,
     polys.small,
     polys.small.p,
     polys.small.df,
     microbenchmark.small,
     support.small.ensemble,
     polys.medium,
     polys.medium.p,
     polys.medium.df,
     microbenchmark.medium,
     support.medium.ensemble,
     polys.large,
     polys.large.p,
     polys.large.df,
     microbenchmark.large,
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
  #Delete all remaining temporary file directory at end of species processing.
  #http://stackoverflow.com/questions/18955305/setting-an-overwriteable-temporary-file-for-rasters-in-r?noredirect=1&lq=1
  unlink(file.path(getwd(),"rastertemp"),
         recursive = TRUE)


