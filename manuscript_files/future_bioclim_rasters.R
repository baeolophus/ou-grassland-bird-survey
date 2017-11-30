



#statewide

#load trees
statewide_products_tree_and_varimp <- readRDS(paste0(SPECIES,
                                                     "_statewide_products_tree_and_varimp"))
#1 is tree.statewide,
#2 is tree.statewide.cforest,
#3 is imp.cforest,
#4 is varnames.cforest
tree.statewide <- statewide_products_tree_and_varimp[[1]]
#run.
rasterOptions()$tmpdir
rasterOptions(tmpdir=paste0(getwd(),
                            "/rastertemp/",
                            SPECIES,
                            "/statewide"))

tempdir() #check for correct temporary directory from new .Renviron file placed in home directory.

send.mail(from = sender,
          to = recipients,
          subject = paste0("statewide starts for ",
                           SPECIES),
          body = "hope it works eh",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "curryclairem.mail@gmail.com",            
                      passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
microbenchmark.statewide.future <- microbenchmark (
  future.tree.statewide.raster.prediction.prob <- raster::predict(future_predictors_stack_with_all_variables,
                                                                  model = tree.statewide,
                                                                  progress = "text",
                                                                  filename = paste0(SPECIES,
                                                                                    "_products_future_statewide_raster_prediction_prob",
                                                                                    ".tif"),
                                                                  format="GTiff",
                                                                  overwrite = TRUE),
  times = 1)

microbenchmark.statewide.future$model <- "statewidefuture2"
microbenchmark.statewide.future$Species <- SPECIES

saveRDS(microbenchmark.statewide.future,
        file = paste0(SPECIES,
                      "_product_microbenchmarks_statewide_future"))

write.csv(microbenchmark.statewide.future,
          file = paste0(SPECIES,
                        "_product_microbenchmarks_statewide_future.csv"))

send.mail(from = sender,
          to = recipients,
          subject = paste0("statewide is complete for ",
                           SPECIES),
          body = "Go download files!  Onward!",
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "curryclairem.mail@gmail.com",            
                      passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)

future_processing <- function (
  predictor_stack,
  SPECIES,
  sizename_for_reading,
  sizename  #for writing
) {
  
  #load function to generate rasters when current trees are already made.
  source("source_ensemble_function_treesdone_future_rasters.R") #trees.already.done.raster.generation
  source("source_ensemble_function_fromtreesdone_support_set_ensemble_mosaic.R") #ensemble.function
  environment(trees.already.done.raster.generation) <- environment()
  environment(ensemble.function) <- environment()
  
  
  #load files generated from current tree models.
  
  polys <- readRDS(paste(SPECIES,
                         sizename_for_reading,
                         "intermediatefile",
                         "polys",
                         sep = "_"))
  
  polys.p <- unlist(polys[[1]])
  polys.df <- unlist(polys[[2]])
  
  
  support_set <- readRDS(paste0(SPECIES,
                                "_", 
                                sizename_for_reading,
                                "_",
                                "intermediates_support_list"))
  
  #create temporary raster files in a folder that can be deleted as the intermediate ones won't be needed later
  rasterOptions()$tmpdir
  rasterOptions(tmpdir=paste0(getwd(),
                              "/rastertemp/",
                              SPECIES,
                              "/",
                              sizename_for_reading))
  send.mail(from = sender,
            to = recipients,
            subject = paste0(sizename,
                             " ensemble is starting for ",
                             SPECIES),
            body = "Save the before and after in case microbenchmark crashes.",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
  #spatial support sets contain these:
  #list(tree.test.raster.prediction.extended,
  #    sample.size.good,
  #     tree.test)
  tree.list0 <- lapply(support_set,
                       "[",
                       3) #get trees, 3rd in list
  #remove one level of list on the remaining support sets
  tree.list<-lapply(tree.list0,
                    "[[",
                    1)
  
  weights <- lapply(support_set,
                    "[",
                    2) #get weights, 2nd in list
  
  support.rasters <- lapply(1:length(polys.df),
                            FUN = trees.already.done.raster.generation,
                            predictor_stack,
                            polys.df,
                            trees = tree.list)
  
  #rebuild list so ensemble mosaic can be created from new future predictions based on current distribution models.
  support.list <- list (support.rasters,
                        weights,
                        tree.list)
  
  print("ensembling now")
  microbenchmarkfuture2 <- microbenchmark(support.ensemble <- ensemble.function(support.list),
                                          times = 1)
  
  
  #report microbenchmark values
  microbenchmarkfuture2$model <- paste0(sizename, "2")
  saveRDS(microbenchmarkfuture2,
          file = paste(SPECIES,
                       sizename,
                       "product_microbenchmark2",
                       sep = "_"))
  

  microbenchmarkfuture2$Species <- SPECIES
  
  write.csv(microbenchmarkfuture2,
            file = paste0(SPECIES,
                          "_",
                          sizename,
                          "_products_microbenchmarks_future.csv"))
  
  send.mail(from = sender,
            to = recipients,
            subject = paste0(sizename,
                             " ensemble is complete for ",
                             SPECIES),
            body = "Go download files!  Onward!",
            smtp = list(host.name = "smtp.gmail.com", port = 465, 
                        user.name = "curryclairem.mail@gmail.com",            
                        passwd = "J9YgBkY5wxJhu5h90rKu", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
  rm(support.list)
  gc()
  
  unlink(file.path(getwd(),
                   "rastertemp",
                   SPECIES,
                   "/",
                   sizename_for_reading),
         recursive = TRUE)
  
  return(support.ensemble)
  
}



future_large <- future_processing (predictor_stack = future_predictors_stack_with_all_variables,
                                        SPECIES,
                                        sizename_for_reading = "large",
                                        sizename = "large_future")

future_medium <- future_processing (predictor_stack = future_predictors_stack_with_all_variables,
                                         SPECIES,
                                         sizename_for_reading = "medium",
                                         sizename = "medium_future")

future_small <- future_processing (predictor_stack = future_predictors_stack_with_all_variables,
                                        SPECIES,
                                        sizename_for_reading = "small",
                                        sizename = "small_future")


#threshold calculations
source("threshold_calculations.R")
stateraster <- raster(paste0(
  SPECIES,
  "_products_future_statewide_raster_prediction_prob.tif"))
smallraster <- raster(paste0(
  SPECIES,
  "_small_future_products_ensembleweightedmosaic.tif"))

mediumraster <- raster(paste0(
  SPECIES,
  "_medium_future_products_ensembleweightedmosaic.tif"))
largeraster <- raster(paste0(
  SPECIES,
  "_large_future_products_ensembleweightedmosaic.tif"))

small.area <- thresholds(SPECIES,
                         "_small_future_products_ensembleweightedmosaic.tif",
                         0.5)

medium.area <- thresholds(SPECIES,
                          "_medium_future_products_ensembleweightedmosaic.tif",
                          0.5)

large.area <- thresholds(SPECIES,
                         "_large_future_products_ensembleweightedmosaic.tif",
                         0.5)

statewide.area <- thresholds(SPECIES,
                             "_products_future_statewide_raster_prediction_prob.tif",
                             0.5)

future.areas <- data.frame("areakm2" = rbind(small.area,
                      medium.area,
                      large.area,
                      statewide.area))

future.areas$Species <- SPECIES
future.areas$model <- "future"
future.areas$threshold <- 0.5 #0.5 = top 50% of range.

write.csv(future.areas,
          file = paste0(SPECIES,
                        "_products_future_map_areas.csv"))

