#Now, function for subsetting and running the test on each subset.

spatial.support.set<-function(whichrandombox,
                              spatialdataset,
                              predictor_stack,
                              polys.df,
                              ...){

  spatial.support.set<-spatialdataset[polys.df[whichrandombox,],]
  sample.size.good<-ifelse(length(spatial.support.set$presence)>25 &
                             length(unique(spatial.support.set$presence))>1,
                           1, #if both conditions met for sample size and both 0/1s are present
                           0) #if not, do not use (0 weight in ensemble step)
  #need to have the minimum data requirement in here too.
  support.set.data <- as.data.frame(spatial.support.set)
  support.set.data$Longitude <- NULL
  support.set.data$Latitude <- NULL
  #These two columns should be taken out because not predicting on them.
  
  library(randomForest)
  tree.test <- randomForest(presence ~ ., 
                            data = support.set.data,
                            replace = FALSE,
                            ...) #This allows all other random forest arguments to be set at the spatial.support.set function level.
 

  support.set <- crop(predictor_stack,
                      extent(polys.df[whichrandombox,]))

  tree.test.raster.prediction <-  raster::predict(object = support.set,
                                                 model = tree.test,
                                               #  type = "prob",
                                                 progress = "text",
                                                 filename = paste0(SPECIES,
                                                                   "_",
                                                                   sizename,
                                                                   "_treetestrasterpredictionextended",
                                                                   whichrandombox,
                                                                   ".tif"),
                                                 format = "GTiff",
                                                 overwrite = TRUE)

  tree.test.raster.prediction.extended <- raster::extend(x = tree.test.raster.prediction,
                                                         y = studyarea.extent,
                                                         value = NA)

  library(party)
  #Then variable importance in cforest (Strobl et al. papers on bias).
  my_cforest_control <- cforest_control(teststat = "quad",
                                        testtype = "Univ",
                                        mincriterion = 0, #max depth
                                        ntree = ntree, 
                                        mtry = floor(sqrt(ncol(spatialdataset)))-1,
                                        replace = FALSE)
  
  cforest_importance_tree <- cforest(presence ~ .,
                                    data = support.set.data,
                                    controls = my_cforest_control)
  
  imp.cforest <- as.data.frame(varimp(cforest_importance_tree))
  ordered.varnames.cforest <- rownames(imp.cforest)[order(imp.cforest, decreasing=TRUE)]

  results.support.set <- list(tree.test.raster.prediction.extended,
                              sample.size.good,
                              tree.test,
                              cforest_importance_tree,
                              imp.cforest,
                              ordered.varnames.cforest)
  return(results.support.set)


}
