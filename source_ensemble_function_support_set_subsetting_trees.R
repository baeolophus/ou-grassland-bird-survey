#Now, function for subsetting and running the test on each subset.

spatial.support.set<-function(whichrandombox,
                              spatialdataset,
                              predictor_stack,
                              polys.df,
                              ...){

  spatial.support.set<-spatialdataset[polys.df[whichrandombox,],]
  #I think there should be a line that turns it back into regular data?
  sample.size.good<-ifelse(length(spatial.support.set)>25, 1, 0)
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
 
  saveRDS(tree.test,
          file = paste0(SPECIES,
                        "tree",
                        whichrandombox,
                        deparse(substitute(polys.df))
                        )
          )
  support.set <- crop(predictor_stack,
                      extent(polys.df[whichrandombox,]))

  tree.test.raster.prediction <-  raster::predict(object = support.set,
                                                 model = tree.test,
                                               #  type = "prob",
                                                 progress = "text",
                                                 filename = paste0(SPECIES,
                                                                   "_treetestrasterpredictionextended",
                                                                   deparse(substitute(polys.df)),
                                                                   whichrandombox,
                                                                   ".tif"),
                                                 format = "GTiff",
                                                 overwrite = TRUE)

  tree.test.raster.prediction.extended <- raster::extend(x = tree.test.raster.prediction,
                                                         y = studyarea.extent,
                                                         value = NA)

  return(list(tree.test.raster.prediction.extended,
              sample.size.good))


}
