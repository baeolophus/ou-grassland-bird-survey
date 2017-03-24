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
  beginCluster() #use raster's multicore clustering to automatically use more cores and speed up predict and extend
  tree.test.raster.prediction <- clusterR(support.set,
                                          fun = raster::predict,
                                          args = list(model = tree.test))
  endCluster()
  tree.test.raster.prediction.extended <- raster::extend(x = tree.test.raster.prediction,
                                                         y = studyarea.extent,
                                                         value = NA)
  writeRaster(tree.test.raster.prediction.extended,
             filename = paste0(SPECIES,
                                "_treetestrasterpredictionextended",
                                deparse(substitute(polys.df)),
                                whichrandombox,
                                ".tif"),
              format="GTiff",
              overwrite = TRUE)
  #remove all temporary files.
  removeTmpFiles(h=0)

}