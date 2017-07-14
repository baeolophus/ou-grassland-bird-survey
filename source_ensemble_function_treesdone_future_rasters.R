#Now, function for subsetting and running the test on each subset.

trees.already.done.raster.generation <-function(whichrandombox,
                                                predictor_stack,
                                                polys.df,
                                                trees,
                                                ...){
  
  support.set <- crop(predictor_stack,
                      extent(polys.df[whichrandombox,]),
                      progress = "text")
  print(paste0("predicting", whichrandombox))
  tree.test.raster.prediction <-  raster::predict(object = support.set,
                                                  model = trees[[whichrandombox]],
                                                  progress = "text",
                                                  filename = paste0(SPECIES,
                                                                    "_",
                                                                    sizename,
                                                                    "_treetestrasterpredictionextended",
                                                                    whichrandombox,
                                                                    ".tif"),
                                                  format = "GTiff",
                                                  overwrite = TRUE)
  print(paste0("extending", whichrandombox))
  tree.test.raster.prediction.extended <- raster::extend(x = tree.test.raster.prediction,
                                                         y = studyarea.extent,
                                                         value = NA)
  
  
  results.support.set <- tree.test.raster.prediction.extended
  return(results.support.set)
  
}