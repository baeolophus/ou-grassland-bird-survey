#Spatial sampling function
library(GSIF)
library(ROCR)

spatial.sampling.evaluation <- function (evaluation.spatial,
                                         cell.size,
                                         n,
                                         typeofeval, #typeofeval is "rmse" or "auc" for performance function.
                                         prediction.raster) { #the model full prediction map
  
  list.of.object.spdf.and.grid <- sample.grid(evaluation.spatial, #spatial dataframe
                                              cell.size, #maybe 10 or 100 km?
                                              n) #maximum number of samples per grid cell
  
  iteration.testing.spatial <- list.of.object.spdf.and.grid[[1]]
  
  model.predictions <- extract(x = prediction.raster,
                               #the raster containing predictions from which values are extracted
                               y = iteration.testing.spatial
                               #spatial.evaluation.dataset
  )
  
  #model.predictions.presence <- as.data.frame(model.predictions$presence)
  
  pred <- prediction(predictions = model.predictions,#model.predictions.presence,
                     labels = iteration.testing.spatial$presence) #evaluation.dataset$presence)
  
  perf <- performance(pred,
                      typeofeval)
  return(perf@y.values[[1]])
}