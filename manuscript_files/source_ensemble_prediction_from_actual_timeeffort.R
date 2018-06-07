#load trees
loadRDS(file = paste0(#filepath here
  species,
  size,
  "_intermediates_support_list"))

########################
#load spatial dataframe that contains all predictors extracted from predictors stack, 
#plus the actual presence/absence data


################################
#Repeat 50 x
################################
########################
#Subsample evaluation dataset by whole-area grid
#Spatial sampling function
library(GSIF)
library(ROCR)

# spatial.sampling.evaluation <- function (evaluation.spatial,
#                                          cell.size,
#                                          n,
#                                          typeofeval, #typeofeval is "rmse" or "auc" for performance function.
#                                          prediction.raster) { #the model full prediction map
#   
list.of.object.spdf.and.grid <- sample.grid(evaluation.spatial, #spatial dataframe
                                            cell.size, #in meters
                                            n) #maximum number of samples per grid cell

#THE spatially subsampled DATASET ACROSS THE WHOLE STATE
iteration.testing.spatial <- list.of.object.spdf.and.grid[[1]]


#replace this with new predictions for each value in the spatial testing dataset (iteration.testing.spatial).
#load evaluation dataset that has extracted predictors except for time and effort.
#merge the spatial subset with each predictor value for it.

########################
#Then cycle through each support set and predict.
#loop or function here

#crop the spatial dataframe to the extent of each support set
eval.support.set <- crop(iteration.testing.spatial,
                         extent(polys.df[whichrandombox,]))

#make predictions on that
tree.prediction <-  predict( model = tree.test,
                             newdata = eval.support.set)

#end loop or vectorized function
########################

#Merge all predictions (which should perhaps go in a list?)
#Any spatially identical predictions are averaged between columns.
model.predictions <- dplyr::full_join(tree.prediction,
                                      by = c("x", "y"))

########################
#Take final model predictions and label with actual/presence absence.

pred <- prediction(predictions = model.predictions,#model.predictions.presence,
                   labels = iteration.testing.spatial$presence) #evaluation.dataset$presence)

perf <- performance(pred,
                    typeofeval)
perf@y.values[[1]]
################################
#end 50x repeat
