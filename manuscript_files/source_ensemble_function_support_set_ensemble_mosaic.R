#Ensemble function
library(raster)

############################
#assemble the support set lists into a model prediction surface.
#Mosaic function
ensemble.function <- function (list.of.rasters) {
  weights<-lapply(list.of.rasters,
                  "[",
                  2)
  weights<-as.vector(unlist(weights))
  #The weights are 1 (have enough samples) and 0 (do not have enough samples, do not use)
  #For use in weighted.mean below.  It is a vector of weights per layer.  Since values are 1 if enough sample
  #then all are weighted equally if they are used.  If 0 (not enough sample size), then 0 weighted, not used.
  
  #remove one level of list on the remaining support sets
  predictions.support.sets<-unlist(lapply(list.of.rasters,
                                          "[",
                                          1))
  
  #Stack all the support sets (this will work because they have been extended with NA in non-support-set regions)
  predictions.support.sets.stacked<-raster::stack(predictions.support.sets)
  
  
  #Run the ensemble by using weighted mean.  The weight is by number of support sets.
  ensemble.weighted.mosaic <- raster::weighted.mean(predictions.support.sets.stacked,
                                                    weights,
                                                    na.rm = TRUE,
                                                    filename = paste0(SPECIES,
                                                                     "_",
                                                                      sizename,
                                                           "_products_ensembleweightedmosaic",
                                                           ".tif"),
                                                    format="GTiff",
                                                    overwrite = TRUE,
                                                    progress = "text")



  return(ensemble.weighted.mosaic)
}