cforest.support.set<-function(whichrandombox,
                              spatialdataset,
                              predictor_stack,
                              sizename,
                              ...){
  
  polys <- readRDS(file = paste(SPECIES,
                                sizename,
                                "intermediatefile",
                                "polys",
                                sep = "_"))
  
  polys.p <- unlist(polys[[1]])
  polys.df <- unlist(polys[[2]])
  
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
  
    support.set <- crop(predictor_stack,
                      extent(polys.df[whichrandombox,]))
  
  
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
  
  results.cforest <- list(cforest_importance_tree,
                          imp.cforest,
                          ordered.varnames.cforest)
  return(results.cforest)
 
}

