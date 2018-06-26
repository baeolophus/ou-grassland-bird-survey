library(caret)
library(data.table)
library(dplyr)
library(GSIF)
library(party)
library(randomForest)
library(ROCR)

#load spatial dataframe that contains all predictors extracted from predictors stack, 
#plus the actual presence/absence data
#Bring in predictor data.
#downscaled ones
source("manuscript_files/source_ensemble_predictor_import.R")

#Responses
#Bring in whole response data set (with NA lat/long already removed) as a spatial object including presence/absence.
#Is already in utm
#bring in evaluation dataset and make it spatial object
complete.dataset.for.sdm <- read.csv(file = "manuscript_files/oklahomadatasetforsdm_naomit_utm.csv")

#load list of species to use
specieslist <- c("NOBO",
                 "UPSA",
                 "HOLA",
                 "CASP",
                 "FISP",
                 "LASP",
                 "GRSP",
                 "DICK",
                 "EAME",
                 "WEME",
                 "BHCO")
################################
#Predict from support set scaled models
################################
predict_from_done_trees <- function (SPECIES, size, typeofeval) {
  
  #load trees
trees <- readRDS(file = paste0(#filepath here
  "/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Downscale_current/",
  #  "/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current/",
  "/",
  SPECIES,
  "/",
  SPECIES,
  "_",
  size,
  "_intermediates_support_list"))
polys <- readRDS(file = paste0(#filepath here
  "/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Downscale_current/",
  #  "/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current/",
  "/",
  SPECIES,
  "/",
  SPECIES,
  "_",
  size,
  "_intermediatefile_polys"))
polys.df <- polys[[2]]

########################

#Import dataset and filter by species and having time of day.
complete.dataset.for.sdm.SPECIES<-dplyr::filter(complete.dataset.for.sdm,
                                                SPEC==SPECIES,
                                                !is.na(ebird.time))

#show how many checklists from each data source type (ebird, pointcount, or transect)
complete.dataset.for.sdm.SPECIES %>% group_by(datasource) %>%dplyr::summarize(n_distinct(SAMPLING_EVENT_ID))

#make it spatial and set coordinate system appropriately.
coordinates(complete.dataset.for.sdm.SPECIES)<-c("Longitude", "Latitude")
#make it spatial
proj4string(complete.dataset.for.sdm.SPECIES)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
#check it worked
proj4string(complete.dataset.for.sdm.SPECIES)
#extract values for analysis
predictors_stack.SPECIES<-extract(x = predictors_stack,
                                  y = complete.dataset.for.sdm.SPECIES)
#convert to data frame
predictors_stack.SPECIES.df <- as.data.frame(predictors_stack.SPECIES)

#add in effort, time of day, and presence/absence.
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
#ie for coarse data
latlong.predictors.SPECIES.unsplit <- na.omit(latlong.predictors.SPECIES.unsplit)

#set seed for reproducible split
set.seed(78)
train <- createDataPartition(y = latlong.predictors.SPECIES.unsplit$presence,
                             times = 1,
                             p = 0.5,
                             list = FALSE)

#split into training and evaluation (.eval) sets. 
latlong.predictors.SPECIES <- latlong.predictors.SPECIES.unsplit[train,]
latlong.predictors.SPECIES.eval <- latlong.predictors.SPECIES.unsplit[-train,]
latlong.predictors.SPECIES.eval$rownums <- rownames(latlong.predictors.SPECIES.eval)
#spatial version of evaluation dataset
latlong.predictors.SPECIES.eval.spatial <- latlong.predictors.SPECIES.eval
coordinates(latlong.predictors.SPECIES.eval.spatial) <- c("Longitude", "Latitude")
proj4string(latlong.predictors.SPECIES.eval.spatial)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))


#free up memory
rm(predictors_stack.SPECIES)  
rm(predictors_stack.SPECIES.df) 
rm(complete.dataset.for.sdm.SPECIES)
gc()

################################
#Repeat 50 x
################################
list_of_eval_from_support_set <- replicate(50, expr = {
########################
#Subsample evaluation dataset by whole-area grid
#Spatial sampling function

#evaluation grid size
cell.size <- c(10000, 10000)
n <- 10 #number of samples to take from each evaluation grid
# spatial.sampling.evaluation <- function (evaluation.spatial,
#                                          cell.size,
#                                          n,
#                                          typeofeval, #typeofeval is "rmse" or "auc" for performance function.
#                                          prediction.raster) { #the model full prediction map
#   
list.of.object.spdf.and.grid <- sample.grid(latlong.predictors.SPECIES.eval.spatial,
                                            #spatial dataframe
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
print(paste(SPECIES,
            size,
            typeofeval,
            "loop"))
tree.prediction <- list()
for (whichrandombox in 1:length(polys.df)) {
  #crop the spatial dataframe to the extent of each support set
eval.support.set <- crop(iteration.testing.spatial,
                         extent(polys.df[whichrandombox,]))
eval.df <- data.frame(eval.support.set)
eval.df.pred.only <- eval.df[,4:(ncol(eval.df)-1)]
#make predictions on that

rf <- trees[[whichrandombox]][[3]]
use <- trees[[whichrandombox]][[2]]

tree.predict <-  predict( object = rf,
                             newdata = eval.df.pred.only)
support.df <- data.frame("pred" = tree.predict,
                         "use" = use,
                         "Longitude" = eval.df$Longitude,
                         "Latitude" = eval.df$Latitude,
                         "rownums" = as.numeric(as.character(eval.df$rownums))
                         )
tree.prediction[[whichrandombox]] <- support.df
}
#end loop or vectorized function
########################

#Merge all predictions (which should perhaps go in a list?)
#Any spatially identical predictions are averaged between columns.

model.predictions <- rbindlist(tree.prediction) %>% 
  dplyr::filter(use == 1)%>%
  group_by(rownums)%>%
  arrange(rownums) %>%
  summarize(mean_pixel_pred = mean(pred, 
                                   na.rm = TRUE))

########################
#Take final model predictions and label with actual/presence absence.
#If predictions don't cover entire area, need to only check for labels at the rows where it exists.
it.test <- iteration.testing.spatial@data[, c("presence", "rownums")]
it.test$rownums <- as.numeric(it.test$rownums)
eval_combined_set <- left_join(model.predictions,
          it.test,
          by = c("rownums"))

pred <- prediction(predictions = eval_combined_set$mean_pixel_pred,#model.predictions.presence,
                   labels = eval_combined_set$presence) #evaluation.dataset$presence)


perf <- performance(pred,
                    typeofeval)


evalue <- perf@y.values[[1]]
return(evalue)
################################
#end 50x repeat
})
df <- data.frame("evalue" = list_of_eval_from_support_set,
                 "species" = SPECIES,
                 "scale" = size,
                 "errortype" = typeofeval,
                 stringsAsFactors = FALSE)
print(paste(SPECIES,
            size,
            typeofeval,
            "made it to data frame at end"))
return(df)
}

listofeval.small.rmse <- lapply (specieslist,
                      FUN = predict_from_done_trees,
                      size = "small",
                      typeofeval = "rmse")
small.rmse <- data.frame(matrix(unlist(listofeval.small.rmse),
                     nrow = 550,
                     ncol = 4, 
                     byrow = TRUE),
                     stringsAsFactors = FALSE)
colnames(small.rmse) <- c("evalue",
                          "species",
                          "scale",
                          "errortype")
small.rmse$evalue <-as.numeric(small.rmse$evalue)


listofeval.medium.rmse <- lapply (specieslist,
                      FUN = predict_from_done_trees,
                      size = "medium",
                      typeofeval = "rmse")
medium.rmse <- data.frame(matrix(unlist(listofeval.medium.rmse),
                                nrow = 550,
                                ncol = 4, 
                                byrow = TRUE),
                          stringsAsFactors = FALSE)
colnames(medium.rmse) <- c("evalue",
                          "species",
                          "scale",
                          "errortype")
medium.rmse$evalue <-as.numeric(medium.rmse$evalue)

listofeval.large.rmse <- lapply (specieslist,
                      FUN = predict_from_done_trees,
                      size = "large",
                      typeofeval = "rmse")
large.rmse <- data.frame(matrix(unlist(listofeval.large.rmse),
                                 nrow = 550,
                                 ncol = 4, 
                                 byrow = TRUE),
                         stringsAsFactors = FALSE)
colnames(large.rmse) <- c("evalue",
                           "species",
                           "scale",
                           "errortype")
large.rmse$evalue <-as.numeric(large.rmse$evalue)

listofeval.small.auc <- lapply (specieslist,
                                 FUN = predict_from_done_trees,
                                 size = "small",
                                 typeofeval = "auc")
small.auc <- data.frame(matrix(unlist(listofeval.small.auc),
                                nrow = 550,
                                ncol = 4, 
                                byrow = TRUE),
                        stringsAsFactors = FALSE)
colnames(small.auc) <- c("evalue",
                          "species",
                          "scale",
                          "errortype")
small.auc$evalue <-as.numeric(small.auc$evalue)

listofeval.medium.auc <- lapply (specieslist,
                                  FUN = predict_from_done_trees,
                                  size = "medium",
                                  typeofeval = "auc")
medium.auc <- data.frame(matrix(unlist(listofeval.medium.auc),
                               nrow = 550,
                               ncol = 4, 
                               byrow = TRUE),
                         stringsAsFactors = FALSE)
colnames(medium.auc) <- c("evalue",
                         "species",
                         "scale",
                         "errortype")

medium.auc$evalue <-as.numeric(medium.auc$evalue)

listofeval.large.auc <- lapply (specieslist,
                                 FUN = predict_from_done_trees,
                                 size = "large",
                                 typeofeval = "auc")
large.auc <- data.frame(matrix(unlist(listofeval.large.auc),
                                nrow = 550,
                                ncol = 4, 
                                byrow = TRUE),
                        stringsAsFactors = FALSE)
colnames(large.auc) <- c("evalue",
                          "species",
                          "scale",
                          "errortype")
large.auc$evalue <-as.numeric(large.auc$evalue)
################################
################################
#New evaluations from statewide
################################
#Predictors and dataset already loaded above.

################################
#Function for going through all species
predict_from_done_state_trees <- function (SPECIES, size, typeofeval) {
  
  #load trees
  trees <- readRDS(file = paste0(#filepath here
      "/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Downscale_current/",
    #"/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current/",
    "/",
    SPECIES,
    "/",
    SPECIES,
    "_",
    size,
    "_products_tree_and_varimp"))

  rf <- trees[[1]]
  ########################
  
  #Import dataset and filter by species and having time of day.
  complete.dataset.for.sdm.SPECIES<-dplyr::filter(complete.dataset.for.sdm,
                                                  SPEC==SPECIES,
                                                  !is.na(ebird.time))
  
  #show how many checklists from each data source type (ebird, pointcount, or transect)
  complete.dataset.for.sdm.SPECIES %>% 
    group_by(datasource) %>%
    dplyr::summarize(n_distinct(SAMPLING_EVENT_ID))
  
  #make it spatial and set coordinate system appropriately.
  coordinates(complete.dataset.for.sdm.SPECIES)<-c("Longitude", "Latitude")
  #make it spatial
  proj4string(complete.dataset.for.sdm.SPECIES)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  #check it worked
  proj4string(complete.dataset.for.sdm.SPECIES)
  #extract values for analysis
  predictors_stack.SPECIES<-extract(x = predictors_stack,
                                    y = complete.dataset.for.sdm.SPECIES)
  #convert to data frame
  predictors_stack.SPECIES.df <- as.data.frame(predictors_stack.SPECIES)
  
  #add in effort, time of day, and presence/absence.
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
  #ie for coarse data
  latlong.predictors.SPECIES.unsplit <- na.omit(latlong.predictors.SPECIES.unsplit)
  
  #set seed for reproducible split
  set.seed(78)
  train <- createDataPartition(y = latlong.predictors.SPECIES.unsplit$presence,
                               times = 1,
                               p = 0.5,
                               list = FALSE)
  
  #split into training and evaluation (.eval) sets. 
  latlong.predictors.SPECIES <- latlong.predictors.SPECIES.unsplit[train,]
  latlong.predictors.SPECIES.eval <- latlong.predictors.SPECIES.unsplit[-train,]
  latlong.predictors.SPECIES.eval$rownums <- rownames(latlong.predictors.SPECIES.eval)
  #spatial version of evaluation dataset
  latlong.predictors.SPECIES.eval.spatial <- latlong.predictors.SPECIES.eval
  coordinates(latlong.predictors.SPECIES.eval.spatial) <- c("Longitude", "Latitude")
  proj4string(latlong.predictors.SPECIES.eval.spatial)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  
  #free up memory
  rm(predictors_stack.SPECIES)  
  rm(predictors_stack.SPECIES.df) 
  rm(complete.dataset.for.sdm.SPECIES)
  gc()
  
  ################################
  #Repeat 50 x
  ################################
  list_of_state_replicate_evals <- replicate(50, expr = {
    ########################
    #Subsample evaluation dataset by whole-area grid
    #Spatial sampling function
    
    #evaluation grid size
    cell.size <- c(10000, 10000)
    n <- 10 #number of samples to take from each evaluation grid
    # spatial.sampling.evaluation <- function (evaluation.spatial,
    #                                          cell.size,
    #                                          n,
    #                                          typeofeval, #typeofeval is "rmse" or "auc" for performance function.
    #                                          prediction.raster) { #the model full prediction map
    #   
    list.of.object.spdf.and.grid <- sample.grid(latlong.predictors.SPECIES.eval.spatial,
                                                #spatial dataframe
                                                cell.size, #in meters
                                                n) #maximum number of samples per grid cell
    
    #THE spatially subsampled DATASET ACROSS THE WHOLE STATE
    iteration.testing.spatial <- list.of.object.spdf.and.grid[[1]]
    
    
    #replace this with new predictions for each value in the spatial testing dataset (iteration.testing.spatial).
    #load evaluation dataset that has extracted predictors except for time and effort.
    #merge the spatial subset with each predictor value for it.
    
    ########################
    #Then predict from statewide tree
    print(paste(SPECIES,
                size,
                typeofeval,
                "once because statewide"))
    
    eval.df <- iteration.testing.spatial
    eval.df.pred.only <- eval.df[,2:(ncol(eval.df)-1)]
      tree.predict <-  predict( object = rf,
                                newdata = eval.df.pred.only)
      model.predictions <- data.frame("pred" = tree.predict,
                               "Longitude" = eval.df$Longitude,
                               "Latitude" = eval.df$Latitude,
                               "rownums" = as.numeric(as.character(eval.df$rownums))
      )

    ########################
    
    ########################
    #Take model predictions and label with actual/presence absence.
    #If predictions don't cover entire area, need to only check for labels at the rows where it exists.
      model.predictions$presence <- iteration.testing.spatial@data[, c("presence")]

    
    pred <- prediction(predictions = model.predictions$pred,# prediction from tree,
                       labels = model.predictions$presence) #actual presence at that point)
    
    
    perf <- performance(pred,
                        typeofeval)
    print(paste(SPECIES,
                size,
                typeofeval,
                "made it to data frame at end"))
    eval <- perf@y.values[[1]]

    return(eval)
    ################################
    #end 50x repeat
  })
  df <- data.frame("evalue" = list_of_state_replicate_evals,
                   "species" = SPECIES,
                   "scale" = size,
                   "errortype" = typeofeval,
                   stringsAsFactors = FALSE)
  return(df)
}
listofeval.statewide.rmse <- lapply (specieslist,
                                 FUN = predict_from_done_state_trees,
                                 size = "statewide",
                                 typeofeval = "rmse")
statewise.rmse <- rbindlist(listofeval.statewide.rmse)

listofeval.statewide.auc <- lapply (specieslist,
                                FUN = predict_from_done_state_trees,
                                size = "statewide",
                                typeofeval = "auc")
statewise.auc <- rbindlist(listofeval.statewide.auc)

###################################
#Combine all lists into one dataframe per eval type

rmse <- rbind(statewise.rmse,
              small.rmse,
              medium.rmse,
              large.rmse)

auc <- rbind(statewise.auc,
              small.auc,
              medium.auc,
              large.auc)


#Save RDS files

saveRDS(rmse,
        "manuscript_files/rmse_downscale.rds")
saveRDS(auc,
        "manuscript_files/auc_downscale.rds")


saveRDS(rmse,
        "manuscript_files/rmse_current.rds")
saveRDS(auc,
        "manuscript_files/auc_current.rds")

