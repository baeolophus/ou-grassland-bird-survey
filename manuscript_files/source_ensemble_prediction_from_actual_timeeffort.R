library(dplyr)
library(caret)
#load trees
SPECIES <- "BHCO"
size <- "large"

trees <- readRDS(file = paste0(#filepath here
  "/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Downscale_current/",
  "/",
  SPECIES,
  "/",
  SPECIES,
  "_",
  size,
  "_intermediates_support_list"))
polys <- readRDS(file = paste0(#filepath here
  "/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Downscale_current/",
  "/",
  SPECIES,
  "/",
  SPECIES,
  "_",
  size,
  "_intermediatefile_polys"))
polys.df <- polys[[2]]

########################
#load spatial dataframe that contains all predictors extracted from predictors stack, 
#plus the actual presence/absence data

#Bring in predictor data.
source("manuscript_files/source_ensemble_predictor_import.R")

#Responses
#Bring in whole response data set (with NA lat/long already removed) as a spatial object including presence/absence.
#Is already in utm
#bring in evaluation dataset and make it spatial object
complete.dataset.for.sdm <- read.csv(file = "manuscript_files/oklahomadatasetforsdm_naomit_utm.csv")

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
replicate(50, expr = {
########################
#Subsample evaluation dataset by whole-area grid
#Spatial sampling function
library(GSIF)
library(ROCR)

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
library(randomForest)
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
library(data.table)
model.predictions <- rbindlist(tree.prediction) %>% dplyr::filter(use == 1)%>%
  group_by(rownums)%>%
  arrange(rownums) %>%
  summarize(mean_pixel_pred = mean(pred))

########################
#Take final model predictions and label with actual/presence absence.

pred <- prediction(predictions = model.predictions$mean_pixel_pred,#model.predictions.presence,
                   labels = iteration.testing.spatial$presence) #evaluation.dataset$presence)

typeofeval = "auc"
perf <- performance(pred,
                    typeofeval)
perf@y.values[[1]]
################################
#end 50x repeat
})
