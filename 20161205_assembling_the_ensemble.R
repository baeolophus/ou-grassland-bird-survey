#libraries needed
library(beepr)
library(dplyr)
library(GSIF)
library(microbenchmark)
library(randomForest)
library(raster)
library(rgdal)
library(ROCR)
library(rpart)
library(sp)

#create temporary raster files on large drive because they occupy 10-30 GB
rasterOptions()$tmpdir
rasterOptions(tmpdir="E:/Documents/R/temp")

#Bring in predictor data.
gispath <- "E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed"
#import bioclim layers
bio_utm_list <- list.files(path = paste0(gispath,
                                         "/bio_12_ok_shape"),
                               pattern = "tif$",
                               full.names = FALSE)
bio_utm_files <- paste0(gispath,
                        "/bio_12_ok_shape/",
                        bio_utm_list)

for(i in bio_utm_files) { assign(unlist(strsplit(i,
                                                     "[./]"))[10], #splits filenames at / and and . to eliminate folder name and file type.
                                     raster(i)) } 


resample_census_utm_30m <- raster(paste0(gispath,
                                         "/resample_census_utm_30m.tif"))



conservation_easements_CalcAcres_raster <- raster(paste0(gispath,
                                                         "/conservation_easements_CalcAcres_raster_okmask.tif"))
conservation_easements_presenceabsence_raster_okmask <- raster(paste0(gispath,
                                                                      "/conservation_easements_presenceabsence_raster_okmask.tif"))

nlcdrasters_list <- list.files(paste0(gispath, "/nlcd_processing"),
                               pattern = "tif$",
                               full.names = FALSE)
nlcdrasters_files <- paste0(gispath,
                            "/nlcd_processing/",
                          nlcdrasters_list)
                   
for(i in nlcdrasters_files) { assign(unlist(strsplit(i,
                                                     "[./]"))[10], #splits filenames at / and and . to eliminate folder name and file type.
                                     raster(i)) } 

predictors <- as.list(ls()[sapply(ls(), function(x) class(get(x))) == 'RasterLayer'])

predictors.list <- as.list(lapply(predictors, get))
#using get lets the middle code take the character names of raster layers and stack everything that is a raster layer
#Using lapply on the list lets it do this to all the rasters.


predictors_stack <- stack (predictors.list)
#Define study area extent based on predictors.
(studyarea.extent <- extent(predictors_stack))

#########################
#Responses
#Bring in whole response data set (with NA lat/long already removed) as a spatial object including presence/absence.
#Is already in utm
complete.dataset.for.sdm <- read.csv(file = "oklahomadatasetforsdm_naomit_utm.csv")
complete.dataset.for.sdm.DICK<-dplyr::filter(complete.dataset.for.sdm,
                                             SPEC=="DICK",
                                             month == 4 | month == 5 | month == 6)
#to match transects and point counts, summer only.

#make it spatial, remembering these values were converted from lat/long to UTM already in data manipulation file.
coordinates(complete.dataset.for.sdm.DICK)<-c("Longitude", "Latitude")
#make it spatial
proj4string(complete.dataset.for.sdm.DICK)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#check it worked
proj4string(complete.dataset.for.sdm.DICK)

#extract values for analysis
predictors_stack.DICK<-extract(x=predictors_stack,
                               y=complete.dataset.for.sdm.DICK)
predictors_stack.DICK.df <- as.data.frame(predictors_stack.DICK)

latlong.predictors.DICK<-cbind("presence" = complete.dataset.for.sdm.DICK$presence,
                               coordinates(complete.dataset.for.sdm.DICK),
                               predictors_stack.DICK.df,
                               row.names = NULL)

latlong.predictors.DICK.spatial <-cbind("presence" = complete.dataset.for.sdm.DICK$presence,
                               coordinates(complete.dataset.for.sdm.DICK),
                               predictors_stack.DICK.df,
                               row.names = NULL)
#has to be spatial for function to work so re-add that
coordinates(latlong.predictors.DICK.spatial) <- c("Longitude", "Latitude")
proj4string(latlong.predictors.DICK.spatial)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
rm(complete.dataset.for.sdm) #free up memory
rm(predictors_stack.DICK)
##################################
##################################
#Generate support sets
#start by generating random points within the study area.
state<-readOGR(dsn="E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed",
                      layer="ok_state_vector_smallest_pdf_3158")

state<-spTransform(x = state,
                           CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
)

set.seed(6798257) #set seed for random points, and later the other random processes.

random.stratified.support.sets <- function (numberofpoints,
                                            radius){ #radius = 1/2 of a square side
random.points<-spsample(x=state, #should be able to use the spatial polygon here too.  or studyarea.extent.poly
                        n=numberofpoints,
                        type="stratified")

#confirm projection
proj4string(random.points)

#plot(random.points) #view

#create squares around them, these will be support set extents.
#How to make squares/rectangles, code/comments adapted from here:
#http://neondataskills.org/working-with-field-data/Field-Data-Polygons-From-Centroids
#they got much from: http://stackoverflow.com/questions/26620373/spatialpolygons-creating-a-set-of-polygons-in-r-from-coordinates
#get the centroids from the random.points spatial points object.

centroids<-data.frame(
  "x"=coordinates(random.points)[,1],
  "y"=coordinates(random.points)[,2]
)
centroids$ID<-paste("ID", rownames(centroids), sep="")

#define the plot boundaries based upon the plot radius. 
#NOTE: this assumes that plots are oriented North and are not rotated. 
#If the plots are rotated, you'd need to do additional math to find 
#the corners.
yPlus <- centroids$y+radius
xPlus <- centroids$x+radius
yMinus <- centroids$y-radius
xMinus <- centroids$x-radius

#Extract the plot ID information. NOTE: because we set
#stringsAsFactor to false above, we can import the plot 
#ID's using the code below. If we didn't do that, our ID's would 
#come in as factors by default. 
#We'd thus have to use the code ID=as.character(centroids$Plot_ID) 
ID<-centroids$ID

#calculate polygon coordinates for each plot centroid. 
square<-cbind(xMinus,yPlus, xPlus,yPlus, xPlus,yMinus, xMinus,yMinus,xMinus,yPlus,xMinus,yPlus)


#create spatial polygons
polys <- SpatialPolygons(
  mapply(function(poly, id) {
    xy <- matrix(poly, ncol=2, byrow=TRUE)
    Polygons(list(Polygon(xy)), ID=id)
  },
  split(square, row(square)), ID),
  proj4string=CRS(as.character(
    "+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")))

# Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

#plot(random.points)
#plot(polys.df,
#     add=TRUE)
return(list(polys, polys.df))
}

#########################
#Now, function for subsetting and running the test on each subset.


spatial.support.set<-function(whichrandombox,
                              spatialdataset,
                              predictor_stack,
                              polys.df,
                              ...){
  #create temporary raster files on large drive because they occupy 10-30 GB
  rasterOptions()$tmpdir
  rasterOptions(tmpdir="E:/Documents/R/temp")
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

  return(list(tree.test.raster.prediction.extended,
              sample.size.good,
              tree.test))
}


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
 # ensemble.weighted.mosaic<-do.call(raster::weighted.mean,
 #                                  list(predictions.support.sets.stacked,
 #                                       weights,
 #                                         TRUE))
  beginCluster()
  ensemble.weighted.mosaic<-clusterR(x = predictions.support.sets.stacked,
                                     fun = raster::weighted.mean,
                                     w = weights,
                                     na.rm = TRUE)
  endCluster()
  #Plot the mosaic.  Remove this line when I transfer to AWS.
  #create file here in .eps or .pdf.
  plot(ensemble.weighted.mosaic)

  return(ensemble.weighted.mosaic)
}
############################
#Run small, medium, and large support set models.
#parameters
ntree <- 50
importance <- FALSE
radius.small <- 60000 #small radius in meters. =6,000 = 60 km = 120 x 120 km boxes #200 points
radius.medium <- 100000 #med radius in meters. =100,000 = 100 km = 200 x 200 km boxes #75 points
radius.large <- 225000 #large radius in meters. =250,000 = 250 km = 500 x 500 km boxes #25 points

polys.small <- random.stratified.support.sets(numberofpoints = 200,
                                              radius.small)
polys.small.p <- unlist(polys.small[[1]])
polys.small.df <- unlist(polys.small[[2]])
support.small.list <- lapply(1,
                             FUN = spatial.support.set,
                             spatialdataset = latlong.predictors.DICK.spatial,
                             predictor_stack = predictors_stack,
                             polys.df = polys.small.df,
                             ntree = ntree,
                             importance = importance)

polys.medium <- random.stratified.support.sets(numberofpoints = 75,
                                               radius.medium)
polys.medium.p <- unlist(polys.medium[[1]])
polys.medium.df <- unlist(polys.medium[[2]])
support.medium.list <- lapply(1:2,
                              FUN = spatial.support.set,
                              spatialdataset = latlong.predictors.DICK.spatial,
                              predictor_stack = predictors_stack,
                              polys.df = polys.medium.df,
                              ntree = ntree,
                              importance = importance)

polys.large <- random.stratified.support.sets(numberofpoints = 25,
                                              radius.large)
polys.large.p <- unlist(polys.large[[1]])
polys.large.df <- unlist(polys.large[[2]])

support.large.list <- lapply(1:2,
                             FUN = spatial.support.set,
                             spatialdataset = latlong.predictors.DICK.spatial,
                             predictor_stack = predictors_stack,
                             polys.df = polys.large.df,
                             ntree = ntree,
                             importance = importance)
support.small.ensemble <- ensemble.function(support.small.list)
support.medium.ensemble <- ensemble.function(support.medium.list)
support.large.ensemble <- ensemble.function(support.large.list)



beep()#small, medium, large

############################
#Statewide model
statewide.data <- latlong.predictors.DICK
statewide.data$Longitude <- NULL
statewide.data$Latitude <- NULL
#These two columns should be taken out because not predicting on them.

tree.statewide <- randomForest(presence ~ ., 
                          data = statewide.data,
                          ntree = ntree,
                          importance = TRUE) 

beginCluster()
microbenchmark(tree.statewide.raster.prediction.prob<-clusterR(predictors_stack,
                                                raster::predict,
                                                args = list(model = tree.statewide,
                                                          #  type = "prob",
                                                            progress = "text")), times = 1) #3.7 hours

endCluster()

#pdf or eps of map here generated here too
plot(tree.statewide.raster.prediction.prob)

varImpPlot(tree.statewide)
print(tree.statewide)
tree.statewide.varimp <- data.frame(importance(tree.statewide))

#Go through top 30 variables in partialPlot
#pdf

partialPlot(tree.statewide, 
            statewide.data, 
            grasslands71_15cell,
            "1") #using 1 as reference ie presence

#http://stats.stackexchange.com/questions/93202/odds-ratio-from-decision-tree-and-random-forest
#http://r.789695.n4.nabble.com/randomForest-PartialPlot-reg-td2551372.html shoudl not do the logit thing actually
#Just go for target class (I want presence ie "1") and interpret higher as more likely.

#http://stackoverflow.com/questions/32606375/rmse-calculation-for-random-forest-in-r

#for spatially uniform test data
#http://stackoverflow.com/questions/32862606/taking-random-point-from-list-of-points-per-grid-square
#Do this sampling n times (200?  250?)


#details on how to do probability maps for classification http://evansmurphy.wixsite.com/evansspatial/random-forest-sdm

#########################################
#Once predictions made, evaluation of model.
#RMSE (root mean square error)

#AUC
#example code: http://stackoverflow.com/questions/30366143/how-to-compute-roc-and-auc-under-roc-after-training-using-caret-in-r
#and https://www.biostars.org/p/87110/
evaluation.spatial <- latlong.predictors.DICK.spatial
coordinates(evaluation.spatial) <- c("Longitude", "Latitude")
prediction.raster<-tree.statewide.raster.prediction.prob
#later,  do in a loop or lapply for bootstrap/sampling of distribution.  for now just make sure this works.

#http://stats.idre.ucla.edu/r/faq/how-can-i-generate-bootstrap-statistics-in-r/
#http://gsif.r-forge.r-project.org/sample.grid.html
#http://www.stat.wisc.edu/~larget/stat302/chap3.pdf #bootstrapping/sampling

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

###############################
#evaluate small, med, large, and statewide, then plot bootstrap distributions and calculation mean and sd for AUC and RMSE
spatial.sampling.evaluation <- latlong.predictors.DICK.spatial #replace with evaluation dataset
cell.size <- c(10000, 10000)
n <- 10

statewide.sampling.rmse <- replicate(50,
          expr = do.call (what = spatial.sampling.evaluation,
                          args = list(evaluation.spatial= latlong.predictors.DICK.spatial,
                                      cell.size,
                                      n,
                                      typeofeval = "rmse",
                                      prediction.raster = tree.statewide.raster.prediction.prob)))

#repeat for these
support.small.ensemble
support.medium.ensemble
support.large.ensemble

boxplot(cbind("Small" = statewide.sampling.rmse,
              "Medium" = statewide.sampling.rmse,
              "Large" = statewide.sampling.rmse,
              "Statewide" = statewide.sampling.rmse),
        xlab = "Support set size",
        ylab = "AUC")


#Then repeat for AUC

statewide.sampling.auc <- replicate(50,
                                    expr = do.call (what = spatial.sampling.evaluation,
                                                    args = list(evaluation.spatial= latlong.predictors.DICK.spatial,
                                                                cell.size,
                                                                n,
                                                                typeofeval = "auc",
                                                                prediction.raster = tree.statewide.raster.prediction.prob)))
boxplot(cbind("Small" = ,
              "Medium" = ,
              "Large" = ,
              "Statewide" = statewide.sampling.rmse),
        xlab = "Support set size",
        ylab = "AUC")
