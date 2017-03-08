#libraries needed
library(beepr)
library(caret)
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
plot(conservation_easements_presenceabsence_raster_okmask)

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

plot(complete.dataset.for.sdm.DICK,
     pch = complete.dataset.for.sdm.DICK$presence)

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

plot(random.points) #view

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

plot(random.points)
plot(polys.df,
     add=TRUE)
return(list(polys, polys.df))
}

radius.small <- 60000 #small radius in meters. =6,000 = 60 km = 120 x 120 km boxes #200 points
radius.medium <- 100000 #med radius in meters. =100,000 = 100 km = 200 x 200 km boxes #75 points
radius.large <- 225000 #large radius in meters. =250,000 = 250 km = 500 x 500 km boxes #20 points

polys.small <- random.stratified.support.sets(numberofpoints = 200,
                                              radius.small)
polys.small.p <- unlist(polys.small[[1]])
polys.small.df <- unlist(polys.small[[2]])

polys.medium <- random.stratified.support.sets(numberofpoints = 75,
                                               radius.medium)
polys.medium.p <- unlist(polys.medium[[1]])
polys.medium.df <- unlist(polys.medium[[2]])

polys.large <- random.stratified.support.sets(numberofpoints = 25,
                                              radius.large)
polys.large.p <- unlist(polys.large[[1]])
polys.large.df <- unlist(polys.large[[2]])

#http://r-sig-geo.2731867.n2.nabble.com/Efficient-way-to-obtain-gridded-count-of-overlapping-polygons-td6034590.html
#Checking that the three scales have similar numbers of overlaps

beginCluster()
microbenchmark(countoverlapping.small <- clusterR(nlcd_ok_utm14, #raster
                                                   fun = rasterize,
                                                   args = list(x = polys.small.p,
                                                               fun = 'count',
                                                               update = TRUE,
                                                               updateValue = '!NA')),
  times = 1)
plot(countoverlapping.small)
plot(state, add = TRUE)

microbenchmark(countoverlapping.medium <- clusterR(nlcd_ok_utm14, #raster
                                                  fun = rasterize,
                                                  args = list(x = polys.medium.p,
                                                              fun = 'count',
                                                              update = TRUE,
                                                              updateValue = '!NA')),
               times = 1)
plot(countoverlapping.medium)
plot(state, add = TRUE)

microbenchmark(countoverlapping.large <- clusterR(nlcd_ok_utm14, #raster
                                                  fun = rasterize,
                                                  args = list(x = polys.large.p,
                                                              fun = 'count',
                                                              update = TRUE,
                                                              updateValue = '!NA')),
               times = 1)
plot(countoverlapping.large)
plot(state)

sampling.overlaps <- spsample(state,
                              type = "regular",
                              n = 50)
plot(sampling.overlaps, add= TRUE)

overlaps.small.sample <- as.data.frame(extract(x = countoverlapping.small,
                                 y = sampling.overlaps))
overlaps.small.sample$set <- "small"
colnames(overlaps.small.sample) <- c("overlapsperpixel", "set")
overlaps.medium.sample <- as.data.frame(extract(x = countoverlapping.medium,
                                 y = sampling.overlaps))
overlaps.medium.sample$set <- "medium"
colnames(overlaps.medium.sample) <- c("overlapsperpixel", "set")

overlaps.large.sample <- as.data.frame(extract(x = countoverlapping.large,
                                 y = sampling.overlaps))
overlaps.large.sample$set <- "large"
colnames(overlaps.large.sample) <- c("overlapsperpixel", "set")
min(overlaps.large.sample$overlapsperpixel)

overlaps <- bind_rows(overlaps.small.sample,
                      overlaps.medium.sample,
                      overlaps.large.sample)

colnames(overlaps) <- c("overlapsperpixel", "set")
overlaps$set <- as.factor(overlaps$set)
endCluster()

overlaps.lm <- lm (overlapsperpixel ~ set,
                   data = overlaps)
anova(overlaps.lm)
plot(overlapsperpixel ~ set,
     data = overlaps)
library(multcomp)
anova.tukey<-glht(overlaps.lm,
                  linfct=mcp(set="Tukey"))
summary(anova.tukey)
#########################
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
  
  support.set <- crop(predictor_stack,
                            extent(polys.df[whichrandombox,]))
  beginCluster() #use raster's multicore clustering to automatically use more cores and speed up predict and extend
  tree.test.raster.prediction <- clusterR(support.set,
                                          fun = raster::predict,
                                          args = list(model = tree.test))
  plot(tree.test.raster.prediction)
  tree.test.raster.prediction.extended <- clusterR(tree.test.raster.prediction,
                                                   fun = extend,
                                                   args = list(y = studyarea.extent,
                                                               value = NA))
  endCluster()
  return(list(tree.test.raster.prediction.extended,
              sample.size.good,
              tree.test))
}


###########################
#test with single dataframes
spatial.support.set<-latlong.predictors.DICK.spatial[polys.medium.df[1,],]
#I think there should be a line that turns it back into regular data?
sample.size.good<-ifelse(length(spatial.support.set)>25, 1, 0)
#need to have the minimum data requirement in here too.
support.set.data <- as.data.frame(spatial.support.set)
support.set.data$Longitude <- NULL
support.set.data$Latitude <- NULL


tree.test <- randomForest(presence ~ ., 
                          data = support.set.data,
                          ntree = 50) #This allows all other random forest arguments to be set at the spatial.support.set function level.
#ca 1 sec run

support.set<-crop(predictors_stack,
                  extent(polys.medium.df[1,]))

beginCluster()
microbenchmark(preds_rf<- clusterR(support.set, raster::predict, args = list(model = tree.test)), times = 1)

microbenchmark(tree.test.raster.prediction <- raster::predict(object = support.set, #raster object, probably use bioclim.extent,
                                             model = tree.test), times = 1) #28 min
plot(tree.test.raster.prediction)
microbenchmark(tree.test.raster.prediction.extended <- clusterR(tree.test.raster.prediction,
                                                                fun = extend,
                                                                args = list(y=studyarea.extent,
                                                                            value=NA)),
               times = 1)
endCluster()
beep()



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
  ensemble.weighted.mosaic<-do.call(raster::weighted.mean,
                                    list(predictions.support.sets.stacked,
                                         weights,
                                         TRUE))
  
  #Plot the mosaic.  Remove this line when I transfer to AWS.
  plot(ensemble.weighted.mosaic)
  
  #create file here in .eps or .pdf.
  
  return(ensemble.weighted.mosaic)
}
############################
#Run small, medium, and large support set models.
#parameters
ntree <- 50
importance <- FALSE
support.small.list <- lapply(1:2,
                             FUN = spatial.support.set,
                             spatialdataset = latlong.predictors.DICK.spatial,
                             predictor_stack = predictors_stack,
                             polys.df = polys.small,
                             ntree = ntree,
                             importance = importance)

support.medium.list <- lapply(1:2,
                              FUN = spatial.support.set,
                              spatialdataset = latlong.predictors.DICK.spatial,
                              predictor_stack = predictors_stack,
                              polys.df = polys.medium,
                              ntree = ntree,
                              importance = importance)
support.large.list <- lapply(1:2,
                             FUN = spatial.support.set,
                             spatialdataset = latlong.predictors.DICK.spatial,
                             predictor_stack = predictors_stack,
                             polys.df = polys.large,
                             ntree = ntree,
                             importance = importance)
support.small.ensemble <- ensemble.function(support.small.list)
support.medium.ensemble <- ensemble.function(support.medium.list)
support.large.ensemble <- ensemble.function(support.large.list)



beepr()#small, medium, large

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
tree.statewide.raster.prediction.prob<-clusterR(predictors_stack,
                                                raster::predict,
                                                args = list(model = tree.statewide,
                                                            type = "prob",
                                                            progress = "text"))

endCluster()
plot(tree.statewide.raster.prediction)
#pdf or eps of map here generated here too

summary(tree.statewide)
varImp(tree.statewide)
varImpPlot(tree.statewide)
print(tree.statewide)
importance(tree.statewide)
importance(tree.statewide, type=1)

#Go through top 20? variables in partialPlot
partialPlot(tree.statewide, 
            training, 
            bio12_12_OK)
partialPlot(tree.statewide, 
            training, 
            undevopenspace_15cell,
            "1") #using 1 as reference ie presence

#http://stats.stackexchange.com/questions/93202/odds-ratio-from-decision-tree-and-random-forest
#http://r.789695.n4.nabble.com/randomForest-PartialPlot-reg-td2551372.html shoudl not do the logit thing actually
#Just go for target class (I want presence ie "1") and interpret higher as more likely.

#http://stackoverflow.com/questions/32606375/rmse-calculation-for-random-forest-in-r

#for spatially uniform test data
#http://stackoverflow.com/questions/32862606/taking-random-point-from-list-of-points-per-grid-square
#Do this sampling n times (200?  250?)


#details on how to do probability maps for classification http://evansmurphy.wixsite.com/evansspatial/random-forest-sdm

plot(tree.test.raster.prediction.iris.rf.prob)
beepr::beep() #notification on completion


#########################################
#Once predictions made, evaluation of model.
#RMSE (root mean square error)

#AUC
#example code: http://stackoverflow.com/questions/30366143/how-to-compute-roc-and-auc-under-roc-after-training-using-caret-in-r
#and https://www.biostars.org/p/87110/
evaluation.spatial <- testing
coordinates(evaluation.spatial) <- c("Longitude", "Latitude")
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

model.predictions.presence <- as.data.frame(model.predictions$presence)

pred <- prediction(predictions = iteration.testing.spatial$presence,#model.predictions.presence,
                   labels = testing.nolatlong$presence) #evaluation.dataset$presence)

perf <- performance(pred,
                    typeofeval)
return(perf)
}

spatial.sampling.evaluation(evaluation.spatial,
                            cell.size = 1000,
                            n = 10,
                            typeofeval = "rmse")

perf_AUC <- performance(pred,
                     "auc") #Calculate the AUC value

AUC=perf_AUC@y.values[[1]]
###############################
#evaluate small, med, large, and statewide, then plot bootstrap distributions and calculation mean and sd for AUC and RMSE

statewide.sampling.rmse <- replicate(50,
          spatial.sampling.evaluation,
          cell.size = 1000,
          n = 10,
          typeofeval = "rmse",
          prediction.raster = tree.statewide.raster.prediction.prob)

#repeat for these
support.small.ensemble
support.medium.ensemble
support.large.ensemble

#Then repeat for AUC

###############################
#plot the actual ROC curve... fink et al. don't include this, do I want it?

perf_ROC <- performance(pred,
                     "tpr",
                     "fpr") 
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

#############################
#############################
#############################
#ensemble mosaic non-function version in case needed later
#Get the weights out, which is the support set sample size at each pixel
weights<-lapply(list.test,
                "[",
                2)
weights<-as.vector(unlist(weights))
#The weights are 1 (have enough samples) and 0 (do not have enough samples, do not use)
#For use in weighted.mean below.  It is a vector of weights per layer.  Since values are 1 if enough sample
#then all are weighted equally if they are used.  If 0 (not enough sample size), then 0 weighted, not used.

#remove one level of list on the remaining support sets
predictions.support.sets<-unlist(lapply(list.test,
                                        "[",
                                        1))

#Stack all the support sets (this will work because they have been extended with NA in non-support-set regions)
predictions.support.sets.stacked<-raster::stack(predictions.support.sets)


#Run the ensemble by using weighted mean.  The weight is by number of support sets.
ensemble.weighted.mosaic<-do.call(raster::weighted.mean,
                                  list(predictions.support.sets.stacked,
                                       weights,
                                       TRUE))

#Plot the mosaic.
plot(ensemble.weighted.mosaic)
