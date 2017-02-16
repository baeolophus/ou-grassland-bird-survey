#libraries needed
library(raster)
library(sp)
library(rgdal)
library(dplyr)

rasterOptions()$tmpdir
rasterOptions(tmpdir="E:/Documents/R/temp")


#Bring in predictor data.
#import bioclim layers
bio_utm_list <- list.files(path = paste0(getwd(),
                                         "/bio_12_ok_shape"),
                               pattern = "tif$",
                               full.names = FALSE)
bio_utm_files <- paste0("bio_12_ok_shape/",
                        bio_utm_list)

for(i in bio_utm_files) { assign(unlist(strsplit(i,
                                                     "[./]"))[2], #splits filenames at / and and . to eliminate folder name and file type.
                                     raster(i)) } 


resample_census_utm_30m <- raster("resample_census_utm_30m.tif")



conservation_easements_CalcAcres_raster <- raster("conservation_easements_CalcAcres_raster.tif")
conservation_easements_presenceabsence_raster_okmask <- raster("conservation_easements_presenceabsence_raster_okmask.tif")

nlcdrasters_list <- list.files(paste0(getwd(), "/nlcd_processing"),
                               pattern = "tif$",
                               full.names = FALSE)
nlcdrasters_files <- paste0("nlcd_processing/",
                          nlcdrasters_list)
                   
for(i in nlcdrasters_files) { assign(unlist(strsplit(i,
                                                     "[./]"))[2], #splits filenames at / and and . to eliminate folder name and file type.
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
#has to be spatial for function to work so re-add that
coordinates(latlong.predictors.DICK) <- c("Longitude", "Latitude")
proj4string(latlong.predictors.DICK)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

##################################
#Generate support sets
#start by generating random points within the study area.

# coerce to a SpatialPolygons object
studyarea.extent.poly <- as(studyarea.extent,
                            'SpatialPolygons')  

random.points<-spsample(x=studyarea.extent.poly, #should be able to use the spatial polygon here too.  or studyarea.extent.poly
                        n=1000,
                        type="random")

#give it correct project like sightings.
proj4string(random.points)<-CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))


plot(random.points)
#create squares around them, these will be support set extents.
#How to make squares/rectangles, code/comments adapted from here:
#http://neondataskills.org/working-with-field-data/Field-Data-Polygons-From-Centroids
#they got much from: http://stackoverflow.com/questions/26620373/spatialpolygons-creating-a-set-of-polygons-in-r-from-coordinates

#set the radius for the plots
radius <- 50000 #radius in meters. =50,000 = 50 km = 200 x 200 km boxes

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

#Now, function for subsetting and running the test on each subset.


spatial.support.set<-function(whichrandombox,
                              spatialdataset){
  spatial.support.set<-spatialdataset[polys.df[whichrandombox,],]
  #I think there should be a line that turns it back into regular data?
  sample.size.good<-ifelse(length(spatial.support.set)>10, 1, 0)
  #need to have the minimum data requirement in here too.
  library(rpart)
  vars <- paste(predictors,collapse="+")
  frmla <- paste("presence ~", vars)
  as.formula(frmla)
  
  tree.test<-rpart(frmla,
                   data=spatialdataset,
                   method="anova",
                   control=rpart.control(cp=0.001))
  #create prediction map for illustration purposes
  support.set<-crop(predictors_stack,
                            extent(polys.df[whichrandombox,]))
  tree.test.raster.prediction<-raster::predict(object=support.set, #raster object, probably use bioclim.extent,
                                               model=tree.test)
  plot(tree.test.raster.prediction)
  tree.test.raster.prediction.extended<-extend(x=tree.test.raster.prediction,
                                      y=extent(studyarea.extent.poly),
                                      value=NA)
  return(list(tree.test.raster.prediction.extended,
              sample.size.good,
              tree.test))
  
  #I could even do the mean of several kinds of models as the prediction for each square
  #if I choose to include more than one model type.
}

#Some info on tuning rpart.
#https://stat.ethz.ch/R-manual/R-devel/library/rpart/html/rpart.control.html
#http://stackoverflow.com/questions/20993073/the-result-of-rpart-is-just-with-1-root

#Then apply the function for each support set
list.test<-lapply(1:2,
                  FUN=spatial.support.set,
                  spatialdataset=latlong.predictors.DICK)

#Get the weights out, which is the support set sample size at each pixel
weights<-lapply(list.test,
                "[",
                2)
weights<-as.vector(unlist(weights))

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
