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
                                         "/bio_12_utm"),
                               pattern = "tif$",
                               full.names = FALSE)
bio_utm_files <- paste0("bio_12_utm/",
                        bio_utm_list)

for(i in bio_utm_files) { assign(unlist(strsplit(i,
                                                     "[./]"))[2], #splits filenames at / and and . to eliminate folder name and file type.
                                     raster(i)) } 


popdensity_census_raster <- raster("resample_census_utm_30m.tif")



easement_acres <- raster("conservation_easements_CalcAcres_raster.tif")
easement_yesno <- raster("conservation_easements_presenceabsence_raster.tif")

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


extent(grasslands71_15cell)
extent(easement_acres)
extent(easement_yesno)
extent(nlcd_ok_utm14)
extent(popdensity_census_raster)


extent(bio10_12)


studyarea.extent <- c(139321.6,
                      921001.6,
                      3704685,
                      4126455)

extended.predictors <- lapply(predictors.list,
                              FUN = extend,
                              y = studyarea.extent,
                              value=NA)



predictors.list[[2]]
extended.predictors[[2]]
predictors.list[[3]]
extended.predictors[[3]]

lapply(extended.predictors,
       FUN = extent)

predictors_stack <- stack (extended.predictors)

extent(extended.predictors)
#Using lapply on the list lets it do this to all the rasters.

#Then write the rasters by layer

#Then import the final thingies.

#Define study area extent based on predictors.
studyarea.extent <- extent(predictors_stack)

#########################
#Responses
#Bring in whole response data set as a spatial object including presence/absence.
complete.dataset.for.sdm <- read.csv(file = "completedatasetforsdm.csv")
complete.dataset.for.sdm.DICK<-dplyr::filter(complete.dataset.for.sdm,
                                             SPEC=="DICK")

#Get rid of NA for lat/long values.
complete.dataset.for.sdm.DICK<-na.omit(complete.dataset.for.sdm.DICK)

#extract values for analysis
predictors_stack.DICK<-as.data.frame(extract(x=predictors_stack,
                                    y=c(complete.dataset.for.sdm.DICK$Longitude,
                                        complete.dataset.for.sdm.DICK$Latitude)))


subs.bioclim<-cbind(complete.dataset.for.sdm[,c("Longitude",
                                                    "Latitude")],
                    na.omit(predictors_stack.DICK))

tree.data.DICK<-rbind(bg.bioclim.DICK,
                      subs.bioclim)

coordinates(tree.data.DICK)<-c("Longitude", "Latitude")
#make it spatial
proj4string(tree.data.DICK)<-CRS(as.character(
  "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#use this: proj4string(data) <- CRS("+proj=longlat + ellps=WGS84")
#Then convert to utm
alldata.DICK <- spTransform(tree.data.DICK,
                            CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

##################################
#Generate support sets
#start by generating random points within the study area.

# coerce to a SpatialPolygons object
studyarea.extent.poly <- as(studyarea.extent,
                            'SpatialPolygons')  

#if above works, delete#########
studyarea.extent<-Polygon(
  matrix(c(-103, 33,
           -103, 38,
           -94, 38,
           -94, 33,
           -103, 33),
         nrow=5, ncol=2,
         byrow=TRUE)
)

#I think I can combine this with polygon() above and use in all places.
studyarea.extent.polygons<-SpatialPolygons(list(Polygons(list(studyarea.extent), "studyarea")))
####################end delete


random.points<-spsample(x=studyarea.extent, #should be able to use the spatial polygon here too.  or studyarea.extent.poly
                        n=1000,
                        type="random")

#Fix up projections here.
proj4string(random.points)<-CRS(as.character(
  "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
proj4string(tree.data.DICK)<-CRS(as.character(
  "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(random.points)
#create squares around them, these will be support set extents.
#How to make squares/rectangles, code/comments adapted from here:
#http://neondataskills.org/working-with-field-data/Field-Data-Polygons-From-Centroids
#they got much from: http://stackoverflow.com/questions/26620373/spatialpolygons-creating-a-set-of-polygons-in-r-from-coordinates

#set the radius for the plots
radius <- 1 #radius in decimal degrees.  Units should be different with everything in utm now.
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
    "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

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
  tree.test<-rpart(response~bio1+
                     bio2,
                   data=spatialdataset,
                   method="class",
                   control=rpart.control(cp=0.0000000000000000000000000001))
  #create prediction map for illustration purposes
  support.set.bioclim<-crop(studyarea.bioclim,
                            extent(polys.df[whichrandombox,]))
  tree.test.raster.prediction<-raster::predict(object=support.set.bioclim, #raster object, probably use bioclim.extent,
                                               model=tree.test)
  plot(tree.test.raster.prediction)
  tree.test.raster.prediction.extended<-extend(x=tree.test.raster.prediction,
                                      y=extent(studyarea.extent.polygons),
                                      value=NA)
  return(list(tree.test.raster.prediction.extended,
              sample.size.good))
  
  #I could even do the mean of several kinds of models as the prediction for each square
  #if I choose to include more than one model type.
}

list.test<-lapply(1:1000,
                  FUN=spatial.support.set,
                  spatialdataset=tree.data.DICK)


weights<-lapply(list.test,
                "[",
                2)
weights<-as.vector(unlist(weights))

predictions.support.sets<-unlist(lapply(list.test,
                                 "[",
                                 1))

predictions.support.sets.stacked<-raster::stack(predictions.support.sets)

#names(predictions.support.sets)[1:2] <- c('x', 'y')
#x and y seems necessary, cannot rename these or mosaic gives error in do.call below.
#predictions.support.sets$fun <- mean
#predictions.support.sets$na.rm <- TRUE

#ensemble.mosaic <- do.call(mosaic, predictions.support.sets) #This works

#predictions.support.sets2<-predictions.support.sets
#predictions.support.sets2$fun<-raster::weighted

ensemble.weighted.mosaic<-do.call(raster::weighted.mean,
                                  list(predictions.support.sets.stacked,
                                       weights,
                                       TRUE))

plot(ensemble.weighted.mosaic)
