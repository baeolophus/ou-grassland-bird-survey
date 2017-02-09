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


popdensity_census_raster<-raster("r_censusblock_raster.tiff")

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
predictors_stack <- stack (lapply(predictors, get))
#using get lets the middle code take the character names of raster layers and stack everything that is a raster layer
#Using lapply on the list lets it do this to all the rasters.

extent(predictors_stack)

#crop to extent of study area
studyarea.extent<-extent(-103,-94,
                         33,38) # define the extent


#Bring in whole response data set as a spatial object including presence/absence.

#bring in data and metadata, join for species locations.
pointcount.data<-read.csv(file="pointcount_data.csv")
#bring in the manually corrected file
pointcount.metadata.manually.corrected<-read.csv(file="pointcount_metadata.csv")
#merge the files so every row has all metadata attached.
pointcounts.complete<-left_join(pointcount.data,
                                pointcount.metadata.manually.corrected,
                                by=c("Date",
                                     "Observer",
                                     "Location",
                                     "Point"))

#double-check always that row counts for pointcount.data and pointcounts.complete are the same!
#if they are not, go back to data manipulation code and run checks to look for duplicates.


pointcounts.complete.na<-filter(pointcounts.complete,
                                !is.na(Latitude)&
                                  !is.na(Longitude))
#SET SPECIES HERE
pointcounts.complete.na.dick<-filter(pointcounts.complete.na,
                                     Species=="DICK")


#extract values for analysis
bioclim.dick<-as.data.frame(extract(x=studyarea.bioclim,
                                    y=c(pointcounts.complete.na.dick$Longitude,
                                        pointcounts.complete.na.dick$Latitude)))


subs.bioclim<-cbind(pointcounts.complete.na.dick[,c("Longitude",
                                                    "Latitude")],
                    na.omit(bioclim.dick))
subs.bioclim$response<-1



nosightings.dick<-filter(pointcounts.complete.na,
                         Species!="DICK")

(absences<-group_by(nosightings.dick,
                    Date,
                    Observer,
                    Location,
                    Point)%>%
  
  summarize(Sightings=n()))

(presences<-group_by(pointcounts.complete.na.dick,
                     Date,
                     Observer,
                     Location,
                     Point)%>%
  
  summarize(Sightings=n()))

pcs.sans.bird<-anti_join(x=absences,
                         y=presences,
                         by=c("Date",
                              "Observer",
                              "Location",
                              "Point"))

#join to gps for bg/absence points.
bg.draft<-left_join(pcs.sans.bird,
                    pointcount.metadata.manually.corrected,
                    by=c("Date",
                         "Observer",
                         "Location",
                         "Point"))
bg<-bg.draft[,c("Longitude",
                "Latitude")]
coordinates(bg)<-c("Longitude",
                   "Latitude")
bg.bioclim.dick<-as.data.frame(extract(studyarea.bioclim,
                                       bg))
bgpoints<-as.data.frame(bg@coords)
colnames(bgpoints)<-c("Longitude","Latitude")

bg.bioclim.dick<-cbind(as.data.frame(bgpoints),
                       bg.bioclim.dick)

bg.bioclim.dick$response<-0

tree.data.dick<-rbind(bg.bioclim.dick,
                      subs.bioclim)

coordinates(tree.data.dick)<-c("Longitude", "Latitude")
#make it spatial
proj4string(tree.data.dick)<-CRS(as.character(
  "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#generate random points

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

random.points<-spsample(x=studyarea.extent, #should be able to use the spatial polygon here too.
                        n=1000,
                        type="random")

proj4string(random.points)<-CRS(as.character(
  "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
proj4string(tree.data.dick)<-CRS(as.character(
  "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(random.points)
#create squares around them, these will be support set extents.
#How to make squares/rectangles, code/comments adapted from here:
#http://neondataskills.org/working-with-field-data/Field-Data-Polygons-From-Centroids
#they got much from: http://stackoverflow.com/questions/26620373/spatialpolygons-creating-a-set-of-polygons-in-r-from-coordinates

#set the radius for the plots
radius <- 1 #radius in decimal degrees
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
                  spatialdataset=tree.data.dick)


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
