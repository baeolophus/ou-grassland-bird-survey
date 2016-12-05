#libraries needed
library(raster)
library(sp)
library(rgdal)

#Bring in predictor data.

#Bring in whole response data set as a spatial object including presence/absence.
#specify species

#Merge the predictor and response data

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


random.points<-spsample(x=studyarea.extent,
                        n=1000,
                        type="random")

proj4string(random.points)<-CRS(as.character(
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

spatial.support.set<-function(numbersupportsets, dataset, N){
  support.set<-dataset[polys.df[numbersupportsets,],]
  #I think there should be a line that turns it back into regular data?
  sample.size.good<-ifelse(length(support.set)>N, 1, 0)
  #need to have the minimum data requirement in here too.
  library(rpart)
  tree.test<-rpart(response~bio1+
                     bio2+
                     bio3+
                     bio4+
                     bio5+
                     bio6+
                     bio7+
                     bio8+
                     bio9+
                     bio10+
                     bio11+
                     bio12+
                     bio13+
                     bio14+
                     bio15+
                     bio16+
                     bio17+
                     bio18+
                     bio19,
                   data=tree.data.dick,
                   method="class",
                   control=rpart.control(cp=0.0000000000000000000000000001))
  summary(tree.test)
  plot(tree.test)
  #create prediction map for illustration purposes
  tree.test.raster.prediction<-raster::predict(object=studyarea.bioclim, #raster object, probably use bioclim.extent,
                                               model=tree.test)
  plot(tree.test.raster.prediction)
  tree.test.raster.prediction.extended<-extend(tree.test.raster.prediction,
                                      studyarea.extent)
  return(list(tree.test.raster.prediction.extended,
              sample.size.good))
  
  #I could even do the mean of several kinds of models as the prediction for each square
  #if I choose to include more than one model type.
}

list.test<-lapply(1:6,
                  FUN=spatial.support.set,
                  dataset=NAME.OF.DATASET,
                  N=)


names(list.test)[1:2] <- c('x', 'y')
#x and y seems necessary, cannot rename these or mosaic gives error in do.call below.
list.test$fun <- mean
list.test$na.rm <- TRUE

ensemble.mosaic <- do.call(mosaic, list.test)

ensemble.weighted.mosaic<-do.call(weighted.mean,
                                  list.test,
                                  w=,
                                  na.rm=TRUE)

list.test<-list(list(1,1,1), list(1,2,6))
test.function<-function(x){
  thing<-unlist(list.test[[x]][3])
  mean(thing)}
test.function(2)
hmm<-lapply(1:2, test.function)
plot(ensemble.mosaic)

