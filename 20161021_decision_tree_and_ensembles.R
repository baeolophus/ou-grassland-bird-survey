#testing creation of ensemble model designed after Fink et al. 2010's STEM models.

#import data
library(dplyr)
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

pointcounts.complete.na.dick<-filter(pointcounts.complete.na,
                                       Species=="DICK")


#sampling https://rdrr.io/cran/dismo/man/gridSample.html
library(dismo)
res <- 0.05 #resolution of cell
r <- raster(extent(range(pointcounts.complete.na.dick[,"Longitude"]),
                   range(pointcounts.complete.na.dick[,"Latitude"])) + res) 
#create raster grid for sampling with given resolution
res(r) <- res #apply resolution

subs <- gridSample(pointcounts.complete.na.dick[,c("Longitude","Latitude")], 
                   r, #raster grid
                   n=1) #number of samples per cell
#bring in bioclim data
#import bioclim layers
library(raster)
bio1<-raster('bio_5m_bil/bio1.bil')
bio2<-raster('bio_5m_bil/bio2.bil')
bio3<-raster('bio_5m_bil/bio3.bil')
bio4<-raster('bio_5m_bil/bio4.bil')
bio5<-raster('bio_5m_bil/bio5.bil')
bio6<-raster('bio_5m_bil/bio6.bil')
bio7<-raster('bio_5m_bil/bio7.bil')
bio8<-raster('bio_5m_bil/bio8.bil')
bio9<-raster('bio_5m_bil/bio9.bil')
bio10<-raster('bio_5m_bil/bio10.bil')
bio11<-raster('bio_5m_bil/bio11.bil')
bio12<-raster('bio_5m_bil/bio12.bil')
bio13<-raster('bio_5m_bil/bio13.bil')
bio14<-raster('bio_5m_bil/bio14.bil')
bio15<-raster('bio_5m_bil/bio15.bil')
bio16<-raster('bio_5m_bil/bio16.bil')
bio17<-raster('bio_5m_bil/bio17.bil')
bio18<-raster('bio_5m_bil/bio18.bil')
bio19<-raster('bio_5m_bil/bio19.bil')
bioclimlayer<-brick(bio1,
                    bio2,
                    bio3,
                    bio4,
                    bio5,
                    bio6,
                    bio7,
                    bio8,
                    bio9,
                    bio10,
                    bio11,
                    bio12,
                    bio13,
                    bio14,
                    bio15,
                    bio16,
                    bio17,
                    bio18,
                    bio19)


#crop to extent of study area
studyarea.extent<-extent(-103,-94,
                         33,38) # define the extent
studyarea.bioclim<-crop(bioclimlayer,
                        studyarea.extent)


#extract values for analysis
bioclim.dick<-as.data.frame(extract(x=studyarea.bioclim,
                      y=c(subs$Longitude,
                        subs$Latitude)))


subs.bioclim<-cbind(subs,
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

#Create decision tree that works using rpart.
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

tree.test2<-lm(response~bio1+
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
                                data=tree.data.dick)

summary(tree.test2)

#create prediction map for illustration purposes
tree.test.raster.prediction<-raster::predict(object=studyarea.bioclim, #raster object, probably use bioclim.extent,
                  model=tree.test)
plot(tree.test.raster.prediction)
points(subs)

tree.test2.raster.prediction<-raster::predict(object=studyarea.bioclim, #raster object, probably use bioclim.extent,
                                             model=tree.test2)
plot(tree.test2.raster.prediction)
points(subs)

#create a simple ensemble model.
#principles based on this: http://www.overkillanalytics.net/more-is-always-better-the-power-of-simple-ensembles/
#but used raster stacks instead because that is not simple x/y like the example.

stack.test.trees<-stack(tree.test.raster.prediction,
                        tree.test2.raster.prediction)

#Do the randomized geographic sampling to get overlapping support sets.
#possibly use raster::mosaic() function.

ensemble.test.trees.mosaic.version<-mosaic(tree.test.raster.prediction,
       tree.test2.raster.prediction,
       fun=mean)

plot(ensemble.test.trees.mosaic.version)

#Overlapping support sets works to create mixture model.

#Testing two overlapping layers and weights.
#Creating an artificially shortened set of predictions to overlap.
test.extent1<-extent(-101,-96,
                         33,38)
test.extent2<-extent(-103,-98,
                     33,38)

test.extent.layer1<-crop(tree.test.raster.prediction,
                         y=test.extent1)

test.extent.layer1.extended<-extend(test.extent.layer1,
                                    studyarea.extent)
test.extent.layer2<-crop(tree.test2.raster.prediction,
                         y=test.extent2)

test.extent.layer2.extended<-extend(test.extent.layer2,
                                    studyarea.extent)

ensembleoverlaptest <- list(test.extent.layer1.extended,
          test.extent.layer2.extended)
names(ensembleoverlaptest)[1:2] <- c('x', 'y')
#x and y seems necessary, cannot rename these or mosaic gives error in do.call below.
ensembleoverlaptest$fun <- mean
ensembleoverlaptest$na.rm <- TRUE

ensemble.mosaic <- do.call(mosaic, ensembleoverlaptest)
plot(ensemble.mosaic)

#generate random points

polygon.extent<-Polygon(
  matrix(c(-103, 33,
                                  -103, 38,
                                  -94, 38,
                                  -94, 33,
                                  -103, 33),
                                  nrow=5, ncol=2,
                                  byrow=TRUE)
  )


random.points<-spsample(x=polygon.extent,
         n=1000,
         type="random")

plot(random.points)
#create squares around them, these will be support set extents.
#How to make squares/rectangles, code/comments adapted from here:
#http://neondataskills.org/working-with-field-data/Field-Data-Polygons-From-Centroids
#they got much from: http://stackoverflow.com/questions/26620373/spatialpolygons-creating-a-set-of-polygons-in-r-from-coordinates

library(sp)
library(rgdal)

#set the radius for the plots
radius <- 20 #radius in meters
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
polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
},
split(square, row(square)), ID),
proj4string=CRS(as.character(
  "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")))

# Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

plot(polys.df, col=rainbow(50, alpha=0.5),
     add=TRUE)


#it makes things, but they don't look spaced appropriately or the 20 m thing is off scale?

#crop the main object (a spatial object with given sighting points) to each extent
crop(object,
     extent)

#Do this into list format for all.

#Run the models on each item in the list

#Then assemble per above.
