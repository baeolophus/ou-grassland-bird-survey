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

#This file tests whether the support sets in the ensemble are significantly different in pixel coverage.
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