#Support set generation function plus importing of state for it

library(rgdal)
library(sp)

##################################
#Generate support sets
#start by generating random points within the study area.
#home computer
#state<-readOGR(dsn="E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed",
#               layer="ok_state_vector_smallest_pdf_3158")
#aws file system
state<-readOGR(dsn=getwd(),
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
  saveRDS(file = paste0(SPECIES,
                        "polygons",
                        numberofpoints)
                        )
  
}
