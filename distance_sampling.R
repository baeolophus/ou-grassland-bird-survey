library(Distance)
library(geosphere)
library(rgeos)
library(rgdal)
library(sp)

data(book.tee.data)
tee.data<-book.tee.data$book.tee.dataframe[book.tee.data$book.tee.dataframe$observer==1,]
ds.model <- ds(tee.data,4)
summary(ds.model)
plot(ds.model)

ds.minke<-ds(minke, truncation="10%")
plot(ds.minke)
summary(ds.minke)

ds.gof(ds.minke)

#Transect analysis
#import data
source(file = "source_transect_data.R")

transect.complete <- transect.complete %>%
                      filter(Possible.Species == "DICK",
                             !is.na(sighting.LON),
                             !is.na(sighting.LAT),
                             !is.na(Angle..deg.),
                             !is.na(Distance..m.),
                             whattodowiththisrecord == "KEEP",
                             whattodowiththissighting == "KEEP")

#Calculate perpendicular point where bird is
bird.points <- data.frame(destPointRhumb(p = matrix(c(transect.complete$sighting.LON, transect.complete$sighting.LAT), ncol = 2),
                                                           b = transect.complete$Angle..deg.,
                                                           d = transect.complete$Distance..m.))

perpendicular.distance <- dist2gc(p1 = matrix(c(transect.complete$Start.LON, transect.complete$Start.LAT), ncol = 2),
                                  p2 = matrix(c(transect.complete$End.LON, transect.complete$End.LAT), ncol = 2),
                                  p3 = bird.points)
#Get area of region
state<-readOGR(dsn="E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed",
               layer="ok_state_vector_smallest_pdf_3158")

state<-spTransform(x = state,
                   CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
)
Region.Area <- gArea(state, byid=TRUE)

#format transects with distance, Sample.Label (transectID), Effort (line length), Region Label, Area (area of the region).
transect_for_distance <- data.frame ("distance" = perpendicular.distance,
                                     "Sample.Label" = paste(substring(transect.complete$START.newspotnames, 1, 6), transect.complete$Date),
                                     "Effort" = transect.complete$transect.distance,
                                     "Region.Label" = "Oklahoma",
                                     "Area" = Region.Area,
                                     "size" = transect.complete$Quantity.corrected,
                                     "covar.obs" = transect.complete$Observer,
                                     "covar.days.into.surveys" = transect.complete$ebird.day-min(transect.complete$ebird.day),
                                     "covar.time" = transect.complete$ebird.time,
                                     "covar.month" = as.factor(transect.complete$month),
                                     "covar.year" = as.factor(transect.complete$year)
                                     )
                                     

testing <- transect_for_distance %>%
  group_by(Sample.Label, Effort) %>%
  summarize()
                                    
ds.dick <- ds(transect_for_distance,
              truncation = "10%",
              transect = "line",
              formula = ~ covar.obs + covar.time + covar.days.into.surveys + covar.year
              )
plot(ds.dick)
summary(ds.dick)
ds.gof(ds.dick)

#Point count analysis
#here a section to count up total number of visits per PC, tag back on to data file.

#Then function as above.