library(Distance)
library(geosphere)
library(rgeos)
library(rgdal)
library(sp)

#Get area of region for both types of analysis
state<-readOGR(dsn="E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed",
               layer="ok_state_vector_smallest_pdf_3158")

state<-spTransform(x = state,
                   CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
)
Region.Area <- gArea(state, byid=TRUE)

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


#transect labels by the already gps-checked newspotnames.
transect.complete$newspotname <- as.factor(substring(transect.complete$START.newspotnames, 1, 6))

#Calculate perpendicular point where bird is
bird.points <- data.frame(destPointRhumb(p = matrix(c(transect.complete$sighting.LON, transect.complete$sighting.LAT), ncol = 2),
                                                           b = transect.complete$Angle..deg.,
                                                           d = transect.complete$Distance..m.))

perpendicular.distance <- dist2gc(p1 = matrix(c(transect.complete$Start.LON, transect.complete$Start.LAT), ncol = 2),
                                  p2 = matrix(c(transect.complete$End.LON, transect.complete$End.LAT), ncol = 2),
                                  p3 = bird.points)


#Calculating effort for each transect
transect.effort <- transect.complete %>%
  group_by(newspotname) %>%
  summarize("Effort" = sum(transect.distance))
#sum the effort for repeat visits to line. pg 294 in Buckland et al. 2001 (Introduction to Distance Sampling)

#Then join it back onto transect.complete
transect.complete<- left_join(transect.complete,
                              transect.effort)

#format transects with distance, Sample.Label (transectID), Effort (line length), Region Label, Area (area of the region).
transect_for_distance <- data.frame ("distance" = perpendicular.distance,
                                     "Sample.Label" = transect.complete$newspotname,
                                     "start" = transect.complete$START.newspotnames,
                                     "Effort" = transect.complete$Effort,
                                     "Region.Label" = "Oklahoma",
                                     "Area" = Region.Area,
                                     "size" = transect.complete$Quantity.corrected,
                                     "covar.obs" = transect.complete$Observer,
                                     "covar.days.into.surveys" = transect.complete$ebird.day-min(transect.complete$ebird.day),
                                     "covar.time" = transect.complete$ebird.time,
                                     "covar.month" = as.factor(transect.complete$month),
                                     "covar.year" = as.factor(transect.complete$year)
                                     )
                                     

                      
ds.dick <- ds(transect_for_distance,
              truncation = "10%",
              transect = "line",
              formula = ~ covar.obs
              )
plot(ds.dick)
summary(ds.dick)
ds.gof(ds.dick)

#model summarizing from help file
## model summaries
model.sel.bin <- data.frame(name=c("Pooled f(0)", "Stratum covariate",
                                   "Full stratification"),
                            aic=c(pooled.binned$ddf$criterion,
                                  strat.covar.binned$ddf$criterion,
                                  full.strat.binned.North$ddf$criterion+
                                    full.strat.binned.South$ddf$criterion))

# Note model with stratum as covariate is most parsimonious
print(model.sel.bin)

#Point count analysis

source(file = "source_pointcount_data.R")
#here a section to count up total number of visits per PC, tag back on to data file.
pointcounts.complete$newspotname.area <- as.factor(pointcounts.complete$StandardizedLocationName.y)

pointcounts.complete <- pointcounts.complete %>%
  filter(Species == "DICK",
         !is.na(Longitude),
         !is.na(Latitude),
         !is.na(Angle..deg.),
         !is.na(Distance..m.),
         whattodowiththisrecord == "KEEP",
         whattodowiththissighting == "KEEP")
#Calculating effort for each transect
pointcounts.effort <- pointcounts.complete %>%
  group_by(newspotnames) %>%
  distinct(Date, Observer)%>%
  summarize("Effort" = n())

pointcounts.complete <- left_join(pointcounts.complete,
                                  pointcounts.effort)

#format transects with distance, Sample.Label (transectID), Effort (number of visits), Region Label, Area (area of the region).
pointcounts_for_distance <- data.frame ("distance" = as.numeric(pointcounts.complete$Distance..m.),
                                     "Sample.Label" = pointcounts.complete$newspotnames,
                                     "Effort" = as.numeric(pointcounts.complete$Effort),
                                     "Region.Label" = "Oklahoma",
                                     "Area" = Region.Area,
                                     "size" = as.numeric(pointcounts.complete$Quantity),
                                     "covar.obs" = pointcounts.complete$Observer,
                                     "covar.days.into.surveys" = pointcounts.complete$ebird.day-min(pointcounts.complete$ebird.day),
                                     "covar.time" = pointcounts.complete$ebird.time,
                                     "covar.month" = as.factor(pointcounts.complete$month),
                                     "covar.year" = as.factor(pointcounts.complete$year)
)

pc.dick <- ds(pointcounts_for_distance,
              truncation = "10%",
              transect = "point",
              formula = ~ covar.obs
)
plot(pc.dick)
summary(pc.dick)
ds.gof(pc.dick)
