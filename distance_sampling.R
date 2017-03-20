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
                             !is.na(Distance..m.))


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


transect.function <- function(species) {
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
                                     "covar.month" = as.factor(transect.complete$month),
                                     "covar.time" = transect.complete$ebird.time,
                                     "covar.year" = as.factor(transect.complete$year)
                                     )
                                     

                      
line.hn.null <- ds(transect_for_distance,
              truncation = "10%",
              transect = "line",
              formula = ~ 1,
              key = "hn",
              adjustment = NULL
              )

line.hn.covar.obs <- ds(transect_for_distance,
                  truncation = "10%",
                  transect = "line",
                  formula = ~ covar.obs,
                  key = "hn",
                  adjustment = NULL
                  )

line.hn.covar.obs.days <- ds(transect_for_distance,
                  truncation = "10%",
                  transect = "line",
                  formula = ~ covar.obs + covar.days.into.surveys,
                  key = "hn",
                  adjustment = NULL
                  )

line.hn.covar.obs.time <- ds(transect_for_distance,
                  truncation = "10%",
                  transect = "line",
                  formula = ~ covar.obs + covar.time,
                  key = "hn",
                  adjustment = NULL
                  )

line.hn.covar.obs.time.year <- ds(transect_for_distance,
                             truncation = "10%",
                             transect = "line",
                             formula = ~ covar.obs + covar.time + covar.year,
                             key = "hn",
                             adjustment = NULL
)

line.hn.covar.days.time.year <- ds(transect_for_distance,
                                  truncation = "10%",
                                  transect = "line",
                                  formula = ~ covar.days.into.surveys + covar.time + covar.year,
                                  key = "hn",
                                  adjustment = NULL
)

line.hn.covar.days.time.year <- ds(transect_for_distance,
                                   truncation = "10%",
                                   transect = "line",
                                   formula = ~ covar.obs + covar.month + covar.time + covar.year,
                                   key = "hr",
                                   adjustment = NULL
)

gof <- ds.gof(line.hn.covar.days.time.year)

gof[[2]]$ks$Dn

plot(line.hn.covar.obs.time)
summary(line.hn.covar.obs.time)
ds.gof(line.hn.covar.obs.time)

#model summarizing from help file
## model summaries
model.sel.bin <- data.frame(name = c("HN Null", 
                                   "HN obs",
                                   "HN obs + time",
                                   "HN obs + month",
                                   "HN obs + year",
                                   "HN obs + time + month",
                                   "HN obs + time + year",
                                   "HN obs + month + year",
                                   "HN Date/Time Only (month + time + year)"),
                            aic = c(line.hn.null$ddf$criterion,
                                  line.hn.covar.obs$ddf$criterion,
                                  line.hn.covar.obs.days$ddf$criterion,
                                  line.hn.covar.obs.time$ddf$criterion,
                                  line.hn.covar.obs.time.year$ddf$criterion,
                                  line.hn.covar.days.time.year$ddf$criterion),
                            gof.sig.is.bad = c(ds.gof(line.hn.covar.days.time.year)[[3]]))

# Note model with stratum as covariate is most parsimonious
print(model.sel.bin)

summarize_ds_models(line.hn.covar.days.time.year,
                    output = "plain")

}

#Point count analysis

source(file = "source_pointcount_data.R")
#here a section to count up total number of visits per PC, tag back on to data file.
pointcounts.complete$newspotname.area <- as.factor(pointcounts.complete$StandardizedLocationName.y)

pointcounts.complete <- pointcounts.complete %>%
  filter(Species == "DICK",
         !is.na(Longitude),
         !is.na(Latitude),
         !is.na(Angle..deg.),
         !is.na(Distance..m.))
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
