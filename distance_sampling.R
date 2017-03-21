library(Distance)
library(geosphere)
library(rgeos)
library(rgdal)
library(sp)

########################
#Get area of region for both types of analysis
state<-readOGR(dsn="E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed",
               layer="ok_state_vector_smallest_pdf_3158")

state<-spTransform(x = state,
                   CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
)
Region.Area <- gArea(state, byid=TRUE)

########################
#Process data
#Transect analysis
#import data
source(file = "source_transect_data.R")

transect.complete <- transect.complete %>%
  filter(!is.na(sighting.LON),
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
                                       "covar.year" = as.factor(transect.complete$year),
                                       "Species" = as.factor(transect.complete$Possible.Species)
                                       )
#####
  #Point count analysis
  
  source(file = "source_pointcount_data.R")
  #here a section to count up total number of visits per PC, tag back on to data file.
  pointcounts.complete$newspotname.area <- as.factor(pointcounts.complete$StandardizedLocationName.y)
  
  pointcounts.complete <- pointcounts.complete %>%
    filter(!is.na(Longitude),
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
                                          "covar.year" = as.factor(pointcounts.complete$year),
                                          "Species" = as.factor(pointcounts.complete$Species)
  )
  
  
#####
#########################
#Create model summary function (works for both line and point counts).


#########################
#Transect or point count function
processing.function.with.models.built.in <- function (Species,
                                                      transect = c("line", 
                                                                   "point"),
                                                      dataset) {

  dataset <- dataset %>%
    filter_(Species == Species)
 
  uniform <- ds(dataset,
                truncation = "10%",
                transect = transect,
                formula = ~ 1,
                key = "unif",
                adjustment = "cos"
  )
  
  hn.null <- ds(dataset,
                     truncation = "10%",
                     transect = transect,
                     formula = ~ 1,
                     key = "hn",
                     adjustment = NULL
  )

  hn.covar.obs <- ds(dataset,
                          truncation = "10%",
                          transect = transect,
                          formula = ~ covar.obs,
                          key = "hn",
                          adjustment = NULL
  )
  
  hn.covar.obs.time <- ds(dataset,
                               truncation = "10%",
                               transect = transect,
                               formula = ~ covar.obs + covar.time,
                               key = "hn",
                               adjustment = NULL
  )
  
  hn.covar.obs.month <- ds(dataset,
                               truncation = "10%",
                               transect = transect,
                               formula = ~ covar.obs + covar.month,
                               key = "hn",
                               adjustment = NULL
  )
  
  hn.covar.obs.year <- ds(dataset,
                                    truncation = "10%",
                                    transect = transect,
                                    formula = ~ covar.obs + covar.year,
                                    key = "hn",
                                    adjustment = NULL
  )
  
  hn.covar.obs.time.month <- ds(dataset,
                          truncation = "10%",
                          transect = transect,
                          formula = ~ covar.obs + covar.time + covar.month,
                          key = "hn",
                          adjustment = NULL
  )
  
  hn.covar.obs.time.year <- ds(dataset,
                                truncation = "10%",
                                transect = transect,
                                formula = ~ covar.obs + covar.time + covar.year,
                                key = "hn",
                                adjustment = NULL
  )
  
  hn.covar.obs.year.month <- ds(dataset,
                                truncation = "10%",
                                transect = transect,
                                formula = ~ covar.obs + covar.year + covar.month,
                                key = "hn",
                                adjustment = NULL
  )
  
  hn.covar.obs.time.month.year <- ds(dataset,
                                     truncation = "10%",
                                     transect = transect,
                                     formula = ~ covar.obs + covar.time + covar.month + covar.year,
                                     key = "hn",
                                     adjustment = NULL
  )
  
  hr.null <- ds(dataset,
                truncation = "10%",
                transect = transect,
                formula = ~ 1,
                key = "hr",
                adjustment = NULL
  )
  
  hr.covar.obs <- ds(dataset,
                     truncation = "10%",
                     transect = transect,
                     formula = ~ covar.obs,
                     key = "hr",
                     adjustment = NULL
  )
  
  hr.covar.obs.time <- ds(dataset,
                          truncation = "10%",
                          transect = transect,
                          formula = ~ covar.obs + covar.time,
                          key = "hr",
                          adjustment = NULL
  )
  
  hr.covar.obs.month <- ds(dataset,
                           truncation = "10%",
                           transect = transect,
                           formula = ~ covar.obs + covar.month,
                           key = "hr",
                           adjustment = NULL
  )
  
  hr.covar.obs.year <- ds(dataset,
                          truncation = "10%",
                          transect = transect,
                          formula = ~ covar.obs + covar.year,
                          key = "hr",
                          adjustment = NULL
  )
  
  hr.covar.obs.time.month <- ds(dataset,
                                truncation = "10%",
                                transect = transect,
                                formula = ~ covar.obs + covar.time + covar.month,
                                key = "hr",
                                adjustment = NULL
  )
  
  hr.covar.obs.time.year <- ds(dataset,
                               truncation = "10%",
                               transect = transect,
                               formula = ~ covar.obs + covar.time + covar.year,
                               key = "hr",
                               adjustment = NULL
  )
  
  hr.covar.obs.year.month <- ds(dataset,
                                truncation = "10%",
                                transect = transect,
                                formula = ~ covar.obs + covar.year + covar.month,
                                key = "hr",
                                adjustment = NULL
  )
  
  hr.covar.obs.time.month.year <- ds(dataset,
                                     truncation = "10%",
                                     transect = transect,
                                     formula = ~ covar.obs + covar.time + covar.month + covar.year,
                                     key = "hr",
                                     adjustment = NULL
  )
  
  

summary.table <- summarize_ds_models(uniform,
                      hn.null,
                      hn.covar.obs,
                      hn.covar.obs.time,
                      hn.covar.obs.month,
                      hn.covar.obs.year,
                      hn.covar.obs.time.month,
                      hn.covar.obs.time.year,
                      hn.covar.obs.year.month,
                      hn.covar.obs.time.month.year,
                      hr.null,
                      hr.covar.obs,
                      hr.covar.obs.time,
                      hr.covar.obs.month,
                      hr.covar.obs.year,
                      hr.covar.obs.time.month,
                      hr.covar.obs.time.year,
                      hr.covar.obs.year.month,
                      hr.covar.obs.time.month.year,
                      output = "plain")
#print csv of summary table
##
test <- summarize_ds_models(line.hn.covar.days.time.year,
                     output = "plain")

#Select the AICs within the top deltaAIC = 0-5 and get the gof, density and abundance estimates (mean, SE, CV).
names.of.top.models <- test %>%
  filter(`Delta AIC`<5)%>%
  select(Model)

bestmodels.names <- as.character(names.of.top.models$Model)

list.of.models.for.summary <- mget(bestmodels.names)

list.of.estimates <- lapply(list.of.models.for.summary,
                            FUN = summary)

list.of.estimates[[1]]$dht$individuals$N
list.of.estimates[[1]][[2]][[2]][3]

lapply('[',
       c)
#print csv of gof and estimates

}

##################################
#Run the model function for each species and survey type

#Dickcissel
dick.line <- processing.function.with.models.built.in (Species = "DICK",
                                                     transect = "line",
                                                     dataset = transect_for_distance)
dick.pc <- processing.function.with.models.built.in (Species = "DICK",
                                                       transect = "point",
                                                     dataset = pointcounts_for_distance)