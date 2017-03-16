library(dplyr) # data manipulation
library(geosphere) #distances between transects
library(lubridate) #dates for year

#transect polishing and finishing up columns used in species distribution models and in distance sampling.
#######################
#Polishing up data formats and combining the different sources.
#then merge with sightings and do all data checks as for point counts.
transect.data<-read.csv("transect_data.csv")
transect.metadata<-read.csv("transect_metadata.csv")
#merge the files so every row has all metadata attached.
transect.complete<-left_join(transect.data,
                             transect.metadata,
                             by=c("Date",
                                  "Observer",
                                  "Location",
                                  "Transect"))

#complete matches number of rows in data, so there are no weird duplicates.  Good.

#How to generate gps points for each bird sighting in transects.
#use chron::times() to get times, calculate location as proportion of start to finish time between gps points of start and finish.
#For each transect need start, end, and sighting time in this minutes format.
transect.complete$starttime<-paste(transect.complete$Start.Time..24h.,
                                   ":00",
                                   sep="")
transect.complete$endtime<-paste(transect.complete$End.Time..24h.,
                                 ":00",
                                 sep="")
transect.complete$time<-paste(transect.complete$Time..24hr.,
                              ":00",
                              sep="")
transect.complete$starttime<-chron::times(transect.complete$starttime)
transect.complete$endtime<-chron::times(transect.complete$endtime)
transect.complete$time<-chron::times(transect.complete$time)

rownames(transect.complete[(is.na(transect.complete$time)),])
#This checks to see if any have NA.
#These 73 remaining simply have no time on the original data sheets.

#Then column of proportion for each time along it, divided by end time.  
transect.complete$lengthoftransect.time<-as.numeric(abs(transect.complete$starttime-transect.complete$endtime)*24*60)
#absolute value of differences, gives hours minutes seconds difference
#as.numeric(num*24*60) #to convert to hours then minutes
transect.complete$time.to.this.bird<-as.numeric(abs(transect.complete$starttime-transect.complete$time)*60*24)
transect.complete$proportion.time.along.transect<-transect.complete$time.to.this.bird/transect.complete$lengthoftransect.time

x<-matrix(c(transect.complete$Start.LON, transect.complete$Start.LAT), 
          ncol=2)
#find distance between start and end.
transect.complete$transect.distance<-distHaversine(p1=matrix(c(transect.complete$Start.LON, transect.complete$Start.LAT), 
                                                             ncol=2),
                                                   p2=matrix(c(transect.complete$End.LON, transect.complete$End.LAT), 
                                                             ncol=2))
#get the initial bearing to extend from the start (west) to end (east).
transect.complete$transect.bearing<-bearing(p1=matrix(c(transect.complete$Start.LON, transect.complete$Start.LAT), 
                                                      ncol=2),
                                            p2=matrix(c(transect.complete$End.LON, transect.complete$End.LAT), 
                                                      ncol=2))


#Then multiply proportion column by distance.
transect.complete$transect.distance.sighting<-transect.complete$transect.distance*transect.complete$proportion.time.along.transect
#Then use function that places point along a line a given distance.
transect.sighting.lonlat<-destPoint(p=matrix(c(transect.complete$Start.LON, transect.complete$Start.LAT),ncol=2),
                                    b=transect.complete$transect.bearing,
                                    d=transect.complete$transect.distance.sighting)

#it should give gps points for it.
transect.complete$sighting.LON<-transect.sighting.lonlat[,1]
transect.complete$sighting.LAT<-transect.sighting.lonlat[,2]

#Now clean up dates to match ebird.
transect.complete$dates.format<-as.character(transect.complete$Date)
transect.complete$dates.format<-as.Date(transect.complete$dates.format,
                                        format="%m/%d/%Y")
transect.complete$year<-as.numeric(format(as.Date(transect.complete$dates.format),
                                          format="%Y"))
transect.complete$month<-as.numeric(format(as.Date(transect.complete$dates.format),
                                           format="%m"))
transect.complete$day<-as.numeric(format(as.Date(transect.complete$dates.format),
                                         format="%d"))
transect.complete$ebird.day<-yday(transect.complete$dates.format)
transect.complete$TIME<-as.character(transect.complete$Start.Time..24h.)
timebits.tr<-strsplit(transect.complete$TIME,
                      ":")
transect.complete$hours<-as.numeric(sapply(timebits.tr, "[", 1))
transect.complete$minutes<-as.numeric(sapply(timebits.tr, "[", 2))
transect.complete$ebird.time<-transect.complete$hours+transect.complete$minutes/60

#correcting quantity to numeric
transect.complete$Quantity.corrected<-gsub("+", "",
                                           as.character(transect.complete$Quantity),
                                           fixed=TRUE)
transect.complete$Quantity.corrected<-as.numeric(as.character(transect.complete$Quantity.corrected))

#View(transect.complete[is.na(transect.complete$Quantity.corrected),c("Quantity.corrected", "Quantity")])
#checks that all NAs are reasonable (no value given in original Quantity)


transect.complete <- transect.complete %>%
  filter(whattodowiththisrecord == "KEEP",
         whattodowiththissighting == "KEEP")

