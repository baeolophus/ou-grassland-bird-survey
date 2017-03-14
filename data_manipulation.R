#libraries used in this file.
#library(tools)
#library(sp) # R's base package for vector data types
#library(raster) # better printing of spatial objects, export shapefiles
#library(rgdal) # for reading different spatial file formats
#library(rgeos) # for spatial distance and topology operations
library(dplyr) # data manipulation
library(geosphere) #distances between transects
library(lubridate) #dates for year
library(fuzzyjoin) #for getting ebird data out by spatial location
#####

source(file = "source_transect_data.R")

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
#These remaining simply have no time on the original data sheets.

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

View(transect.complete[is.na(transect.complete$Quantity.corrected),c("Quantity.corrected", "Quantity")])
#checks that all NAs are reasonable (no value given in original Quantity)

#add in month, date, year columns for PCs.  then export as new current version.
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

#checking that everything has a match
rownames(pointcounts.complete[(is.na(pointcounts.complete$Latitude)),])

#Now clean up dates to match ebird.
pointcounts.complete$dates.format<-as.character(pointcounts.complete$Date)
pointcounts.complete$dates.format<-as.Date(pointcounts.complete$dates.format,
                                           format="%m/%d/%Y")
pointcounts.complete$year<-as.numeric(format(as.Date(pointcounts.complete$dates.format),
                                  format="%Y"))
pointcounts.complete$month<-as.numeric(format(as.Date(pointcounts.complete$dates.format),
                                   format="%m"))
pointcounts.complete$day<-as.numeric(format(as.Date(pointcounts.complete$dates.format),
                                 format="%d"))

pointcounts.complete$ebird.day<-yday(pointcounts.complete$dates.format)
pointcounts.complete$TIME<-as.character(pointcounts.complete$Start.Time..24h.)

timebits.pc<-strsplit(pointcounts.complete$TIME,
                                       ":")
pointcounts.complete$hours<-as.numeric(sapply(timebits.pc, "[", 1))
pointcounts.complete$minutes<-as.numeric(sapply(timebits.pc, "[", 2))
pointcounts.complete$ebird.time<-pointcounts.complete$hours+pointcounts.complete$minutes/60
####################################
###
#Formatting ebird to fit into same format as transects and PCs.
#get list of species codes from PC
#get list of species codes from transect
(tr.species<-levels(transect.complete$Possible.Species))
(pc.species<-levels(pointcounts.complete$Species))
distinct(data.frame(tr.species))
distinct(data.frame(pc.species))

all.species<-c(tr.species, pc.species)
names.i.have<-dplyr::distinct(data.frame(all.species)) #view to see what corrections need made in each file
colnames(names.i.have)<-"SPEC"
#combine list for no duplicates
#get scientific names for all
aou.codes<-foreign::read.dbf(file="AOUcodes2016.dbf")
#this file has codes, scientific names (presumably will match ebird), and common names.
does.it.have.match<-dplyr::left_join(names.i.have, aou.codes)

#select only those columns from ebird
pull.these.columns.from.ebird<-gsub(" ", "_", as.character(does.it.have.match$SCINAME))

#ebird import
ebird2013<-read.csv(file="bigfiles\\studyarea_ebird2013.csv")
ebird2014<-read.csv(file="bigfiles\\studyarea_ebird2014.csv")

selected.ebird.sp.2013<-dplyr::select(ebird2013, #original dataframe
                             1:19, #metadata columns from original dataframe
                             one_of(pull.these.columns.from.ebird)) #select species in our surveys


gathered.ebird.data.2013<-selected.ebird.sp.2013%>%
  mutate_each(funs(as.character), one_of(pull.these.columns.from.ebird))%>%
  tidyr::gather(key=species, value=Quantity, one_of(pull.these.columns.from.ebird))

selected.ebird.sp.2014<-dplyr::select(ebird2014, #original dataframe
                                      1:19, #metadata columns from original dataframe
                                      one_of(pull.these.columns.from.ebird)) #select species in our surveys

#Then change format to one row per species.
#gives an error about losing attributes but this is because the count columns are factors.
#http://stackoverflow.com/questions/28972386/retain-attributes-when-using-gather-from-tidyr-attributes-are-not-identical
#The values seem okay so I'm leaving it for now.

#It already gives days where absences occured.  Will need to filter so they are complete counts that give absences.
 
#do for 2014 too
gathered.ebird.data.2014<-selected.ebird.sp.2014%>%
  mutate_each(funs(as.character), one_of(pull.these.columns.from.ebird))%>%
  tidyr::gather(key=species, value=Quantity, one_of(pull.these.columns.from.ebird))

#Then combine into one big ebird file
gathered.ebird.data.all<-rbind(gathered.ebird.data.2013,
                               gathered.ebird.data.2014)

##Eliminate ebird duplicates because some survey data was uploaded to ebird.
#First eliminate non-primary checklists (where people submitted more than one checklist for one birding event)
ebird.cleaned<-gathered.ebird.data.all%>%
  filter(PRIMARY_CHECKLIST_FLAG==1,
         COUNT_TYPE!="P20") #eliminate casual counts

#Pull out transects and point counts from ebird 
#with the checklist code types.
#from ebird documentation: "What kind of observation the sample is:
#stationary (P21), traveling (P22, P34), 
#area (P23, P35), 
#casual (P20), or random (P48).
#Protocol P34 is a small amount of data contributed from the Rocky
#Mountain Bird Observatory that we believe is high quality.
#Protocol P35 data are back-yard area counts made on consecutive days
#(see http://www.birds.cornell.edu/MyYardCounts).

ebird.cleaned.test<-ebird.cleaned%>%
  filter(STATE_PROVINCE=="Oklahoma"
         )

ebird.sampling.ids<-dplyr::select(ebird.cleaned.test, YEAR, DAY, TIME, SAMPLING_EVENT_ID, LONGITUDE, LATITUDE)%>%
  dplyr::distinct(SAMPLING_EVENT_ID, .keep_all=TRUE)

transect.primary.keys<-dplyr::select(transect.complete, 
                                     Date, Observer, Location, spot=Transect,
                                     ebird.day,
                                     ebird.time,
                                     year,
                                     Longitude=Start.LON, Latitude=Start.LAT)%>%
  dplyr::distinct(Date, Observer, Location, spot, .keep_all=TRUE)
pointcount.primary.keys<-dplyr::select(pointcounts.complete, 
                                     Date, Observer, Location, spot=Point,
                                     ebird.day,
                                     ebird.time,
                                     year,
                                     Longitude, Latitude)%>%
  dplyr::distinct(Date, Observer, Location, spot, .keep_all=TRUE)

primary.keys<-rbind(transect.primary.keys,
                    pointcount.primary.keys)
#Use spatial buffer for point counts and transects
#Slightly bigger than that for matching points above, or same??
sampling.ids.that.we.input<-fuzzyjoin::geo_left_join(x=primary.keys,
                                        y=ebird.sampling.ids,
                               max_dist=15,
                               unit="km",
                               by=c("Longitude"="LONGITUDE", 
                                    "Latitude"="LATITUDE"))%>%
  filter(.,
       DAY==ebird.day,
       YEAR==year,
       TIME>=(ebird.time-1)&TIME<=(ebird.time+1))%>%
  #Then get out which ebird checklist codes, these are the ones that will be eliminated.
  distinct(SAMPLING_EVENT_ID)

omit.these<-as.character(sampling.ids.that.we.input[,1])

complete.list.of.jeremy.samples<-c("S18101887", #Grady County WMA point counts on 4/17/2014. PCs
                               "S18137660", #Tallgrass Prairie Preserve on 4/23/2014. transect.
                               "S18137507", #Rita Blanca point counts on 4/25/2014. PCs
                               "S18434697", #Cimmaron Bluffs WMA on 5/16/2014 transects
                               "S18434249") #Cimmaron Hills WMA on 5/17/2014 transects
#These are all from Jeremy.
#These are all included in the filtered "omit.these" so it works for known ebird samples.
#It is unclear how many other people entered surveys or "presurvey" birds,
#so we will eliminate all 25-35 sampling events in the "omit.these" list.

ebird.complete<-dplyr::filter(ebird.cleaned,
                     !(SAMPLING_EVENT_ID %in% omit.these))


###############
##MAKING COMBINED SINGLE DATA SHEET for presence/absence data
###############
#If there are any more changes to data,
#then edit original ones and rerun this script.
###############
#Specify data sources for impending combination.
ebird.complete$datasource<-"EBIRD"
transect.complete$datasource<-"TRANSECT"
pointcounts.complete$datasource<-"POINTCOUNT"
pointcounts.complete<-left_join(pointcounts.complete,
                                         aou.codes,
                                         by=c("Species"="SPEC"))
pointcounts.complete$SCINAME<-gsub(" ", "_", as.character(pointcounts.complete$SCINAME))
transect.complete<-left_join(transect.complete,
                                aou.codes,
                                by=c("Possible.Species"="SPEC"))
transect.complete$SCINAME<-gsub(" ", "_", as.character(transect.complete$SCINAME))


ebird.complete$SCINAME.spaces<-gsub("_", " ", as.character(ebird.complete$species))
ebird.complete<-left_join(ebird.complete,
                             aou.codes,
                             by=c("SCINAME.spaces"="SCINAME"))

ebird.complete$SCINAME<-gsub(" ", "_", as.character(ebird.complete$SCINAME.spaces))


#Create primary key single columns for our data with same name as ebird primary key (SAMPLING_EVENT_ID)
transect.complete$SAMPLING_EVENT_ID<-paste(transect.complete$datasource,
                                           transect.complete$Date,
                                           transect.complete$Observer,
                                           transect.complete$Location,
                                           transect.complete$Transect,
                                           sep="_")
pointcounts.complete$SAMPLING_EVENT_ID<-paste(pointcounts.complete$datasource,
                                              pointcounts.complete$Date,
                                              pointcounts.complete$Observer,
                                              pointcounts.complete$Location,
                                              pointcounts.complete$Point,
                                           sep="_")


pc.species.per.transect<-dplyr::group_by(pointcounts.complete,
                                         SAMPLING_EVENT_ID,
                                         Species)%>%
  do(.,
     sample_n(., 1))%>%ungroup(.)
pc.combine<-dplyr::filter(pc.species.per.transect,
                          whattodowiththisrecord=="KEEP",
                          whattodowiththissighting=="KEEP",
                          Distance..m.<500)%>%
                          select(.,
                   datasource,
                   SAMPLING_EVENT_ID,
                   year,
                   month,
                   ebird.day,
                   ebird.time,
                   Observer,
                   SCINAME,
                   Longitude,
                   Latitude,
                   Quantity
                   )

#This gets one species sighting per transect so we have one row per species per transect while still retaining detailed location data.
#There may be a better way (first sighting?  last sighting?) but this will do for now.
tr.species.per.transect<-dplyr::group_by(transect.complete,
                                         SAMPLING_EVENT_ID,
                                         Possible.Species)%>%
  do(.,
     sample_n(., 1))%>%ungroup(.)

tr.combine<-dplyr::filter(tr.species.per.transect,
                          whattodowiththisrecord=="KEEP",
                          whattodowiththissighting=="KEEP",
                          Distance..m.<500)%>%
                          select(.,
                   datasource,
                   SAMPLING_EVENT_ID,
                   year,
                   month,
                   ebird.day,
                   ebird.time,
                   Observer,
                   SCINAME,
                   Longitude=sighting.LON,
                   Latitude=sighting.LAT,
                   Quantity=Quantity.corrected)
oursurveys.combined<-bind_rows(tr.combine,
                                      pc.combine)

oursurveys.combined<-ungroup(oursurveys.combined)

oursurveys.generate.presence<-dplyr::filter(oursurveys.combined,
                                             !is.na(SCINAME)) %>%
                               tidyr::spread(key=SCINAME,
                                             value=Quantity, 
                                             fill=0) %>% #whereever there is an empty row, create 0
                               tidyr::gather(key=SCINAME,
                                             value=Quantity,
                                             one_of(pull.these.columns.from.ebird)) %>% #gather all the species columns
                               dplyr::arrange(SAMPLING_EVENT_ID)
oursurveys.generate.presence$Quantity<-as.character(oursurveys.generate.presence$Quantity)
#necessary to convert to character to combine with ebird

str(oursurveys.generate.presence)

combine.ebird<-select(ebird.complete,
                      datasource,
                      SAMPLING_EVENT_ID,
                      year=YEAR,
                      month=MONTH,
                      ebird.day=DAY,
                      ebird.time=TIME,
                      Observer=OBSERVER_ID,
                      SCINAME,
                      Longitude=LONGITUDE,
                      Latitude=LATITUDE,
                      Quantity) %>%
                dplyr::arrange(SAMPLING_EVENT_ID)

#all have same columns in same order though with different names.
str(combine.ebird)
str(oursurveys.generate.presence)
complete.dataset.for.sdm<-rbind(oursurveys.generate.presence,
                                    combine.ebird)

complete.dataset.for.sdm$quantity.numeric<-gsub("X",
                                                9999999999,
                                                complete.dataset.for.sdm$Quantity,
                                                fixed=TRUE)
complete.dataset.for.sdm$quantity.numeric<-as.numeric(complete.dataset.for.sdm$quantity.numeric)
complete.dataset.for.sdm$presence<-ifelse(complete.dataset.for.sdm$quantity.numeric<1, 0, 1)
complete.dataset.for.sdm$SCINAME.space<-gsub("_",
                                                " ",
                                                complete.dataset.for.sdm$SCINAME,
                                                fixed=TRUE)


complete.dataset.for.sdm<-left_join(complete.dataset.for.sdm,
                                         aou.codes[,c("SPEC",
                                                      "COMMONNAME",
                                                      "SCINAME")],
                                         by=c("SCINAME.space"="SCINAME"))
complete.dataset.for.sdm<-dplyr::arrange(complete.dataset.for.sdm,
                                         datasource,
                                         SAMPLING_EVENT_ID)

#now select species to get smaller datasets that are easily used.

write.csv(complete.dataset.for.sdm,
          "completedatasetforsdm.csv")

#Masking for just oklahoma

#Import oklahoma polygon mask.
ok_vector <- readOGR(dsn=getwd(),
                     layer="ok_state_vector_smallest_pdf")

ok_vector <- spTransform(ok_vector,
                         CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
plot(ok_vector)

##make it spatial, remembering these values are lat/long in decimal degrees
#eliminate missing values first.
complete.dataset.for.sdm.na <- complete.dataset.for.sdm %>%
  dplyr::filter(!is.na(Longitude) & !is.na(Latitude))
coordinates(complete.dataset.for.sdm.na)<-c("Longitude", "Latitude")
#make it spatial
proj4string(complete.dataset.for.sdm.na)<-CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#use this: proj4string(data) <- CRS("+proj=longlat + ellps=WGS84")
#Then convert to utm
complete.dataset.for.sdm.na.utm <- spTransform(complete.dataset.for.sdm.na,
                                CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

proj4string(complete.dataset.for.sdm.na.utm)

#Subset responses.
oklahoma.dataset.for.sdm.na.utm<-complete.dataset.for.sdm.na.utm[ok_vector,]

plot(oklahoma.dataset.for.sdm.na.utm)

#Write to file
write.csv(as.data.frame(oklahoma.dataset.for.sdm.na.utm),
          "oklahomadatasetforsdm_naomit_utm.csv")

