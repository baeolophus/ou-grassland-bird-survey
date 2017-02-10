#libraries used in this file.
library(tools)
library(sp) # R's base package for vector data types
library(raster) # better printing of spatial objects, export shapefiles
library(rgdal) # for reading different spatial file formats
library(rgeos) # for spatial distance and topology operations
library(dplyr) # data manipulation
library(geosphere) #distances between transects
library(lubridate) #dates for year
library(fuzzyjoin)
#####

#import gpx files of where transects and point counts should be

##this section of code/comments for importing gpx given by a friend of colleague's (do not know name)
# Data Import
# gpx files can store a combination of waypoints, routes, and tracks.
# shapefiles, and R's native spatial formats, can only store one type of 
# geometry at a time, e.g. points OR lines OR polygons.
# I don't know a priori which type of data each gpx file has, so I try both 
# waypoints and tracks for all (no routes exist in this dataset), then combine
# into 2 files, one for all waypoints and one for all tracks.

# list all .gpx files in data/ subdirectory
gpx <- list.files(".", pattern = "\\.gpx", full.names = TRUE)
waypoints <- list()
tracks <- list()
for (f in gpx) {
  # label for different types of features, taken from filename without extension
  nm <- file_path_sans_ext(basename(f))
  
  # process waypoints
  wp <- suppressWarnings(
    tryCatch(
      readOGR(dsn=f, layer="waypoints", stringsAsFactors = FALSE),
      error = function(x) NULL
    )
  )
  if (!is.null(wp)) {
    wp$type <- nm
    wp$id <- wp$name
    # only keep useful fields
    wp <- subset(wp, select = c(type, id, time, ele, cmt))
    # change feature IDs to be unique
    row.names(wp) <- paste0(nm, 1:length(wp)) 
    waypoints[nm] <- wp
  }
  
  # process tracks
  tr <- suppressWarnings(
    tryCatch(
      readOGR(dsn=f, layer="tracks", stringsAsFactors = FALSE),
      error = function(x) NULL
    )
  )
  if (!is.null(tr)) {
    tr$type <- nm
    tr$id <- tr$name
    # only keep useful fields
    tr <- subset(tr, select = c(type, id, cmt))
    # change feature IDs to be unique
    row.names(tr) <- paste0(nm, 1:length(tr)) 
    tracks[nm] <- tr
  }
  rm(wp, tr)
}
waypoints <- do.call(rbind, waypoints)
row.names(waypoints) <- as.character(1:length(waypoints))
unique(waypoints$type)

tracks <- do.call(rbind, tracks)
row.names(tracks) <- as.character(1:length(tracks))
unique(tracks$type)

# seperate out point counts, transect start/ends, and any transect tracks
point_counts <- subset(waypoints, !type %in% c("Transect Start_End"))
transect_startend <-  subset(waypoints, type %in% c("Transect Start_End"))
##end borrowed

#import data from csv files.
######
#point counts
#metadata
pointcount.metadata<-read.csv(file="pointcount_metadata.csv")
pointcount.metadata$Longitude<-ifelse(sign(pointcount.metadata$Longitude)>0, 
                                      pointcount.metadata$Longitude*-1, 
                                      pointcount.metadata$Longitude*1)
pointcount.metadata$index<-rownames(pointcount.metadata)

#plotting gpx original points along with actual data points.
#
plot(y=pointcount.metadata$Latitude,
     x=pointcount.metadata$Longitude)
plot(point_counts, add=TRUE)

#First create buffer around each original point (windmill) to be new polygon.
buf <- gBuffer(point_counts, 
               width = 0.003,
               byid=TRUE,
               quadsegs = 8)
plot(buf, col="red", add=TRUE)
# plotting the map with those points and buffers on it for doublechecking


#make point counts into spatial data
pointcount.metadata.clean<-dplyr::filter(pointcount.metadata,
                                         !is.na(Latitude),
                                         !is.na(Longitude))
coordinates(pointcount.metadata.clean) <- c("Longitude", "Latitude")
proj4string(pointcount.metadata.clean)<-proj4string(point_counts)
#use over() to take actual points (ptsrand) and join with vegmap/buffers
pointcount.join <- sp::over(pointcount.metadata.clean, buf)

#add column of new names back into original clean file.
pointcount.metadata.clean$newspotnames<-pointcount.join$id

#then use index (rownames) to join with original file.
pointcount.metadata.newnames<-merge(x=pointcount.metadata,
                                        y=as.data.frame(pointcount.metadata.clean),
                                    all.x=TRUE,
                                    all.y=FALSE,
                                        by=c("Date",
                                             "Observer",
                                             "Location",
                                             "Point",
                                             "StandardizedLocationName",
                                             "LocalSurveyName",
                                             "Start.Time..24h.",
                                             "Latitude",
                                             "Longitude",
                                             "Cloud",
                                             "Wind",
                                             "Notes",
                                             "Species.before",
                                             "Species.After",
                                             "index"))
                                                    
#Then the actual points should have the nearby names on them.

#Export file, and correct any dubious ones manually in spreadsheet.
write.csv(pointcount.metadata.newnames,
          file="20161003_pointcount_metadata_newnames.csv")

#Export dubious points to .gpx file.
#requires "name" field, use "index" as this.
pointcount.metadata.clean@data$name <- pointcount.metadata.clean@data$index  
#get dubious points only.
fix.pointcounts<-pointcount.metadata.clean[is.na(pointcount.metadata.clean$newspotnames),]

writeOGR(fix.pointcounts["name"], driver="GPX", layer="waypoints", 
         dsn="2016104_unnamed_points.gpx")
#use this to compare original basecamp point list to where point counts conducted

##sightings
pointcount.data<-read.csv(file="pointcount_data.csv")
#bring in the manually corrected file
pointcount.metadata.manually.corrected<-read.csv(file="20161003_pointcount_metadata_newnames_manual_corrections.csv")
#merge the files so every row has all metadata attached.
pointcounts.complete<-left_join(pointcount.data,
                                pointcount.metadata.manually.corrected,
                                    by=c("Date",
                                         "Observer",
                                         "Location",
                                         "Point"))
#Compare pointcounnts.complete row count vs pointcount.data.  
#Pointcount.data is the standard, number should be same.
#Need to check which site/date/obs combos are identical but should not be.

count.combos<-distinct(pointcount.metadata.manually.corrected,
                       Date,
                       Observer,
                       Location,
                       Point,
                       newspotnames) #number of unique.
#Should match number of rows in pointcount.metadata.manually.corrected
#Use which.have.duplicates to see which have duplicate primary key columns.
which.have.duplicates<-group_by(pointcount.metadata.manually.corrected,
                                Date,
                                Observer,
                                Location,
                                Point,
                                newspotnames)%>%
  summarize(duplicates=n())
which.have.duplicates[which.have.duplicates$duplicates>1,]
#after corrections for bad matches in primary key columns and duplicates, now only 6 rows apart.

old.count.combos<-distinct(pointcount.metadata.manually.corrected,
         Date,
         Observer,
         Location,
         Point)
#This one is one short.  So there is one lumped one left??
#Check without the new unique sitenames (newspotnames).
which.have.duplicates.old<-group_by(pointcount.metadata.manually.corrected,
                                Date,
                                Observer,
                                Location,
                                Point)%>%
  summarize(duplicates=n())
which.have.duplicates.old[which.have.duplicates.old$duplicates>1,]
#One sighting (05/08/2014 Roy WXRD point 7 is duplicates).  Found mistaken naming in metadata file.

#Now all numbers match!  number of distinct key columns/sites/observer/date and 
#number of pointcount.metedata.manually.corrected rows are all same.
#pointcount.data and pointcounts.complete also match because all primary keys unique.


####################################
#transects


#metadata
transect.metadata<-read.csv("transect_metadata.csv")
transect.metadata$Start.LON<-ifelse(sign(transect.metadata$Start.LON)>0, 
                                    transect.metadata$Start.LON*-1, 
                                    transect.metadata$Start.LON*1)
transect.metadata$End.LON<-ifelse(sign(transect.metadata$End.LON)>0, 
                                    transect.metadata$End.LON*-1, 
                                    transect.metadata$End.LON*1)
transect.metadata$uniquerows<-rownames(transect.metadata)
#match transect locations to names.
#plotting gpx original points along with actual data points.
#
plot(y=transect.metadata$Start.LAT,
     x=transect.metadata$Start.LON)
plot(y=transect.metadata$End.LAT,
     x=transect.metadata$End.LON)
plot(transect_startend, add=TRUE)

#First create buffer around each original point (windmill) to be new polygon.
buf.t <- gBuffer(transect_startend, 
               width = 0.003,
               byid=TRUE,
               quadsegs = 8)
plot(buf.t, col="red", add=TRUE)
# plotting the map with those points and buffers on it for doublechecking


#make point counts into spatial data
#start
transect.metadata.clean.start<-dplyr::filter(transect.metadata,
                                         !is.na(Start.LAT),
                                         !is.na(Start.LON))
coordinates(transect.metadata.clean.start) <- c("Start.LON", "Start.LAT")
proj4string(transect.metadata.clean.start)<-proj4string(transect_startend)
#use over() to take actual points (ptsrand) and join with vegmap/buffers
transect.start.join <- sp::over(transect.metadata.clean.start, buf.t)

#end
transect.metadata.clean.end<-dplyr::filter(transect.metadata,
                                             !is.na(End.LAT),
                                             !is.na(End.LON))
coordinates(transect.metadata.clean.end) <- c("End.LON", "End.LAT")
proj4string(transect.metadata.clean.end)<-proj4string(transect_startend)
#use over() to take actual points (ptsrand) and join with vegmap/buffers
transect.end.join <- sp::over(transect.metadata.clean.end, buf.t)
transect.metadata.clean.end$END.newspotnames<-transect.end.join$id

#add column of new names back into new clean file.
transect.metadata.newnames<-transect.metadata
transect.metadata.newnames$START.newspotnames<-transect.start.join$id
END.newspotnames  <-merge(x=transect.metadata,
                                                y=as.data.frame(transect.metadata.clean.end[,
                                                                                            c("END.newspotnames",
                                                                                              "uniquerows")]),
                                                all.x=TRUE,
                                                all.y=FALSE,
                                                by=c("uniquerows"))%>% #merge the column back since it has some NAs
                                   arrange(as.numeric(uniquerows)) %>%
                                   dplyr::select(END.newspotnames) #select only the column we need

transect.metadata.newnames$END.newspotnames <-END.newspotnames[,1] #this needed because otherwise it inserts a data frame into the data frame...

#Export file, and correct any dubious ones manually in spreadsheet.
write.csv(transect.metadata.newnames,
          file="20161104_transect_metadata_newnames.csv")

#Export dubious points to .gpx file.
#turn the new data frame back into a spatialpointsdataframe.
spatial.transects<-bind_rows(data.frame("longitude"=transect.metadata.newnames$Start.LON,
                                         "latitude"=transect.metadata.newnames$Start.LAT,
                                         "name"=paste(transect.metadata.newnames$uniquerows, "start", sep=""),
                                        #requires "name" field, use "index"/uniquerows as this.
                                        #but need to paste so have unique values.
                                         "newspotnames"=transect.metadata.newnames$START.newspotnames),
                             data.frame("longitude"=transect.metadata.newnames$End.LON,
                                        "latitude"=transect.metadata.newnames$End.LAT,
                                        "name"=paste(transect.metadata.newnames$uniquerows, "end", sep=""),
                                        "newspotnames"=transect.metadata.newnames$END.newspotnames))

spatial.transects.cleaned<-dplyr::filter(spatial.transects,
                                         !is.na(spatial.transects$latitude)|!is.na(spatial.transects$longitude))
fix.transect<-spatial.transects.cleaned[is.na(spatial.transects.cleaned$newspotnames),]
#get dubious points only.


coordinates(fix.transect) <- c("longitude", "latitude")
proj4string(fix.transect)<-proj4string(transect_startend)


writeOGR(fix.transect["name"], driver="GPX", layer="waypoints", 
         dsn="20161104_unnamed_transect_startends.gpx")
#use this to compare original basecamp point list to where transect start/ends are.

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


