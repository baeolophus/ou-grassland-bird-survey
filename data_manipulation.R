#libraries used in this file.
library(dplyr) # data manipulation
library(fuzzyjoin) #for getting ebird data out by spatial location
library(geosphere) #distances between transects
library(lubridate) #dates for year
library(rgdal) # for reading different spatial file formats
#####

#Polishing up data formats and combining the different sources.
#then merge with sightings and do all data checks as for point counts.
source(file = "source_transect_data.R")
source(file = "source_pointcount_data.R")

####################################
#Formatting ebird to fit into same format as transects and PCs.
#To start, get list of species codes from point counts and from transects
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

####################
#ebird import
ebird2013<-read.csv(file="bigfiles\\studyarea_ebird2013.csv")
ebird2014<-read.csv(file="bigfiles\\studyarea_ebird2014.csv")

#select only species columns from our surveys plus the metadata columns.
selected.ebird.sp.2013<-dplyr::select(ebird2013, #original dataframe
                             1:19, #metadata columns from original dataframe
                             one_of(pull.these.columns.from.ebird)) #select species in our surveys

selected.ebird.sp.2014<-dplyr::select(ebird2014, #original dataframe
                                      1:19, #metadata columns from original dataframe
                                      one_of(pull.these.columns.from.ebird)) #select species in our surveys

#Then change format to one row per species.
#gives an error about losing attributes but this is because the count columns are factors.
#http://stackoverflow.com/questions/28972386/retain-attributes-when-using-gather-from-tidyr-attributes-are-not-identical
#The values seem okay so I'm leaving it for now.
#It already gives days where absences occured.  Will need to filter so they are complete counts that give absences.
gathered.ebird.data.2013<-selected.ebird.sp.2013%>%
  mutate_each(funs(as.character),
              one_of(pull.these.columns.from.ebird))%>%
  tidyr::gather(key=species,
                value=Quantity,
                one_of(pull.these.columns.from.ebird))
gathered.ebird.data.2014<-selected.ebird.sp.2014%>%
  mutate_each(funs(as.character), 
              one_of(pull.these.columns.from.ebird))%>%
  tidyr::gather(key=species, 
                value=Quantity, 
                one_of(pull.these.columns.from.ebird))


#Then combine into one big ebird file
gathered.ebird.data.all<-rbind(gathered.ebird.data.2013,
                               gathered.ebird.data.2014)

########################
#Check on effort for transects
#get maximum length of transect (/60 to convert to hrs to compare to ebird), rounded to 2 sig digits.
ebird.effort_hrs.cutoff <- signif(max(transect.complete$lengthoftransect.time,
                                  na.rm=TRUE)/60,
                              2)
#get maximum length of transect (/1000 to convert to km to compare to ebird), rounded to 2 sig digits.
ebird.effort_distance_km.cutoff <- signif(max(transect.complete$transect.distance,
                                  na.rm=TRUE)/1000,
                              2)

########################
##Eliminate ebird duplicates because some survey data was uploaded to ebird.

#First filter checklists to OK only with effort matching ours and
#eliminate non-primary checklists (where people submitted more than one checklist for one birding event)
#and casual counts.
ebird.cleaned<-gathered.ebird.data.all%>%
  filter(PRIMARY_CHECKLIST_FLAG==1, #get only the first checklist for an event
         STATE_PROVINCE=="Oklahoma",
         EFFORT_HRS <= ebird.effort_hrs.cutoff, #get only transects that are < in time to match our data.
         EFFORT_DISTANCE_KM <= ebird.effort_distance_km.cutoff, #get only transects that are < in length to match our data
         COUNT_TYPE!="P20") #eliminate casual counts


#from ebird documentation: "What kind of observation the sample is:
#stationary (P21), traveling (P22, P34), 
#area (P23, P35), 
#casual (P20), or random (P48).
#Protocol P34 is a small amount of data contributed from the Rocky
#Mountain Bird Observatory that we believe is high quality.
#Protocol P35 data are back-yard area counts made on consecutive days
#(see http://www.birds.cornell.edu/MyYardCounts).
#Email from cornell said the ebird reference dataset IS complete counts only.

#######################
#Get primary keys for all three datasets.

ebird.sampling.ids<-dplyr::select(ebird.cleaned,
                                  YEAR,
                                  DAY,
                                  TIME,
                                  SAMPLING_EVENT_ID,
                                  LONGITUDE,
                                  LATITUDE)%>%
  dplyr::distinct(SAMPLING_EVENT_ID, #primary key
                  .keep_all=TRUE)

transect.primary.keys<-dplyr::select(transect.complete, 
                                     Date,
                                     Observer,
                                     Location,
                                     spot=Transect,
                                     ebird.day,
                                     ebird.time,
                                     year,
                                     Longitude=Start.LON, 
                                     Latitude=Start.LAT)%>%
  dplyr::distinct(Date, Observer, Location, spot, #primary key combo
                  .keep_all=TRUE)
pointcount.primary.keys<-dplyr::select(pointcounts.complete, 
                                     Date, 
                                     Observer, 
                                     Location,
                                     spot=Point,
                                     ebird.day,
                                     ebird.time,
                                     year,
                                     Longitude, 
                                     Latitude)%>%
  dplyr::distinct(Date, Observer, Location, spot, #primary key combo
                  .keep_all=TRUE)

primary.keys<-rbind(transect.primary.keys,
                    pointcount.primary.keys)


###########################
#Now to find out which dates and times and locations overlap from ours to ebird.

#Use spatial buffer for point counts and transects
#15 km buff
sampling.ids.that.we.input<-fuzzyjoin::geo_left_join(x=primary.keys,
                                        y=ebird.sampling.ids,
                               max_dist=15,
                               unit="km",
                               by=c("Longitude"="LONGITUDE", 
                                    "Latitude"="LATITUDE"))%>%
  filter(.,
       DAY==ebird.day,
       YEAR==year,
       TIME>=(ebird.time-1)&TIME<=(ebird.time+1))%>% #everything within one hour plus or minus
  #Then get out which ebird checklist codes, these are the ones that will be eliminated.
  distinct(SAMPLING_EVENT_ID)

#get the list of the ebird sampling_ids that overlap.
omit.these<-as.character(sampling.ids.that.we.input[,1])

complete.list.of.jeremy.samples<-c("S18101887", #Grady County WMA point counts on 4/17/2014. PCs
                               "S18137660", #Tallgrass Prairie Preserve on 4/23/2014. transect.
                               "S18137507", #Rita Blanca point counts on 4/25/2014. PCs
                               "S18434697", #Cimmaron Bluffs WMA on 5/16/2014 transects
                               "S18434249") #Cimmaron Hills WMA on 5/17/2014 transects
#These are all from Jeremy.
#These are all included in the filtered "omit.these" so it works for known ebird samples.
#It is unclear how many other people entered surveys or "presurvey" birds,
#so we will eliminate all sampling events in the "omit.these" list.

#filter out the overlapping 14 samples.
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

#Join with name table to have common, banding code, and scientific names.
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

##############################
#Creating one record per species per survey (PC or transect) as we can only have one each.
#Because it is simple presence/absence we can just select one randomly.
#This gets one random species sighting per point count or transect.
#For point counts, there is only one lat/long.
#The midpoint of the transect is used so it doesn't matter which one is selected.
#However, if we later decided to use exact sightings, replace longitude and latitude with sighting.LON and sighting.LAT
#and it gives you the point on the transect where the observer was for that randomly selected sighting per species.

pc.species.per.transect<-dplyr::group_by(pointcounts.complete,
                                         SAMPLING_EVENT_ID,
                                         Species)%>%
  do(.,
     sample_n(., 1))%>%ungroup(.)
pc.combine<-dplyr::filter(pc.species.per.transect,
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
                   Quantity,
                   effort_time = lengthoftransect.time,
                   effort_length = transect.distance
                   )

tr.species.per.transect<-dplyr::group_by(transect.complete,
                                         SAMPLING_EVENT_ID,
                                         Possible.Species)%>%
  do(.,
     sample_n(., 1))%>%ungroup(.)

tr.combine<-dplyr::filter(tr.species.per.transect,
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
                   Longitude=midpoint.LON,
                   Latitude=midpoint.LAT,
                   Quantity=Quantity.corrected,
                   effort_time = lengthoftransect.time,
                   effort_length = transect.distance)

#combine the point counts and the transects
oursurveys.combined<-bind_rows(tr.combine,
                               pc.combine)
#remove the groupings I used earlier.
oursurveys.combined<-ungroup(oursurveys.combined)

#our data are presence/absence only for the species in each sampling event.
#This generates presence/absence rows for all species by each sampling event.
oursurveys.generate.presence<-dplyr::filter(oursurveys.combined,
                                             !is.na(SCINAME)) %>%
  #Get everything that has a scientific name (this eliminates "sp" entries and unknowns).
                               tidyr::spread(key=SCINAME,
                                             value=Quantity, 
                                             fill=0) %>%
  #spread out so that the key values (SCINAME) become different columns, and wherever there is an empty row, insert 0 in cell.
                               tidyr::gather(key=SCINAME,
                                             value=Quantity,
                                             one_of(pull.these.columns.from.ebird)) %>%
  #gather all the species columns back now that the zero rows have been generated.
                               dplyr::arrange(SAMPLING_EVENT_ID)
  #arrange by sampling event ID so that all entries from a survey date/observer are in same region for ease of reading.

#necessary to convert to character to combine with ebird
oursurveys.generate.presence$Quantity<-as.character(oursurveys.generate.presence$Quantity)

#Select ebird columns and give appropriate names to match.
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
                      Quantity,
                      effort_time = EFFORT_HRS,
                      effort_length = EFFORT_DISTANCE_KM) %>%
                dplyr::arrange(SAMPLING_EVENT_ID)

#check that all have same columns in same order though with different names.
str(combine.ebird)
str(oursurveys.generate.presence)

#combine.
complete.dataset.for.sdm<-rbind(oursurveys.generate.presence,
                                    combine.ebird)

#Then generate new quantity and presence/absence columns, as ebird quantity includes "x" for presence.
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

#Add back in the species codes and the common names to go with the already-present scientific names.
complete.dataset.for.sdm<-left_join(complete.dataset.for.sdm,
                                         aou.codes[,c("SPEC",
                                                      "COMMONNAME",
                                                      "SCINAME")],
                                         by=c("SCINAME.space"="SCINAME"))
#Rearrange the dataset so that if viewed in a spreadsheet it will be organized by datasource and sampling event ID (primary key).
complete.dataset.for.sdm<-dplyr::arrange(complete.dataset.for.sdm,
                                         datasource,
                                         SAMPLING_EVENT_ID)

#Write this to a file.  includes records without lat/long.
write.csv(complete.dataset.for.sdm,
          "completedatasetforsdm.csv")

#Masking for just oklahoma

#Import oklahoma polygon mask.
state<-readOGR(dsn="E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed",
               layer="ok_state_vector_smallest_pdf_3158")

state<-spTransform(x = state,
                   CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
)

##make it spatial (needed to do the spatial subset), remembering these values are lat/long in decimal degrees
#eliminate missing values first.
complete.dataset.for.sdm.na <- complete.dataset.for.sdm %>%
  dplyr::filter(!is.na(Longitude) & !is.na(Latitude))

#make it spatial
coordinates(complete.dataset.for.sdm.na)<-c("Longitude", "Latitude")

#specify projection
proj4string(complete.dataset.for.sdm.na)<-CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Then convert to utm
complete.dataset.for.sdm.na.utm <- spTransform(complete.dataset.for.sdm.na,
                                CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
#confirm UTM
proj4string(complete.dataset.for.sdm.na.utm)

#Subset responses to get it to match the Oklahoma boundary for species distribution predictor rasters.
#Doesn't remove anything actually.
oklahoma.dataset.for.sdm.na.utm<-complete.dataset.for.sdm.na.utm[state,]

#View the records.
plot(oklahoma.dataset.for.sdm.na.utm)

#Write to file
write.csv(as.data.frame(oklahoma.dataset.for.sdm.na.utm),
          "oklahomadatasetforsdm_naomit_utm.csv")

