#libraries used in this file.
library(dplyr) # data manipulation
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
ebird2011<-read.csv(file="E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ebird_data/oklahoma_ebird2011.csv")
ebird2012<-read.csv(file="E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ebird_data/oklahoma_ebird2012.csv")

#select only species columns from our surveys plus the metadata columns.
selected.ebird.sp.2011<-dplyr::select(ebird2011, #original dataframe
                             1:19, #metadata columns from original dataframe
                             one_of(pull.these.columns.from.ebird)) #select species in our surveys

selected.ebird.sp.2012<-dplyr::select(ebird2012, #original dataframe
                                      1:19, #metadata columns from original dataframe
                                      one_of(pull.these.columns.from.ebird)) #select species in our surveys

#Then change format to one row per species.
#gives an error about losing attributes but this is because the count columns are factors.
#http://stackoverflow.com/questions/28972386/retain-attributes-when-using-gather-from-tidyr-attributes-are-not-identical
#The values seem okay so I'm leaving it for now.
#It already gives days where absences occured.  Will need to filter so they are complete counts that give absences.
gathered.ebird.data.2011<-selected.ebird.sp.2011%>%
  mutate_each(funs(as.character),
              one_of(pull.these.columns.from.ebird))%>%
  tidyr::gather(key=species,
                value=Quantity,
                one_of(pull.these.columns.from.ebird))
gathered.ebird.data.2012<-selected.ebird.sp.2012%>%
  mutate_each(funs(as.character), 
              one_of(pull.these.columns.from.ebird))%>%
  tidyr::gather(key=species, 
                value=Quantity, 
                one_of(pull.these.columns.from.ebird))


#Then combine into one big ebird file
gathered.ebird.data.all<-rbind(gathered.ebird.data.2011,
                               gathered.ebird.data.2012)

########################
#Check on effort for transects
summary(transect.complete$lengthoftransect.time,
    na.rm=TRUE)/60
summary(transect.complete$transect.distance,
    na.rm=TRUE)/1000

transect.complete$SAMPLING_EVENT_ID<-paste(transect.complete$Date,
                                           transect.complete$Observer,
                                           transect.complete$Location,
                                           transect.complete$Transect,
                                           sep="_")

narrowed <- transect.complete %>% group_by(SAMPLING_EVENT_ID) %>%
  distinct(SAMPLING_EVENT_ID, .keep_all = TRUE)
ungroup(narrowed) %>% summarize (meantime = mean(lengthoftransect.time/60, na.rm = TRUE),
             meanlength = mean(transect.distance/1000, na.rm = TRUE),
             sdtime = sd(lengthoftransect.time/60, na.rm = TRUE),
             sdlength = sd(transect.distance/1000, na.rm = TRUE),
             minlength = min(transect.distance/1000, na.rm = TRUE),
             maxlength = max(transect.distance/1000, na.rm = TRUE),
             mintime = min(lengthoftransect.time/60, na.rm = TRUE),
             maxtime = max(lengthoftransect.time/60, na.rm = TRUE))

##############################

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
ebird.complete<-gathered.ebird.data.all%>%
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

###############
##MAKING COMBINED SINGLE DATA SHEET for presence/absence data
###############
#If there are any more changes to data,
#then edit original ones and rerun this script.
###############
#Specify data sources for impending combination.
ebird.complete$datasource<-"EBIRD"

ebird.complete$SCINAME.spaces<-gsub("_", " ", as.character(ebird.complete$species))
ebird.complete<-left_join(ebird.complete,
                             aou.codes,
                             by=c("SCINAME.spaces"="SCINAME"))

ebird.complete$SCINAME<-gsub(" ", "_", as.character(ebird.complete$SCINAME.spaces))


##############################
#Select ebird columns and give appropriate names to match.
complete.dataset.for.sdm<-select(ebird.complete,
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
          "evaluation_datasetforsdm.csv")

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
          "oklahoma_evaluation_datasetforsdm_naomit_utm.csv")

