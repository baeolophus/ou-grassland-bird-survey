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


#get scientific names for all
aou.codes<-foreign::read.dbf(file="AOUcodes2016.dbf")


###############
#Specify data sources for impending combination.

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

transect.complete$allnotes <- paste(transect.complete$Notes.x,
                                    transect.complete$Notes.y,
                                    sep = ".")

pointcounts.complete$allnotes <- paste(pointcounts.complete$Notes.x,
                                       pointcounts.complete$Notes.y,
                                       sep = ".")


#observer, species observed, observation date, location. Habitat information and other details are also helpful.

#get same columns for each set

pc.combine<-dplyr::select(pointcounts.complete,
                          Observer,
                          COMMONNAME,
                          SCINAME,
                          year,
                          month,
                          day,
                          Longitude,
                          Latitude,
                          Location,
                          PointorTransect=Point,
                          TIME,
                          datasource,
                          Quantity,
                          Notes = allnotes
  )

tr.combine<-dplyr::select(transect.complete,
         Observer,
         COMMONNAME,
         SCINAME,
         year,
         month,
         day,
         Longitude=sighting.LON,
         Latitude=sighting.LAT,
         Location,
         PointorTransect=Transect,
         TIME,
         datasource,
         Quantity=Quantity.corrected,
         Notes = allnotes)



#combine the point counts and the transects
oursurveys.combined<-bind_rows(tr.combine,
                               pc.combine)



oursurveys.combined <- oursurveys.combined %>%
  filter(!is.na(COMMONNAME))

oursurveys.combined$SCINAME<-gsub("_",
                                  " ",
                                  oursurveys.combined$SCINAME,
                                  fixed=TRUE)

levels(as.factor(oursurveys.combined$Observer))

oursurveys.combined$Observer<-gsub("Crystina",
                                  "Crystina Myers",
                                  oursurveys.combined$Observer,
                                  fixed=TRUE)


oursurveys.combined$Observer<-gsub("MAB",
                                   "Marissa A. Buschow",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$Observer<-gsub("ESB",
                                   "Eli S. Bridge",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$Observer<-gsub("JBT",
                                   "Jeff Tibbits",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$Observer<-gsub("JDR",
                                   "Jeremy D. Ross",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$Observer<-gsub("Jessica",
                                   "Jessica Hightower",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$Observer<-gsub("JGK",
                                   "Jonathan G. Kruk",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$Observer<-gsub("Josh",
                                   "Josh Haughawout",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$Observer<-gsub("Kristen",
                                   "Kristen Oliver",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)


oursurveys.combined$Observer<-gsub("MDF",
                                   "Matthew D. Fuirst",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$Observer<-gsub("Randy",
                                   "Randy Soto",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$Observer<-gsub("Roy",
                                   "Roy Feliciano",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$Observer<-gsub("Tory",
                                   "Tory Smith",
                                   oursurveys.combined$Observer,
                                   fixed=TRUE)

oursurveys.combined$year<-gsub("14",
                               "2014",
                               oursurveys.combined$year,
                               fixed=TRUE)
oursurveys.combined$year<-gsub("13",
                               "2013",
                               oursurveys.combined$year,
                               fixed=TRUE)

oursurveys.combined$year<-as.numeric(oursurveys.combined$year)


write.csv(oursurveys.combined,
          file = "oklahoma_biological_survey_dataset.csv")
