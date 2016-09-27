#libraries used in this file.
library(dplyr)

#import data from csv files.
######
#point counts

#sightings
pointcount.data<-read.csv()
#metadata
pointcount.metadata<-read.csv()
#merge the files so every row has all metadata attached.
pointcount.complete<-left_join(pointcount.data,
                               pointcount.metadata,
                               by)


#transects
  
#sightings
transect.data<-read.csv()
#metadata
transect.metadata<-read.csv()
#merge the files so every row has all metadata attached.
transect.complete<-left_join(transect.data,
                             transect.metadata,
                             by)
######

#
#generate absences for each species 
#these are needed for species distribution models