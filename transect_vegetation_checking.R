#vegetation testing

library(dplyr)
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

transect.veg<-read.csv("transect_vegetation.csv")

transect.veg.metadata<-left_join(transect.veg,
                                 transect.metadata,
                                 by=c("Date",
                                      "Observer",
                                      "Location",
                                      "Transect"))
