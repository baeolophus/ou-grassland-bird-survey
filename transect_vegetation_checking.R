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

#check to make sure no NAs in metadata rows.  Did that and corrected so all veg have metadata matches.

#then this code gets distinct transects from veg.
veg.distincts<-distinct(transect.veg,
                           Date,
                           Observer,
                           Location,
                           Transect)

#There are only 50 vegetations out of 167 transects.  Is this even useful?

matching.metadata.to.veg<-left_join(transect.metadata,
                                    veg.distincts,
                                    by=c("Date",
                                         "Observer",
                                         "Location",
                                         "Transect"))
