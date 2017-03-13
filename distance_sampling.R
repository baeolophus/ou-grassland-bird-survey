library(Distance)

data(book.tee.data)
tee.data<-book.tee.data$book.tee.dataframe[book.tee.data$book.tee.dataframe$observer==1,]
ds.model <- ds(tee.data,4)
summary(ds.model)
plot(ds.model)

ds.minke<-ds(minke, truncation="10%")
plot(ds.minke)
summary(ds.minke)

ds.gof(ds.minke)

#Transect analysis
#import data
complete.dataset.for.distance <- read.csv(file = "oklahomadatasetforsdm_naomit_utm.csv")
#format transects with distance, Sample.Label (transectID), Effort (line length), Region Label, Area (area of the region).

#should be able to automate everything below with only "SPEC" as input.
complete.dataset.for.distance.DICK<-dplyr::filter(complete.dataset.for.sdm,
                                             SPEC=="DICK",
                                             month == 4 | month == 5 | month == 6,
                                             surveytype == "TRANSECT",
                                             presence == 1)


#Get area of region with extent around each "name".  Do I want region?


#Point count analysis
#here a section to count up total number of visits per PC, tag back on to data file.

#Then function as above.