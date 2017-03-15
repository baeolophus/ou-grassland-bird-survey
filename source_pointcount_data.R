library(dplyr) # data manipulation
library(lubridate) #dates for year


#add in month, date, year columns for PCs.
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

