setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ebird_data/")

#ebird.extradata<-read.csv(file="srd_point_data_3km_us48_v2014.csv")

year<-2014

library(ff)
library(ffbase)

options("fftempdir"="tempff/")


checklist2014<-read.csv.ffdf(file=paste(year, "/", "checklists.csv", sep=""),
                                        colClasses=c(
                                          rep('factor', 2),
                                          rep('numeric', 6),
                                          rep('factor', 4),
                                          rep('numeric', 3),
                                          rep('factor', 1),
                                          rep('numeric', 1),
                                          rep('factor', 2),
                                          rep('factor', 933)),
                             na.strings="?")


#https://rpubs.com/msundar/large_data_analysis
#http://stackoverflow.com/questions/6616020/problem-with-specifying-colclasses-in-read-csv-in-r
#http://www.bnosac.be/index.php/blog/22-if-you-are-into-large-data-and-work-a-lot-package-ff
#http://stackoverflow.com/questions/13186077/work-in-r-with-very-large-data-set
#http://stats.stackexchange.com/questions/1471/is-it-possible-to-directly-read-csv-columns-as-categorical-data
#studyarea.extent<-extent(-103,-96,
#                         33,38) 
studyarea.ebird<-subset(checklist2014,
                        LATITUDE>=33 & LATITUDE<=38 &
                          LONGITUDE >=-103 & LONGITUDE <=-96)

write.csv.ffdf(studyarea.ebird,
               file="studyarea_ebird2014.csv")

studyarea.ebird.df<-as.data.frame(studyarea.ebird)

plot(studyarea.ebird.df$LONGITUDE,
     studyarea.ebird.df$LATITUDE)



year2013<-2013


checklist2013<-read.csv.ffdf(file=paste(year2013, "/", "checklists.csv", sep=""),
                             colClasses=c(
                               rep('factor', 2),
                               rep('numeric', 6),
                               rep('factor', 4),
                               rep('numeric', 3),
                               rep('factor', 1),
                               rep('numeric', 1),
                               rep('factor', 2),
                               rep('factor', 933)),
                             na.strings="?")

studyarea.ebird2013<-subset(checklist2013,
                        LATITUDE>=33 & LATITUDE<=38 &
                          LONGITUDE >=-103 & LONGITUDE <=-96)

write.csv.ffdf(studyarea.ebird2013,
               file="studyarea_ebird2013.csv")

studyarea.ebird2013.df<-as.data.frame(studyarea.ebird2013)

plot(studyarea.ebird2013.df$LONGITUDE,
     studyarea.ebird2013.df$LATITUDE,
     pch="+",
     add=TRUE)

delete(checklist2013)
rm(checklist2013) #these two lines delete from memory.  is okay because have subset data to get what I need.

