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
                          LONGITUDE >=-103 & LONGITUDE <=-94)

write.csv.ffdf(studyarea.ebird,
               file="studyarea_ebird2014.csv")



oklahoma.ebird2014<-subset(checklist2014,
                           STATE_PROVINCE=="Oklahoma")

studyarea.ebird.df<-as.data.frame(studyarea.ebird)
oklahoma.ebird.df<-as.data.frame(oklahoma.ebird2014)


write.csv.ffdf(oklahoma.ebird2014,
               file="oklahoma_ebird2014.csv")
plot(studyarea.ebird.df$LONGITUDE,
     studyarea.ebird.df$LATITUDE)
points(oklahoma.ebird.df$LONGITUDE,
       oklahoma.ebird.df$LATITUDE,
       pch="+")

delete(checklist2014)
rm(checklist2014)


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
                          LONGITUDE >=-103 & LONGITUDE <=-94)

write.csv.ffdf(studyarea.ebird2013,
               file="studyarea_ebird2013.csv")


oklahoma.ebird2013<-subset(checklist2013,
                           STATE_PROVINCE=="Oklahoma")

write.csv.ffdf(studyarea.ebird2013,
               file="studyarea_ebird2013.csv")

write.csv.ffdf(oklahoma.ebird2013,
               file="oklahoma_ebird2013.csv")
studyarea.ebird2013.df<-as.data.frame(studyarea.ebird2013)
oklahoma.ebird2013.df<-as.data.frame(oklahoma.ebird2013)

plot(oklahoma.ebird2013.df$LONGITUDE,
     oklahoma.ebird2013.df$LATITUDE)


points(studyarea.ebird2013.df$LONGITUDE,
     studyarea.ebird2013.df$LATITUDE,
     pch="+",
     add=TRUE)

delete(checklist2013)
rm(checklist2013) #these two lines delete from memory.  is okay because have subset data to get what I need.


############
#2011 and 2012

year2012<-2012

checklist2012<-read.csv.ffdf(file=paste(year2012, "/", "checklists.csv", sep=""),
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

oklahoma.ebird2012<-subset(checklist2012,
                           STATE_PROVINCE=="Oklahoma")
write.csv.ffdf(oklahoma.ebird2012,
               file="oklahoma_ebird2012.csv")
oklahoma.ebird2012.df<-as.data.frame(oklahoma.ebird2012)

plot(oklahoma.ebird2012.df$LONGITUDE,
     oklahoma.ebird2012.df$LATITUDE)



delete(checklist2012)
rm(checklist2012) #these two lines delete from memory.  is okay because have subset data to get what I need.

delete(oklahoma.ebird2012)
rm(oklahoma.ebird2012) #these two lines delete from memory.  is okay because have subset data to get what I need.


year2011<-2011

checklist2011<-read.csv.ffdf(file=paste(year2011, "/", "checklists.csv", sep=""),
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

oklahoma.ebird2011<-subset(checklist2011,
                           STATE_PROVINCE=="Oklahoma")
write.csv.ffdf(oklahoma.ebird2011,
               file="oklahoma_ebird2011.csv")
oklahoma.ebird2011.df<-as.data.frame(oklahoma.ebird2011)

plot(oklahoma.ebird2011.df$LONGITUDE,
     oklahoma.ebird2011.df$LATITUDE)



delete(checklist2011)
rm(checklist2011) #these two lines delete from memory.  is okay because have subset data to get what I need.

