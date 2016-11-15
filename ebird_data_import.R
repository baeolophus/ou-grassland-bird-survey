setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ebird_data/")

#ebird.extradata<-read.csv(file="srd_point_data_3km_us48_v2014.csv")

year<-2014

library(ff)
library(ffbase)

checklist2014<-read.csv.ffdf(file=paste(year, "/", "checklists.csv", sep=""),
                                        colClasses=NA, 
                             transFUN=function(x){x[,20:953] <- as.factor(x[,20:953]); x})

#https://rpubs.com/msundar/large_data_analysis
#http://stackoverflow.com/questions/6616020/problem-with-specifying-colclasses-in-read-csv-in-r
#http://www.bnosac.be/index.php/blog/22-if-you-are-into-large-data-and-work-a-lot-package-ff
#http://stackoverflow.com/questions/13186077/work-in-r-with-very-large-data-set

coordinates(checklist2014)<-c("DECIMAL_LONGITUDE",
                      "DECIMAL_LATITUDE")


studyarea.ebird<-crop(checklist2014,
     studyarea.extent)

plot(studyarea.ebird,
     add=TRUE)

write.csv(studyarea.ebird,
          "studyarea_ebird_data.csv")