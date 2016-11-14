setwd("E:\\Documents\\college\\OU-postdoc\\research\\grassland_bird_surveys\\ebird_data\\")

ebird<-read.csv(file="srd_point_data_3km_us48_v2014.csv")

year<-2014
checklist2014<-read.csv(file=paste(year, "/", "checklists.csv", sep=""))

coordinates(ebird)<-c("DECIMAL_LONGITUDE",
                      "DECIMAL_LATITUDE")


studyarea.ebird<-crop(ebird,
     studyarea.extent)

plot(studyarea.ebird,
     add=TRUE)

write.csv(studyarea.ebird,
          "studyarea_ebird_data.csv")