#test running dismo with maxent
library(dismo)
library(rJava)
##check to see if I have java
jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar) & require(rJava)) {}

#test running maxent in biomod2, which can do ensembles
library(biomod2)
system("java -jar maxent.jar") #runs java from the working directory.
#if working correctly brings up maxent window.

library(dplyr)
#bring in data and metadata, join for species locations.
pointcount.data<-read.csv(file="pointcount_data.csv")
#bring in the manually corrected file
pointcount.metadata.manually.corrected<-read.csv(file="20161003_pointcount_metadata_newnames_manual_corrections.csv")
#merge the files so every row has all metadata attached.
pointcounts.complete<-left_join(pointcount.data,
                                pointcount.metadata.manually.corrected,
                                by=c("Date",
                                     "Observer",
                                     "Location",
                                     "Point"))
##pointcounts.complete row count should always match pointcount.data count if joining accurately.
#if not, go back to data_manipulation file for code on finding where errors are.
#Create location column
pointcounts.complete$locationcode<-substr(pointcounts.complete$newspotnames, 1, 4)
######
#Summary tables
######
#Number of species in each location
species.per.locationcode<-distinct(pointcounts.complete,
                                   locationcode,
                                   Species)%>%
                                   group_by(locationcode)%>%
                          summarize(speciescount=n())
#Number of sightings for each species by location


sightings.per.species.per.location<-group_by(pointcounts.complete,
                                             locationcode, Species)%>%
  summarize(speciescount=n())

library(reshape2)

melt.data<-melt(sightings.per.species.per.location,id=c("locationcode", "Species"),
                measured=c("speciescount")) #variable measured is response
melt.data #show data

melt.data$variable<-melt.data$locationcode
#Set the variable here.  "groups" is the factor that we want to make #up our four new columns.

data.columns<-dcast(melt.data, Species~variable,mean)
data.columns #species in rows, locations in columns

#replace NaN with O
data.columns[is.nan(data.columns)]<-0

data.columns2<-as.data.frame(lapply(X=data.columns,
                      FUN=gsub,
                      pattern=NaN,
                      replacement=0,
                      fixed=TRUE))
data.columns2[,2:23]<-lapply(X=data.columns2[,2:23],
                      FUN=as.character)
data.columns2[,2:23]<-lapply(X=data.columns2[,2:23],
                             FUN=as.numeric)
data.columns2$sumsightings<-base::rowSums(data.columns2[,2:23])
data.columns2<-dplyr::arrange(data.columns2,
                              -sumsightings)

#For first preliminary models for Oct. 15 meeting will do EAME, CASP, and DICK.
#For later, important species probably CASP, LASP, EAME, WEME, DICK, 
#GRSP, NOBO, HOLA, PABU, STFL, FISP, CONI.
#need to find out what threshold for number of sightings needed for SDMs is.

#remove sightings where lat/long is.na
pointcounts.complete.na.eame<-filter(pointcounts.complete,
                             !is.na(Latitude)&
                             !is.na(Longitude)&
                               Species=="DICK")


#this preliminary model adapted from: 
#http://www.molecularecologist.com/2013/04/species-distribution-models-in-r/

library(maptools)
library(maps)
library(mapdata)
data(stateMapEnv)

plot(c(min(pointcounts.complete.na.eame$Longitude, na.rm=TRUE),
       max(pointcounts.complete.na.eame$Longitude, na.rm=TRUE)),
     c(min(pointcounts.complete.na.eame$Latitude, na.rm=TRUE), 
       max(pointcounts.complete.na.eame$Latitude, na.rm=TRUE)),
     mar=par("mar"),
     xlab="Longitude",
     ylab="Latitude",
     xaxt="n", yaxt="n", type="n",
     main="Eastern Meadowlark presence data")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
     col="lightblue")
map("state",
    xlim=c(min(pointcounts.complete.na.eame$Longitude, na.rm=TRUE),
                    max(pointcounts.complete.na.eame$Longitude, na.rm=TRUE)),
    ylim=c(min(pointcounts.complete.na.eame$Latitude, na.rm=TRUE), 
           max(pointcounts.complete.na.eame$Latitude, na.rm=TRUE)),
    fill=T,
    col="cornsilk", add=T)
points(pointcounts.complete.na.eame$Longitude,
       pointcounts.complete.na.eame$Latitude)
axis(1,las=1)
axis(2,las=1)
box()

#sampling https://rdrr.io/cran/dismo/man/gridSample.html
res <- 0.05 #resolution of cell
r <- raster(extent(range(pointcounts.complete.na.eame[,"Longitude"]),
                   range(pointcounts.complete.na.eame[,"Latitude"])) + res) 
#create raster grid for sampling with given resolution
res(r) <- res #apply resolution

subs <- gridSample(pointcounts.complete.na.eame[,c("Longitude","Latitude")], 
                   r, #raster grid
                   n=1) #number of samples per cell
points(subs, pch='x', col='red')

x<-circles(subs[,c("Longitude","Latitude")], d=50000, lonlat=T)
bg<-spsample(x@polygons, 1000, type='random', iter=1000)
plot(x, add=T)
plot(bg, add=TRUE)

#import bioclim layers
library(raster)
bio1<-raster('bio_5m_bil/bio1.bil')
bio2<-raster('bio_5m_bil/bio2.bil')
bio3<-raster('bio_5m_bil/bio3.bil')
bio4<-raster('bio_5m_bil/bio4.bil')
bio5<-raster('bio_5m_bil/bio5.bil')
bio6<-raster('bio_5m_bil/bio6.bil')
bio7<-raster('bio_5m_bil/bio7.bil')
bio8<-raster('bio_5m_bil/bio8.bil')
bio9<-raster('bio_5m_bil/bio9.bil')
bio10<-raster('bio_5m_bil/bio10.bil')
bio11<-raster('bio_5m_bil/bio11.bil')
bio12<-raster('bio_5m_bil/bio12.bil')
bio13<-raster('bio_5m_bil/bio13.bil')
bio14<-raster('bio_5m_bil/bio14.bil')
bio15<-raster('bio_5m_bil/bio15.bil')
bio16<-raster('bio_5m_bil/bio16.bil')
bio17<-raster('bio_5m_bil/bio17.bil')
bio18<-raster('bio_5m_bil/bio18.bil')
bio19<-raster('bio_5m_bil/bio19.bil')
bioclimlayer<-brick(bio1,
                    bio2,
                    bio3,
                    bio4,
                    bio5,
                    bio6,
                    bio7,
                    bio8,
                    bio9,
                    bio10,
                    bio11,
                    bio12,
                    bio13,
                    bio14,
                    bio15,
                    bio16,
                    bio17,
                    bio18,
                    bio19)


#crop to extent of study area
studyarea.extent<-extent(-103,-96,
                         33,38) # define the extent
studyarea.bioclim<-crop(bioclimlayer,
                    studyarea.extent)


#plot bioclim and points
plot(studyarea.bioclim,
     1,
     cex=0.5,
     legend=T,
     mar=par("mar"),
     xaxt="n",
     yaxt="n",
     main="Annual mean temperature (ºC x 10)")
map("state", xlim=c(-103,-96), ylim=c(33,38), fill=F,
    col="cornsilk", add=T)

#extract values for analysis
eame.bioclim<-extract(studyarea.bioclim,
                  c(subs$Longitude,
                    subs$Latitude))
eame.bg.bioclim<-extract(studyarea.bioclim,
                         bg)

eame.bioclim.df<-data.frame(lon=subs$Longitude,
                            lat=subs$Latitude,
                            eame.bioclim)

bgpoints<-bg@coords
colnames(bgpoints)<-c("lon","lat")
bg_bc<-data.frame(cbind(bgpoints,eame.bg.bioclim))
length(which(is.na(bg_bc$bio1))) # double-check for missing data
bg_bc<-bg_bc[!is.na(bg_bc$bio1), ] # and pull out the missing lines


group_p<-kfold(eame.bioclim.df, 4)
pres_train<-eame.bioclim.df[group_p!=1,
                        c("lon",
                          "lat")]
pres_test<-eame.bioclim.df[group_p==1,
                       c("lon",
                         "lat")]

abs_train<-bg_bc[group_p!=1,
                            c("lon",
                              "lat")]
abs_test<-bg_bc[group_p==1,
                           c("lon",
                             "lat")]

maxent.eame<-dismo::maxent(x=studyarea.bioclim,
                   p=pres_train,
                   a=abs_train)
(eval.eame<- dismo::evaluate(maxent.eame,
                            p=pres_test,
                            a=abs_test,
                            x=studyarea.bioclim))

plot(maxent.eame)
response(maxent.eame)


predict.eame<-predict(maxent.eame,
                      studyarea.bioclim)

plot(predict.eame,
     1,
     cex=0.5,
     legend=T,
     mar=par("mar"),
     xaxt="n",
     yaxt="n",
     main="Predicted presence of Dickcissel")
map("state", xlim=c(-103,-96), ylim=c(33,38), fill=F,
    col="cornsilk", add=T)

points(subs, pch=20, bg='black')
axis(1,las=1)
axis(2,las=1)
box()