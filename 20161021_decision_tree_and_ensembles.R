#testing creation of ensemble model designed after Fink et al. 2010's STEM models.

#import data
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

pointcounts.complete.na.dick<-filter(pointcounts.complete,
                                     !is.na(Latitude)&
                                       !is.na(Longitude)&
                                       Species=="DICK")

#sampling https://rdrr.io/cran/dismo/man/gridSample.html
library(dismo)
res <- 0.05 #resolution of cell
r <- raster(extent(range(pointcounts.complete.na.dick[,"Longitude"]),
                   range(pointcounts.complete.na.dick[,"Latitude"])) + res) 
#create raster grid for sampling with given resolution
res(r) <- res #apply resolution

subs <- gridSample(pointcounts.complete.na.dick[,c("Longitude","Latitude")], 
                   r, #raster grid
                   n=1) #number of samples per cell
#bring in bioclim data
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


#extract values for analysis
bioclim.dick<-as.data.frame(extract(x=studyarea.bioclim,
                      y=c(subs$Longitude,
                        subs$Latitude)))


subs.bioclim<-cbind(subs,
                    na.omit(bioclim.dick))
subs.bioclim$response<-1
x<-circles(subs[,c("Longitude","Latitude")], d=50000, lonlat=T)
bg<-spsample(x@polygons, 1000, type='random', iter=1000)
bg.bioclim.dick<-as.data.frame(extract(studyarea.bioclim,
                         bg))
bgpoints<-bg@coords
colnames(bgpoints)<-c("Longitude","Latitude")

bg.bioclim.dick<-cbind(as.data.frame(bgpoints),
                       bg.bioclim.dick)

bg.bioclim.dick$response<-0

tree.data.dick<-rbind(bg.bioclim.dick,
                      subs.bioclim)

#Create decision tree that works using rpart.
library(rpart)
tree.test<-rpart(response~bio1+
                   bio2+
                   bio3+
                   bio4+
                   bio5+
                   bio6+
                   bio7+
                   bio8+
                   bio9+
                   bio10+
                   bio11+
                   bio12+
                   bio13+
                   bio14+
                   bio15+
                   bio16+
                   bio17+
                   bio18+
                   bio19,
                 data=tree.data.dick,
                 method="class",
                 control=rpart.control(cp=0.0000000000000000000000000001))
summary(tree.test)
plot(tree.test)

tree.test2<-lm(response~bio1+
                 bio2+
                 bio3+
                 bio4+
                 bio5+
                 bio6+
                 bio7+
                 bio8+
                 bio9+
                 bio10+
                 bio11+
                 bio12+
                 bio13+
                 bio14+
                 bio15+
                 bio16+
                 bio17+
                 bio18+
                 bio19,
                                data=tree.data.dick)

summary(tree.test2)
#check out Dobrowski et al. 2011; ecological monographs.

#create prediction map for illustration purposes
tree.test.raster.prediction<-raster::predict(object=studyarea.bioclim, #raster object, probably use bioclim.extent,
                  model=tree.test)
plot(tree.test.raster.prediction)
points(subs)

tree.test2.raster.prediction<-raster::predict(object=studyarea.bioclim, #raster object, probably use bioclim.extent,
                                             model=tree.test2)
plot(tree.test2.raster.prediction)
points(subs)

#create a simple ensemble model.
#principles based on this: http://www.overkillanalytics.net/more-is-always-better-the-power-of-simple-ensembles/
#but used raster stacks instead because that is not simple x/y like the example.

stack.test.trees<-stack(tree.test.raster.prediction,
                        tree.test2.raster.prediction)
ensemble.test.trees<-mean(stack.test.trees)

plot(ensemble.test.trees)
points(subs)

#Do the randomized geographic sampling to get overlapping support sets.
#possibly use raster::mosaic() function.

ensemble.test.trees.mosaic.version<-mosaic(tree.test.raster.prediction,
       tree.test2.raster.prediction,
       fun=mean)

plot(ensemble.test.trees.mosaic.version)

#develop function
#count number of non-NA layers (http://stackoverflow.com/questions/17304566/r-count-non-nas-in-a-raster-stack, last answer)
weights<-1/sum(!is.na(stack.test.trees))

#stack so have appropriate matching number of layers for raster::weighted.mean to work 
#(http://stackoverflow.com/questions/21041499/stacking-an-existing-rasterstack-multiple-times/21041582?noredirect=1#comment31634090_21041582)
weights.dim <- stack(replicate(dim(stack.test.trees)[3],
                 weights ))

#need to confirm that this will extend to cover all area.

ensemble.weighted<-raster::weighted.mean(x=stack.test.trees,
                      w=weights.dim,
                      na.rm=TRUE)

plot(ensemble.weighted)

#Overlapping support sets works to create mixture model.
