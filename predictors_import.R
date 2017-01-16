#Bringing in predictors.

#NASS- raster, each year has its own
#https://www.nass.usda.gov/Research_and_Science/Cropland/SARS1a.php
#Includes switchgrass code, so even if switchgrass not found in OK could do analysis nationwide??
#30 meter resolution.
#switchgrass appears to be rare to non-existent in OK in both years.

#help on working with rasters in R:
#http://neondataskills.org/R/Raster-Data-In-R/
#

library(raster)
NASS2013<-raster('bigfiles/cdl_30m_r_ok_2013_utm14.tif')
NASS2014<-raster('bigfiles/cdl_30m_r_ok_2014_utm14.tif')
plot(NASS2013)
plot(NASS2014)

extract2013<-getValues(NASS2013)
unique(values(NASS2013))

#test merge with bioclim.  5 minute resolution.
bio18<-raster('bio_5m_bil/bio18.bil')
bio19<-raster('bio_5m_bil/bio19.bil')

#add extent to NASS so can be merged with bioclim.
#download smallest resolution bioclim?

predictors.brick<-brick(NASS2013,
                    NASS2014,
                    bio18,
                    bio19)

library(sp)
library(rgdal)
library(raster)
library(ggplot2)
#Import census blocks for Oklahoma.
censusblocks<-readOGR(dsn="E:\\Downloads\\tabblock2010_40_pophu",
                      layer="tabblock2010_40_pophu")

str(censusblocks)
head(censusblocks)
#Get out population count field.

ggplot(data=censusblocks)+
  geom_polygon(mapping=aes(color=POP10))

