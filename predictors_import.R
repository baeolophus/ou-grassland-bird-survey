#Bringing in predictors.


#help on working with rasters in R:
#http://neondataskills.org/R/Raster-Data-In-R/
#

library(sp)
library(rgdal)
library(raster)
library(ggplot2)

#NASS- raster, each year has its own
#https://www.nass.usda.gov/Research_and_Science/Cropland/SARS1a.php
#Includes switchgrass code, so even if switchgrass not found in OK could do analysis nationwide??
#30 meter resolution.
#switchgrass appears to be rare to non-existent in OK in both years.

NASS2013<-raster('bigfiles/cdl_30m_r_ok_2013_utm14.tif')
NASS2014<-raster('bigfiles/cdl_30m_r_ok_2014_utm14.tif')
NLCD2011<-raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/GIS_layers_original/land_use_land_cover_NLCD_ok_3276698_02/land_use_land_cover/nlcd_ok_utm14.tif")
plot(NASS2013)
plot(NASS2014)
plot(NLCD2011)

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

#Import census blocks for Oklahoma.
censusblocks<-readOGR(dsn="E:\\Downloads\\tabblock2010_40_pophu",
                      layer="tabblock2010_40_pophu")

str(censusblocks)
head(censusblocks)
#Get out population count field.

ggplot(data=censusblocks)+
  geom_polygon(mapping=aes(color=POP10))

#import census data geodatabase

# The input file geodatabase
fgdb<-"E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/GIS_layers_original/easements_EASEAREA_ok_3276698_01/easements/easement_a_ok.gdb"
# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)
# Read the feature class
easements<-readOGR(dsn=fgdb,
                   layer="easement_a_ok")

plot(easements)

#Create masking raster y out of study area extent
blank_oklahoma<-raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/GIS_layers_original/land_use_land_cover_NLCD_ok_3276698_02/land_use_land_cover/nlcd_ok_utm14.tif")
blank_oklahoma<-setValues(NLCD2011, 0)

#Transform polygon to same projection as blank raster.
grs80.14<-CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
easements <- spTransform(easements,
                         CRSobj=grs80.14)

#Create temporary directory in place with enough space to hold 5-10+ GB of temporary files.
rasterOptions()$tmpdir
rasterOptions(tmpdir="E:/Documents/R/temp")

#Rasterize the polygon in two ways.
#First, presence/absence of easements.
easements.raster.presence.absence <- rasterize(x = easements,
                            y = blank_oklahoma,
                            field = 1,
                            background = 0)

#Second, how many acres exist in that easement?
easements.raster.CalcAcres <- rasterize(x = easements,
                                               y = blank_oklahoma,
                                               field = "CalcAcres")

#Write these two files as GeoTiffs.  (Tried .grd default files but they gave many errors and do not load in QGIS.)
writeRaster(easements.raster.CalcAcres,
            filename = "conservation_easements_CalcAcres_raster.tif",
            format="GTiff")

writeRaster(easements.raster.presence.absence,
            filename = "conservation_easements_CalcAcres_raster.tif",
            format="GTiff")

#Re-import to see they will work.
easement.raster.test<-raster("conservation_easements_CalcAcres_raster.tif")

#Plot with vector to see that they match!  (Also did this in QGIS where I can zoom in more easily.)
plot(easement.raster.test)
plot(easements, add=TRUE)
