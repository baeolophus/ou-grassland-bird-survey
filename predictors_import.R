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
studyarea.extent.raster <- 
easements.raster <- rasterize(x = easements,
                            y = studyarea.extent,
                            field = 1,
                            )

#Convert a spatial polygon to a raster (https://www.r-bloggers.com/converting-shapefiles-to-rasters-in-r/)
shp2raster <- function(shp,
                       mask.raster,
                       label,
                       value,
                       transform = FALSE,
                       proj.from = NA,
                       proj.to = NA, 
                       map = TRUE) {
  
  require(raster, rgdal)
  
  # use transform==TRUE if the polygon is not in the same coordinate system as
  # the output raster, setting proj.from & proj.to to the appropriate
  # projections
  if (transform == TRUE) {
    proj4string(shp) <- proj.from
    shp <- spTransform(shp, proj.to)
  }
  
  # convert the shapefile to a raster based on a standardised background
  # raster
  r <- rasterize(shp, mask.raster)
  # set the cells associated with the shapfile to the specified value
  r[!is.na(r)] <- value
  # merge the new raster with the mask raster and export to the working
  # directory as a tif file
  r <- mask(merge(r, mask.raster), mask.raster, filename = label, format = "GTiff",
            overwrite = T)
  
  # plot map of new raster
  if (map == TRUE) {
    plot(r, main = label, axes = F, box = F)
  }
  
  names(r) <- label
  return(r)
}