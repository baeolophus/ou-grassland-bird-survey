#Bringing in predictors.


#help on working with rasters in R:
#http://neondataskills.org/R/Raster-Data-In-R/
#

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(gdalUtils)


#RASTERIZING VECTORS
#Create temporary directory in place with enough space to hold 5-10+ GB of temporary files.
rasterOptions()$tmpdir
rasterOptions(tmpdir="E:/Documents/R/temp")

#Create UTM CRS.

grs80.14<-CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#Create masking raster y out of Oklahoma.
blank_oklahoma<-raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/GIS_layers_original/land_use_land_cover_NLCD_ok_3276698_02/land_use_land_cover/nlcd_ok_utm14.tif")

#Import census blocks for Oklahoma.
censusblocks<-readOGR(dsn="E:\\Documents\\college\\OU-postdoc\\research\\grassland_bird_surveys\\GIS_layers_original\\tabblock2010_40_pophu",
                      layer="tabblock2010_40_pophu")


#convert to UTM before calculating area.
censusblocks <- spTransform(censusblocks,
                         CRSobj=grs80.14)
#Get resolution of blank raster AFTER converting to UTM.
#Is needed later in gdal rasterize for dimensions of new raster.
r <- raster(censusblocks, resolution=res(blank_oklahoma))

#Calculate area for each polygon.
censusblocks$calcarea <- gArea(censusblocks,
                           byid = TRUE)

#Get out population count field and divide by area for density.
censusblocks$POP10KM <- (censusblocks$POP10/censusblocks$calcarea)*1000*1000

#Check calculations so you know what values should be in raster.
summary(censusblocks$POP10KM)

#Write UTM file to bring in for use in rgdal's gdal_rasterize.
writeOGR(obj=censusblocks,
         dsn=file.path(getwd()),
         layer="censusblocks_utm", 
         layer_options = "RESIZE=YES",
         driver="ESRI Shapefile")
#gives errors about area fields (field width problem),
#but I don't want that one, so I'm not worrying.  Other values appear correctly in QGIS.


gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(require(raster) && require(rgdal) && valid_install)
{
test.raster<-gdal_rasterize(src_datasource = paste(file.path(getwd()), 
                                                   "/censusblocks_utm.shp",
                                                   sep=""),
                             dst_filename = paste(file.path(getwd()),
                                                  "/r_censusblock_raster.tiff",
                                                  sep=""),
                             a = "POP10KM",
                            # tr = c(30, 30),
                            ts = c(r@ncols, r@nrows),
                            l = "censusblocks_utm",
                            verbose=TRUE,
                            output_Raster=TRUE,
                            q=FALSE
                             )
}
#originally was getting error.
#I tried these things and they didn't fix.
#http://gis.stackexchange.com/questions/217320/rasterizing-lines-with-gdal-rasterize-in-r-return-no-result-and-status-1
#I think the problem was I had the wrong name of the field.

censusraster<-raster("r_censusblock_raster.tiff")
plot(censusraster)

#gdal_rasterize -a dnsty_k -tr 30.0 30.0 -l censusblocks
#E:/Dropbox/work/ougrassland/ou-grassland-bird-survey/censusblocks.shp
#E:/Dropbox/work/ougrassland/ou-grassland-bird-survey/cnss.tif

#CONSERVATION EASEMENTS TO RASTERS
##The other way of making rasters for smaller, simpler files.
#import easement data geodatabase

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

#Transform polygon to same projection as blank raster.
easements <- spTransform(easements,
                         CRSobj=grs80.14)

#Confirm temporary directory in place with enough space to hold 5-10+ GB of temporary files.
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

#NLCD 2011
#Getting neighborhood values.
NLCD2011<-raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/GIS_layers_original/land_use_land_cover_NLCD_ok_3276698_02/land_use_land_cover/nlcd_ok_utm14.tif")
plot(NLCD2011) #check the raster is there

NLCD2011_undev_openspace<-raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ou-grassland-bird-survey/nlcd_processing/undevopenspace.tif")
plot(NLCD2011_undev_openspace) #check the raster is there


openwater <- raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ou-grassland-bird-survey/nlcd_processing/openwater11.tif")
plot(openwater)

dev_openspace21 <- raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ou-grassland-bird-survey/nlcd_processing/dev_openspace21.tif")
plot(dev_openspace21)

dev_low22 <- raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ou-grassland-bird-survey/nlcd_processing/dev_low22.tif")
plot(dev_low22)

dev_med23 <- raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ou-grassland-bird-survey/nlcd_processing/dev_med23.tif")
plot(dev_med23)

dev_high24 <- raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ou-grassland-bird-survey/nlcd_processing/dev_high24.tif")
plot(dev_high24)

scrub <- raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ou-grassland-bird-survey/nlcd_processing/scrub52.tif")
plot(scrub)

croplands <- raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ou-grassland-bird-survey/nlcd_processing/croplands82.tif")
plot(croplands)




##NASS- raster, each year has its own
#https://www.nass.usda.gov/Research_and_Science/Cropland/SARS1a.php
#Includes switchgrass code, so even if switchgrass not found in OK could do analysis nationwide??
#30 meter resolution.
#switchgrass appears to be rare to non-existent in OK in both years.

NASS2013<-raster('bigfiles/cdl_30m_r_ok_2013_utm14.tif')
NASS2014<-raster('bigfiles/cdl_30m_r_ok_2014_utm14.tif')
plot(NASS2013)
plot(NASS2014)

extract2013<-getValues(NASS2013)
unique(values(NASS2013))


#Test merging all raster layers into a brick for prediction.

#test merge with bioclim.  5 minute resolution.
bio18<-raster('bio_5m_bil/bio18.bil')
bio19<-raster('bio_5m_bil/bio19.bil')

#add extent to NASS so can be merged with bioclim.
#download smallest resolution bioclim?

predictors.brick<-brick(NASS2013,
                        NASS2014,
                        bio18,
                        bio19)

