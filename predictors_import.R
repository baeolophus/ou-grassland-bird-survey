#Bringing in predictors.


#help on working with rasters in R:
#http://neondataskills.org/R/Raster-Data-In-R/
#

library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(rgeos)

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

#RASTERIZING VECTORS
#Create temporary directory in place with enough space to hold 5-10+ GB of temporary files.
rasterOptions()$tmpdir
rasterOptions(tmpdir="E:/Documents/R/temp")

#Create UTM CRS.

grs80.14<-CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#Create masking raster y out of Oklahoma.
blank_oklahoma<-raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/GIS_layers_original/land_use_land_cover_NLCD_ok_3276698_02/land_use_land_cover/nlcd_ok_utm14.tif")

r <- raster(censusblocks, resolution=res(blank_oklahoma))


#Import census blocks for Oklahoma.
censusblocks<-readOGR(dsn="E:\\Downloads\\tabblock2010_40_pophu",
                      layer="tabblock2010_40_pophu")

#convert to UTM before calculating area.
censusblocks <- spTransform(censusblocks,
                         CRSobj=grs80.14)

#Calculate area for each polygon.
censusblocks$calcarea <- gArea(censusblocks,
                           byid = TRUE)

#Get out population count field and divide by area for density.
censusblocks$density_kmsquare <- (censusblocks$POP10/censusblocks$calcarea)*1000*1000

#Check calculations so you know what values should be in raster.
summary(censusblocks$density_kmsquare)

#Write UTM file to bring in for use in rgdal's gdal_rasterize.
writeOGR(obj=censusblocks,
         dsn=file.path(getwd()),
         layer="censusblocks", 
         driver="ESRI Shapefile")
#gives errors about area fields (field width problem),
#but I don't want that one, so I'm not worrying.  Other values appear correctly in QGIS.



library(gdalUtils)

gdal_setInstallation()
valid_install <- !is.null(getOption("gdalUtils_gdalPath"))
if(require(raster) && require(rgdal) && valid_install)
{
test.raster<-gdal_rasterize(src_datasource = "E:/Dropbox/work/ougrassland/ou-grassland-bird-survey/censusblocks.shp",
                             dst_filename = "E:/Dropbox/work/ougrassland/ou-grassland-bird-survey/r_censusblock_raster.tiff",
                             a = "dnsty_k",
                            # tr = c(30, 30),
                            ts = c(r@ncols, r@nrows),
                            l = "censusblocks",
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

summary(censusraster)
#gdal_rasterize -a dnsty_k -tr 30.0 30.0 -l censusblocks
#E:/Dropbox/work/ougrassland/ou-grassland-bird-survey/censusblocks.shp
#E:/Dropbox/work/ougrassland/ou-grassland-bird-survey/cnss.tif
'"C:\Program Files\QGIS 2.16.1\bin\gdal_rasterize.exe" 
-a "density_kmsquare" -l "censusblocks" -tr 30 30
"E:/Dropbox/work/ougrassland/ou-grassland-bird-survey/censusblocks.shp" "E:/Dropbox/work/ougrassland/ou-grassland-bird-survey/r_censusblock_raster.tif"' 
census.block.density <- rasterize(x = censusblocks,
                                        y = r,
                                        field = "density_kmsquare")

#Write these two files as GeoTiffs.  (Tried .grd default files but they gave many errors and do not load in QGIS.)
writeRaster(census.block.density,
            filename = "censusblock_density_raster.tif",
            format="GTiff")
#Re-import to see they will work.
censusblock.density.raster.test<-raster("censusblock_density_raster.tif")



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
