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


#Need to create one raster for each landcover type I want to know about in the neighborhood.
#code adapted from: https://stat.ethz.ch/pipermail/r-sig-geo/2012-March/014598.html
#"open spaces"
NLCD2011_open_space <- NLCD2011
#Which cell values are open space
#read from metadata available in folder
NLCD_open_space <- c(71, 81, 82, 95)
#71 grasslands
#81 hay/pasture
#82 crops
#95 herbaceous wetlands

NLCD2011_open_space[NLCD2011_open_space %in% NLCD_open_space] <- 1
#Change all other values to 0.
NLCD2011_open_space[!NLCD2011_open_space %in% c(1)] <- 0
plot(NLCD2011_open_space)
writeRaster(NLCD2011_open_space,
            filename = "NLCD2011_open_space.tif",
            format="GTiff")
#"croplands"
#"brush"
#"forest"

#testing that I understand how the cells/focal sum/mean works.
par(mfrow=c(1,2))
r <- raster(ncols=5, nrows=6, xmn=0)
r[] <- rep(c(0,1), ncell(r)/2)

r.p<-focal(x = r,
           w = matrix(1/25, 
                      nrow = 5, 
                      ncol = 5)
)
plot(r)
plot(r.p)

matrix(values(r.p), nrow=6, ncol=5, byrow = TRUE)
matrix(values(r), nrow=6, ncol=5, byrow = TRUE)

#Get a neighborhood for 5x5 (150 x 150 m).
NLCD2011_open_space_sums_5cells <- focal(x = NLCD2011_open_space,
                                         w = matrix(1/25, 
                                                    nrow = 5, 
                                                    ncol = 5)
                                         )

writeRaster(NLCD2011_open_space_sums_5cells,
            filename = "NLCD2011_open_space_sums_5cells.tif",
            format="GTiff")

par(mfrow=c(1,1))
plot(NLCD2011_open_space_sums_5cells)

#Writing a function to do this.
nlcd.lumped.raster.neighborhoods <- function (datasource,
                                              prefix,
                                              lumped_name,
                                              what_cell_values,
                                              neighborhood_size_in_pixels,
                                              data_writing_format
){
  #Confirm temporary directory in place with enough space to hold 5-10+ GB of temporary files.
  rasterOptions()$tmpdir
  rasterOptions(tmpdir="E:/Documents/R/temp")
  
  #copy datasource to new layer
  data_copy <- datasource
  
  #change all cell values of interest to 1
  data_copy[data_copy %in% what_cell_values] <- 1
  #Change all other values to 0
  data_copy[!data_copy %in% c(1)] <- 0

  #write this as a raster file.
  writeRaster(data_copy,
              filename = paste(prefix,
                               lumped_name,
                               ".tif",
                               sep= "_"),
              format = data_writing_format,
              overwrite = TRUE)
  
  #put the variable out for use in R
  assign (paste(prefix,
                   lumped_name,
                   sep= "_"),
             data_copy,
          pos = ".GlobalEnv")
 
  #Get a neighborhood for n x n pixels.
  neighborhood <- focal(x = data_copy,
                                           w = matrix(1/(neighborhood_size_in_pixels^2), 
                                                      nrow = neighborhood_size_in_pixels, 
                                                      ncol = neighborhood_size_in_pixels)
  )
  
  #write as a raster file.
  writeRaster(neighborhood,
              filename = paste(prefix,
                               lumped_name,
                               neighborhood_size_in_pixels,
                               "cells.tif",
                               sep= "_"),
              format = data_writing_format,
              overwrite = TRUE)
  
  #put the variable out for use in R
  assign (paste(prefix,
                lumped_name,
                neighborhood_size_in_pixels,
                "cells",
                sep= "_"),
          neighborhood,
          pos = ".GlobalEnv")
  
  par(mfrow=c(1,2))
  plot(data_copy)
  plot(neighborhood)
  
}


#undeveloped open space
#11 open water
#31 barren land
#71 grasslands
#81 hay/pasture
#82 crops
#95 herbaceous wetlands

nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "undev_openspace",
                                  what_cell_values = c(11,
                                                       31,
                                                       71,
                                                       81,
                                                       82,
                                                       95), #grasslands only (71)
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "undev_openspace",
                                  what_cell_values = c(11,
                                                       31,
                                                       71,
                                                       81,
                                                       82,
                                                       95), 
                                  neighborhood_size_in_pixels = 15, #15 x 15 = 450 x 450 m
                                  data_writing_format = "GTiff")

#11 open water
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "openwater11",
                                  what_cell_values = c(11),
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "openwater11",
                                  what_cell_values = c(11),
                                  neighborhood_size_in_pixels = 15,
                                  data_writing_format = "GTiff")

#21 developed open space
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "developed_openspace21",
                                  what_cell_values = c(21),
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")

nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "developed_openspace21",
                                  what_cell_values = c(21),
                                  neighborhood_size_in_pixels = 15,
                                  data_writing_format = "GTiff")

#22 low development
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "dev_low22",
                                  what_cell_values = c(22),
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")

nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "dev_low22",
                                  what_cell_values = c(22),
                                  neighborhood_size_in_pixels = 15,
                                  data_writing_format = "GTiff")

#23 med development
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "dev_med23",
                                  what_cell_values = c(23),
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")

nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "dev_med23",
                                  what_cell_values = c(23),
                                  neighborhood_size_in_pixels = 15,
                                  data_writing_format = "GTiff")


#24 high development
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "dev_high24",
                                  what_cell_values = c(24),
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")

nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "dev_high24",
                                  what_cell_values = c(24),
                                  neighborhood_size_in_pixels = 15,
                                  data_writing_format = "GTiff")


#31 barren land
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "barren31",
                                  what_cell_values = c(31), 
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "barren31",
                                  what_cell_values = c(31), 
                                  neighborhood_size_in_pixels = 15, #15 x 15 = 450 x 450 m
                                  data_writing_format = "GTiff")



#forest 41, 42, 43
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "forest41to43",
                                  what_cell_values = c(41,
                                                       42,
                                                       43), 
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "forest41to43",
                                  what_cell_values = c(41,
                                                       42,
                                                       43), 
                                  neighborhood_size_in_pixels = 15, #15 x 15 = 450 x 450 m
                                  data_writing_format = "GTiff")


#shrub/scrub 52
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "shrub52",
                                  what_cell_values = c(52), 
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "shrub52",
                                  what_cell_values = c(52), 
                                  neighborhood_size_in_pixels = 15, #15 x 15 = 450 x 450 m
                                  data_writing_format = "GTiff")


#71 grasslands
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "grasslands71",
                                  what_cell_values = c(71), #grasslands only (71)
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "grasslands71",
                                  what_cell_values = c(71), #grasslands only (71)
                                  neighborhood_size_in_pixels = 15, #15 x 15 = 450 x 450 m
                                  data_writing_format = "GTiff")

#81 pasture/hay
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "pasturehay81",
                                  what_cell_values = c(81),
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "pasturehay81",
                                  what_cell_values = c(81),
                                  neighborhood_size_in_pixels = 15, #15 x 15 = 450 x 450 m
                                  data_writing_format = "GTiff")



#82 croplands
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "croplands82",
                                  what_cell_values = c(82),
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "croplands82",
                                  what_cell_values = c(82),
                                  neighborhood_size_in_pixels = 15, #15 x 15 = 450 x 450 m
                                  data_writing_format = "GTiff")



#90 woody wetlands
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "woodywetlands90",
                                  what_cell_values = c(90),
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "woodywetlands90",
                                  what_cell_values = c(90),
                                  neighborhood_size_in_pixels = 15, #15 x 15 = 450 x 450 m
                                  data_writing_format = "GTiff")




#95 emergent herbaceous wetlands

nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "herbwetlands95",
                                  what_cell_values = c(95),
                                  neighborhood_size_in_pixels = 5,
                                  data_writing_format = "GTiff")
nlcd.lumped.raster.neighborhoods (datasource = NLCD2011,
                                  prefix = "NLCD2011",
                                  lumped_name = "herwetlands95",
                                  what_cell_values = c(95),
                                  neighborhood_size_in_pixels = 15, #15 x 15 = 450 x 450 m
                                  data_writing_format = "GTiff")




nlcd.single.landcover <- function (datasource,
                                              prefix,
                                              lumped_name,
                                              what_cell_values,
                                   data_writing_format
){
  #Confirm temporary directory in place with enough space to hold 5-10+ GB of temporary files.
  rasterOptions()$tmpdir
  rasterOptions(tmpdir="E:/Documents/R/temp")
  
  #copy datasource to new layer
  data_copy <- datasource
  
  #change all cell values of interest to 1
  data_copy[data_copy %in% what_cell_values] <- 1
  #Change all other values to 0
  data_copy[!data_copy %in% c(1)] <- 0
  
  #write this as a raster file.
  writeRaster(data_copy,
              filename = paste0(prefix,
                               lumped_name,
                               ".tif"),
              format = data_writing_format,
              overwrite = TRUE)
  
  #put the variable out for use in R
  assign (paste(prefix,
                lumped_name,
                sep= "_"),
          data_copy,
          pos = ".GlobalEnv")
}


nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "undev_openspace",
                      what_cell_values = c(11,
                                           31,
                                           71,
                                           81,
                                           82,
                                           95),
                      data_writing_format = "GTiff")
nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "openwater11",
                      what_cell_values = c(11),
                      data_writing_format = "GTiff")
nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "developed_openspace21",
                      what_cell_values = c(21),
                      data_writing_format = "GTiff")
nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "dev_low22",
                      what_cell_values = c(22),
                      data_writing_format = "GTiff")
nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "dev_med23",
                      what_cell_values = c(23),
                      data_writing_format = "GTiff")
nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "dev_high24",
                      what_cell_values = c(24),
                      data_writing_format = "GTiff")
nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "barren31",
                      what_cell_values = c(31),
                      data_writing_format = "GTiff")

nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "forest41to43",
                      what_cell_values = c(41,
                                           42,
                                           43),
                      data_writing_format = "GTiff")

##
nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "shrub52",
                      what_cell_values = c(52),
                      data_writing_format = "GTiff")
nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "grasslands71",
                      what_cell_values = c(71),
                      data_writing_format = "GTiff")
nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "pasturehay81",
                      what_cell_values = c(81),
                      data_writing_format = "GTiff")
nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "croplands82",
                      what_cell_values = c(82),
                      data_writing_format = "GTiff")

nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "woodywetlands90",
                      what_cell_values = c(90),
                      data_writing_format = "GTiff")

nlcd.single.landcover(datasource = NLCD2011,
                      prefix = "NLCD2011_",
                      lumped_name = "herbwetlands95",
                      what_cell_values = c(95),
                      data_writing_format = "GTiff")

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

