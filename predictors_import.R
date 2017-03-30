#Bringing in predictors.


#help on working with rasters in R:
#http://neondataskills.org/R/Raster-Data-In-R/
#

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(gdalUtils)
library(maptools)


#RASTERIZING VECTORS
#Create temporary directory in place with enough space to hold 5-10+ GB of temporary files.
rasterOptions()$tmpdir
rasterOptions(tmpdir="E:/Documents/R/temp")

#Create UTM CRS.

grs80.14<-CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#Create masking raster y out of Oklahoma.
blank_oklahoma<-raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/GIS_layers_original/land_use_land_cover_NLCD_ok_3276698_02/land_use_land_cover/nlcd_ok_utm14.tif")

##################################
#Import census blocks for Oklahoma.
censusblocks<-readOGR(dsn="E:\\Documents\\college\\OU-postdoc\\research\\grassland_bird_surveys\\GIS_layers_original\\tabblock2010_40_pophu",
                      layer="tabblock2010_40_pophu")

proj4string(censusblocks)
#convert to UTM before calculating area.
censusblocks <- spTransform(censusblocks,
                         CRSobj=grs80.14)
proj4string(censusblocks)
####
#Also create a single dissolved polygon of oklahoma.
ok_state_vector_smallest <- unionSpatialPolygons(censusblocks,
                                                 censusblocks$STATEFP10)

ok_state_vector_smallest_pdf <- as(ok_state_vector_smallest,
                                   "SpatialPolygonsDataFrame")

writeOGR(obj=ok_state_vector_smallest_pdf,
         dsn=file.path(getwd()),
         layer="ok_state_vector_smallest_pdf", 
         layer_options = "RESIZE=YES",
         driver="ESRI Shapefile")


###
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
      #   layer_options = "RESIZE=YES", #This line appears unnecessary in future, but I did use it.
         driver="ESRI Shapefile")
#gives errors about area fields (field width problem),
#but I don't want that one, so I'm not worrying.  Other values appear correctly in QGIS.
censusblocks_utm<-readOGR(dsn=getwd(),
                          layer="censusblocks_utm")
proj4string(censusblocks_utm)
#somehow it looks the towgs setting in the writing of it.

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
                             tr = c(30, 30),
                           # ts = c(r@ncols, r@nrows),
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
extent(censusraster)

#no matter what it won't extend to correct origin and resolution, so here is code to resample:
resample.census <- resample (censusraster,
                           y = nlcd_ok_utm14)
writeRaster(resample.census,
            filename = "resample_census_utm_30m.tif",
            format="GTiff",
            overwrite = TRUE)

resample.test<- raster("resample_census_utm_30m.tif")
plot(resample.test)

##################################
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
                                               field = "CalcAcres",
                                        background = 0) #should have added background = 0, then need to clip-mask in qgis (gdalwarp)

#Write these two files as GeoTiffs.  (Tried .grd default files but they gave many errors and do not load in QGIS.)
writeRaster(easements.raster.CalcAcres,
            filename = "conservation_easements_CalcAcres_raster.tif",
            format="GTiff",
            overwrite = TRUE)

writeRaster(easements.raster.presence.absence,
            filename = "conservation_easements_presenceabsence_raster.tif",
            format="GTiff")

#Re-import to see they will work.
easement.raster.test<-raster("conservation_easements_CalcAcres_raster.tif")

#Plot with vector to see that they match!  (Also did this in QGIS where I can zoom in more easily.)
plot(easement.raster.test)
plot(easements, add=TRUE)

##################################
#bioclim to UTM
#import bioclim layers
bio_12_list <- list.files(path = "E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/GIS_layers_original/bio_12",
                          pattern = "bil$",
                          full.names = TRUE)

for(i in bio_12_list) { assign(unlist(strsplit(i,
                                               "[./]"))[9], #splits filenames at / and and . to eliminate folder name and file type.
                               raster(i)) } 

bio <- as.list(ls()[sapply(ls(), function(x) class(get(x))) == 'RasterLayer'])
bio_stack <- stack (lapply(bio, get))
crs(bio_stack) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  #http://www.worldclim.org/format

studyarea.extent.latlong<-extent(-103,-94,
                         33,38) # define the extent for latlong to get a smaller file
studyarea.bioclim<-crop(bio_stack,
                        studyarea.extent.latlong)

#show they are in different CRS with different extents
nlcd_ok_utm14 <- raster("nlcd_processing/nlcd_ok_utm14.tif")
extent(nlcd_ok_utm14)
extent(studyarea.bioclim)

#project to the smaller extent and the crs of popdensity_census_raster (Which was made with NLCD)
utm.bioclim <- projectRaster(from = studyarea.bioclim, 
                             to = nlcd_ok_utm14)

#write the new file to smaller files that I can import later  without re-processing
writeRaster(utm.bioclim,
            filename = names(utm.bioclim),
            format = "GTiff",
            bylayer = TRUE,
            overwrite = TRUE)
##################################
#Create mean time and length rasters to match other ok-census cropped files.
#Also get sample sizes here.
get.mean.efforts.for.rasters <- read.csv(file = "oklahomadatasetforsdm_naomit_utm.csv")

narrowed <- get.mean.efforts.for.rasters %>% group_by(SAMPLING_EVENT_ID) %>%
  distinct(SAMPLING_EVENT_ID, .keep_all = TRUE)
#length of narrowed is sample size for training dataset.

numbers <- ungroup(narrowed) %>% summarize (meantime = mean(effort_time, na.rm = TRUE),
                                 meanlength = mean(effort_length, na.rm = TRUE))

grs80.14<-CRS("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#Create masking raster y out of Oklahoma.
blank_oklahoma_census<-raster("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/GIS_layers_original/land_use_land_cover_NLCD_ok_3276698_02/land_use_land_cover/nlcd_ok_utm14.tif")


writeRaster(effort.length.ok.census,
            filename = "effort_length_ok_census_mask.tif",
            format="GTiff",
            overwrite = TRUE)

writeRaster(effort.time.ok.census,
            filename = "effort_time_ok_census_mask.tif",
            format="GTiff",
            overwrite = TRUE)


