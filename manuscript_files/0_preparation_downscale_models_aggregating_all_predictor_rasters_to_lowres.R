##################################
#resample all rasters to get 4.3 x 4.3 km2 resolution 
setwd("/data/grassland_ensemble")
library(raster)
library(rgdal)

#See manuscript for raster and vector data source citations.

#create temporary raster files on large drive because they occupy 10-30 GB
rasterOptions()$tmpdir
rasterOptions(tmpdir=paste0(getwd(),
                            "/rastertemp"))

bioclim_list <- list.files(path = "E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/bio_12_ok_shape",
                          pattern = "tif$",
                          full.names = TRUE)
for(i in bioclim_list) { assign(unlist(strsplit(i,
                                                "[./]"))[10], #splits filenames at / and and . to eliminate folder name and file type.
                                raster(i)) } 

future_bioclim_list <- list.files(path = "E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/processed_future_bioclim",
                                  pattern = "tif$",
                                  full.names = TRUE)
for(i in future_bioclim_list) { assign(unlist(strsplit(i,
                                                "[./]"))[10], #splits filenames at / and and . to eliminate folder name and file type.
                                raster(i)) } 

nlcd_list <- list.files(path = "E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/nlcd_processing/nlcd_cropped_to_ok_census",
                        pattern = "tif$",
                        full.names = TRUE)
for(i in nlcd_list) { assign(unlist(strsplit(i,
                                                       "[./]"))[11], #splits filenames at / and and . to eliminate folder name and file type.
                                       raster(i)) } 


#load single files
conservation_easements_CalcAcres_raster_okmask <- raster ("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/conservation_easements_CalcAcres_raster_okmask.tif")
conservation_easements_presenceabsence_raster_okmask <- raster ("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/conservation_easements_presenceabsence_raster_okmask.tif")
effort_length_ok_census_mask <- raster ("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/effort_length_ok_census_mask.tif")
effort_time_ok_census_mask <- raster ("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/effort_time_ok_census_mask.tif")
resample_census_utm_30m <- raster ("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/resample_census_utm_30m.tif")
time_of_day <- raster ("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/time_of_day.tif")

#list all raster layers loaded in R environment
rasters_for_aggregating <- as.list(ls()[sapply(ls(), function(x) class(get(x))) == 'RasterLayer'])

#stack them
foraggregating_stack <- stack (lapply(rasters_for_aggregating, get))



#downscale by a factor of 144.  144 x 30 = 4320 m (close as possible to 4.3 km)
downscaled_rasters <- aggregate(foraggregating_stack,
                fact=144,
                fun = mean, 
                na.rm = TRUE,
                expand = TRUE)

writeRaster(downscaled_rasters,
            filename = paste0(names(downscaled_rasters), "_downscale"),
            format = "GTiff",
            bylayer = TRUE,
            overwrite = TRUE)

#Do categorical data separately
nlcd_ok_utm14_okmask_downscale <- resample(nlcd_ok_utm14_okmask,
                                           downscaled_rasters,
                                           method='ngb') #required for categories

writeRaster(nlcd_ok_utm14_okmask_downscale,
            filename = "nlcd_ok_utm14_okmask_downscale",
            format = "GTiff",
            bylayer = TRUE,
            overwrite = TRUE)
