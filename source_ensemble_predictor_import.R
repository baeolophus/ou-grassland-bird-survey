#Bring in predictor data.

library(raster)
#Path on my computer
gispath <- "E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed"
#Path on AWS
gispath <- "E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed"

#import bioclim layers
bio_utm_list <- list.files(path = paste0(gispath,
                                         "/bio_12_ok_shape"),
                           pattern = "tif$",
                           full.names = FALSE)
bio_utm_files <- paste0(gispath,
                        "/bio_12_ok_shape/",
                        bio_utm_list)

for(i in bio_utm_files) { assign(unlist(strsplit(i,
                                                 "[./]"))[10], #splits filenames at / and and . to eliminate folder name and file type.
                                 raster(i)) } 


resample_census_utm_30m <- raster(paste0(gispath,
                                         "/resample_census_utm_30m.tif"))



conservation_easements_CalcAcres_raster <- raster(paste0(gispath,
                                                         "/conservation_easements_CalcAcres_raster_okmask.tif"))
conservation_easements_presenceabsence_raster_okmask <- raster(paste0(gispath,
                                                                      "/conservation_easements_presenceabsence_raster_okmask.tif"))

nlcdrasters_list <- list.files(paste0(gispath, "/nlcd_processing"),
                               pattern = "tif$",
                               full.names = FALSE)
nlcdrasters_files <- paste0(gispath,
                            "/nlcd_processing/",
                            nlcdrasters_list)

for(i in nlcdrasters_files) { assign(unlist(strsplit(i,
                                                     "[./]"))[10], #splits filenames at / and and . to eliminate folder name and file type.
                                     raster(i)) } 

predictors <- as.list(ls()[sapply(ls(), function(x) class(get(x))) == 'RasterLayer'])

predictors.list <- as.list(lapply(predictors, get))
#using get lets the middle code take the character names of raster layers and stack everything that is a raster layer
#Using lapply on the list lets it do this to all the rasters.


predictors_stack <- stack (predictors.list)
#Define study area extent based on predictors.
(studyarea.extent <- extent(predictors_stack))
