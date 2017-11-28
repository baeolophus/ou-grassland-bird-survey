#Bring in predictor data.

library(raster)
#Path on my computer
#gispath <- "E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed"
#Path on AWS
gispath <- paste0(getwd(), "/sources")

#Claire's computer
#import bioclim layers
#bio_utm_list <- list.files(path = paste0(gispath,
#                                         "/bio_12_ok_shape"),
#                           pattern = "tif$",
#                          full.names = FALSE)
#bio_utm_files <- paste0(gispath,
#                        "/bio_12_ok_shape/",
#                        bio_utm_list)

#for(i in bio_utm_files) { assign(unlist(strsplit(i,
#                                                 "[./]"))[10], #splits filenames at / and and . to eliminate folder name and file type.
#                                 raster(i)) } 


#commenting out non-folder file pathes as they will be collected later for AWS.
#resample_census_utm_30m <- raster(paste0(gispath,
#                                         "/resample_census_utm_30m.tif"))



#conservation_easements_CalcAcres_raster <- raster(paste0(gispath,
#                                                         "/conservation_easements_CalcAcres_raster_okmask.tif"))
#conservation_easements_presenceabsence_raster_okmask <- raster(paste0(gispath,
#                                                                      "/conservation_easements_presenceabsence_raster_okmask.tif"))

#nlcdrasters_list <- list.files(paste0(gispath, "/nlcd_processing/nlcd_cropped_to_ok_census"),
#                               pattern = "tif$",
#                               full.names = FALSE)
#nlcdrasters_files <- paste0(gispath,
#                            "/nlcd_processing/nlcd_cropped_to_ok_census/",
#                            nlcdrasters_list)

#for(i in nlcdrasters_files) { assign(unlist(strsplit(i,
 #                                                    "[./]"))[11], #splits filenames at / and and . to eliminate folder name and file type.
 #                                    raster(i)) } 

###AWS filesystem
wdrasters_list <- list.files(paste0(gispath),
                               pattern = "tif$",
                               full.names = FALSE)
wdrasters_files <- paste0(gispath,
                          "/",
                            wdrasters_list)

for(i in wdrasters_files) { assign(unlist(strsplit(i,
                                                     "[./]"))[5], #splits filenames at / and and . to eliminate folder name and file type.
                                     raster(i)) } 


#######
#general turn them into predictors stack.
predictors <- as.list(ls()[sapply(ls(), function(x) class(get(x))) == 'RasterLayer'])

predictors.list <- as.list(lapply(predictors, get))
#using get lets the middle code take the character names of raster layers and stack everything that is a raster layer
#Using lapply on the list lets it do this to all the rasters.


predictors_stack <- stack (predictors.list)
#Define study area extent based on predictors.
(studyarea.extent <- extent(predictors_stack))
