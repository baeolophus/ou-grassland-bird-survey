#Bring in predictor data.

library(raster)

#gispath <- paste0(getwd(), "/manuscript_files/sources")
#requires value of 13 in subsetting in line 16
gispath <- "/media/Data/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/higher_res_versions"
#requires value of 12 in subsetting in line 16

###AWS filesystem
wdrasters_list <- list.files(paste0(gispath),
                               pattern = "tif$",
                               full.names = FALSE)
wdrasters_files <- paste0(gispath,
                          "/",
                            wdrasters_list)

for(i in wdrasters_files) { assign(unlist(strsplit(i,
                                                     "[./]"))[12],
#Splits filenames at / and and . to eliminate folder name and file type.
#The number on the previous line must be edited to match the depth of your filesystem so that the 
#rest of the file creates individual layers that match each raster and then stacks them.
                                     raster(i)) } 


#######
#Get all raster layers in the working environment into one object.
predictors <- as.list(ls()[sapply(ls(), function(x) class(get(x))) == 'RasterLayer'])
predictors.list <- as.list(lapply(predictors, get))
#using get lets the middle code take the character names of raster layers and stack everything that is a raster layer
#using lapply on the list lets it do this to all the rasters.


#stack the rasters.
predictors_stack <- stack (predictors.list)

#Define study area extent based on predictors.
(studyarea.extent <- extent(predictors_stack))
