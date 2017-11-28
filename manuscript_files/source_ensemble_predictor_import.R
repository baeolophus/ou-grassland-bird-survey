#Bring in predictor data.

library(raster)

gispath <- paste0(getwd(), "/sources")

###AWS filesystem
wdrasters_list <- list.files(paste0(gispath),
                               pattern = "tif$",
                               full.names = FALSE)
wdrasters_files <- paste0(gispath,
                          "/",
                            wdrasters_list)

for(i in wdrasters_files) { assign(unlist(strsplit(i,
                                                     "[./]"))[5],
#splits filenames at / and and . to eliminate folder name and file type.
#The number on the previous line must be edited to match the depth of your filesystem so that the 
#rest of the file creates individual layers that match each raster and then stacks them.
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
