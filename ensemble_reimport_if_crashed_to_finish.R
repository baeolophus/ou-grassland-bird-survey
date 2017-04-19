###AWS filesystem
bringback_list <- list.files(getwd(),
                             pattern = "^EAME_treetestrasterpredictionextendedpolys\\.small\\.df.*\\.tif$",
                             full.names = FALSE)
bringback_files <- paste0(getwd(),
                          "/",
                          bringback_list)

for(i in bringback_files) { assign(unlist(strsplit(i,
                                                   "[/]"))[4], #splits filenames at / and and . to eliminate folder name and file type.
                                   raster(i)) } 
support.small.list <- as.list(ls()[sapply(ls(), function(x) class(get(x))) == 'RasterLayer'])

support.small.list <- as.list(lapply(support.small.list, get))
#using get lets the middle code take the character names of raster layers and stack everything that is a raster layer
#Using lapply on the list lets it do this to all the rasters.

