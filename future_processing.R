##################################
#process the future bioclim layers the same way as bioclim (though use the ok_mask_resample to make sure I don't need to crop again later)
#import bioclim layers
future_list <- list.files(path = file.path(getwd(),
                                           "future6"),
                          pattern = "tif$",
                          full.names = TRUE)

for(i in future_list) { assign(unlist(strsplit(i,
                                               "[./]"))[5], #splits filenames at / and and . to eliminate folder name and file type.
                               raster(i)) } 

future <- as.list(ls()[sapply(ls(), function(x) class(get(x))) == 'RasterLayer'])
future_stack <- stack (lapply(future, get))
crs(future_stack) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  #http://www.worldclim.org/format

studyarea.extent.latlong<-extent(-103,-94,
                                 33,38) # define the extent for latlong to get a smaller file
studyarea.future_stack<-crop(future_stack,
                             studyarea.extent.latlong)

#show they are in different CRS with different extents
okcensus <- raster("sources/census_utm_30m.tif")
extent(okcensus)
extent(studyarea.future_stack)

#project to the smaller extent and the crs of popdensity_census_raster (Which was made with NLCD)
utm.future <- projectRaster(from = studyarea.future_stack, 
                            to = okcensus,
                            filename = names(studyarea.future_stack),
                            format = "GTiff",
                            bylayer = TRUE,
                            overwrite = TRUE)

#Now upload and clip/mask them to ok state polygon.
state<-readOGR(dsn=getwd(),
               layer="ok_state_vector_smallest_pdf_3158")
state<-spTransform(x = state,
                   CRS(as.character("+proj=utm +zone=14 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
)

future_OK <- mask(utm.future,
                  state,
                  filename = names(future_OK),
                  format = "GTiff",
                  bylayer = TRUE,
                  overwrite = TRUE)

#write the new file to smaller files that I can import later  without re-processing
writeRaster(future_OK)



future_crop_list <- list.files(path = "E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/gis_layers_processed/he45bi50",
                               pattern = "tif$",
                               full.names = TRUE)

for(i in future_crop_list) { assign(unlist(strsplit(i,
                                                    "[./]"))[10], #splits filenames at / and and . to eliminate folder name and file type.
                                    raster(i)) } 

future_crop <- as.list(ls()[sapply(ls(), function(x) class(get(x))) == 'RasterLayer'])
future_crop_stack <- stack (lapply(future_crop, get))


#write the new file to smaller files that I can import later  without re-processing
writeRaster(future_OK,
            filename = names(future_OK),
            format = "GTiff",
            bylayer = TRUE,
            overwrite = TRUE)

##################################