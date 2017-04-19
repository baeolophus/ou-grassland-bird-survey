library(raster)
wdrasters_list <- list.files("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/EAME",
                             pattern = "tif$",
                             full.names = FALSE)
wdrasters_files <- paste0("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/EAME",
                          "/",
                          wdrasters_list)

for(i in wdrasters_files) { assign(unlist(strsplit(i,
                                                   "[/]"))[10], #splits filenames at / and and . to eliminate folder name and file type.
                                   raster(i)) } 

plot(EAME_ensemble.weighted.mosaicsupport.large.list.tif,
     main = "large")
plot(EAME_ensemble.weighted.mosaicsupport.medium.list.tif,
     main = "medium")
plot(EAME_ensemble.weighted.mosaicsupport.small.list.tif,
     main = "small")
plot(EAME_tree.statewide.raster.prediction.prob.tif,
     main = "statewide")

resultsEAME <- readRDS("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/EAME_ensembleresults")

microbenchmark.results <- resultsEAME[[1]]

list.of.rasters <- readRDS("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/EAME/EAME_small_support_list")
r <-test[[1]][1]
st <-stack(r)

weights<-lapply(list.of.rasters,
                "[",
                2)
weights<-as.vector(unlist(weights))
