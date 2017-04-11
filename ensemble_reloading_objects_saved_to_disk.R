wdrasters_list <- list.files("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/HOLA_test",
                             pattern = "tif$",
                             full.names = FALSE)
wdrasters_files <- paste0("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/HOLA_test",
                          "/",
                          wdrasters_list)

for(i in wdrasters_files) { assign(unlist(strsplit(i,
                                                   "[/]"))[10], #splits filenames at / and and . to eliminate folder name and file type.
                                   raster(i)) } 

plot(HOLA_ensemble.weighted.mosaicsupport.large.list.tif)
plot(HOLA_ensemble.weighted.mosaicsupport.medium.list.tif)
plot(HOLA_ensemble.weighted.mosaicsupport.small.list.tif)
plot(HOLA_tree.statewide.raster.prediction.prob.tif)

resultshola <- readRDS("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/HOLA_ensembleresults")

microbenchmark.results <- resultshola[[1]]
