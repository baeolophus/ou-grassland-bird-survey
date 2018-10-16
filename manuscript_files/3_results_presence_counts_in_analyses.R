
#One of the working directories
setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current") #original results
setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Downscale_current") #coarser results

#list of species
specieslist <- c("NOBO",
                 "UPSA",
                 "HOLA",
                 "CASP",
                 "FISP",
                 "LASP",
                 "GRSP",
                 "DICK",
                 "EAME",
                 "WEME",
                 "BHCO")


#evals for all species

count.presence.function <- function(SPECIES) {
  evalresults <- readRDS(file.path(
    SPECIES,
    paste0(
      SPECIES,
      "_statewide_products_tree_and_varimp"))
  )
  a<-evalresults[[1]]
  
  presence <- data.frame(a$y)
  dplyr::group_by(presence, a.y) %>% summarize(n())
  
}

listofcounts <- lapply (specieslist,
                      FUN = count.presence.function)
