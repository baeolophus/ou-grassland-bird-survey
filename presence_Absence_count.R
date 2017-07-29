#sample sizes of presence and absence by species


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


presence.absence.count <- function (SPECIES) {
  #loading Rdata gets the rest!
  load(paste0("~/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current/",
              SPECIES,
              "/",
              SPECIES,
              "_rdata.RData"))
  

  new <- latlong.predictors.SPECIES[latlong.predictors.SPECIES$presence == 1, "presence"]
  length(new)
  
}

listofeval <- lapply (specieslist,
                      FUN = presence.absence.count)
