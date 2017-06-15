#microbenchmark comparisons

library(dplyr)

#now the microbenchmarks

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

microbenchmarkcsv <- function (SPECIES) {
microbenchmarks <- read.csv(paste0(SPECIES,
                                   "/",
                                   SPECIES,
                                   "_products_microbenchmarks.csv"))
return(microbenchmarks)
}

listofmb <- lapply (specieslist,
                    FUN = microbenchmarkcsv)
mb.df <- do.call(rbind, lapply(listofmb, data.frame, stringsAsFactors=FALSE))

mb.df.sep <- separate(mb.df,
                          into = c("scale",
                                   "runtype"),
                          model,
                      sep = -2)

mb.summed <- group_by(mb.df.sep,
                      scale,
                      Species) %>%
  summarize("runtime" = sum(time)*1000000000) #convert from default nanoseconds to seconds



statewide.values <-filter (mb.summed,
                            scale == "statewide") %>%
  ungroup(.) %>%
  select(runtime)

mb.summed$statewide <- rep(statewide.values$runtime, 4)

mb.summed$ratio <- mb.summed$runtime/mb.summed$statewide

boxplot(mb.summed$ratio~mb.summed$scale)
