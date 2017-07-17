#microbenchmark comparisons

library(dplyr)
library(lme4)
library(lmerTest)

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
boxplot(time ~ runtype, data = mb.df.sep)
mb.summed <- group_by(mb.df.sep,
                      scale,
                      Species) %>%
  summarize("runtime" = sum(time)*1000000000) #convert from default nanoseconds to seconds



statewide.values <-filter (mb.summed,
                            scale == "statewide") %>%
  ungroup(.) %>%
  dplyr::select(., runtime)

mb.summed$statewide <- rep(statewide.values$runtime, 4)

mb.summed$ratio <- mb.summed$runtime/mb.summed$statewide

boxplot(mb.summed$ratio~mb.summed$scale)
boxplot()

#add in evaluation results

eval.function <- function(SPECIES) {
evalresults <- readRDS(file.path(
  SPECIES,
  paste0(
    SPECIES,
    "_products_evaluation_results"))
)
#contains AUC/RMSE for all.
eval.df <- data.frame(evalresults)

listerrornames <- c("statewide.sampling.rmse.sameyear",
                    "statewide.sampling.auc.sameyear",
                    "small.sampling.rmse.sameyear",
                    "small.sampling.auc.sameyear",
                    "medium.sampling.rmse.sameyear",
                    "medium.sampling.auc.sameyear",
                    "large.sampling.rmse.sameyear",
                    "large.sampling.auc.sameyear",
                    "statewide.sampling.rmse.diffyear",
                    "statewide.sampling.auc.diffyear",
                    "small.sampling.rmse.diffyear",
                    "small.sampling.auc.diffyear",
                    "medium.sampling.rmse.diffyear",
                    "medium.sampling.auc.diffyear",
                    "large.sampling.rmse.diffyear",
                    "large.sampling.auc.diffyear")
colnames(eval.df) <- listerrornames

eval.df.sep <- gather_(eval.df,
                       key_col = "errordescriptor",
                       value_col = "errornum",
                       gather_cols = listerrornames) %>%
  separate_(col = "errordescriptor",
            into = c("scale",
                     "sampling",
                     "errortype",
                     "year"))

eval.df.sep$scale <- as.factor(eval.df.sep$scale)
levels(eval.df.sep$scale) <- c("statewide",
                               "large",
                               "medium",
                               "small")
eval.df.sep$species <- SPECIES
return(eval.df.sep)
}

listofeval <- lapply (specieslist,
                    FUN = eval.function)
evalsforbinding <- do.call(rbind,
                 lapply(listofeval, 
                               data.frame, 
                               stringsAsFactors=FALSE))

evalsforbinding.sameyear.rmse <- filter (evalsforbinding,
                                         errortype == "auc",
                                         year == "sameyear") %>%
  group_by(species,
           scale)%>%
  summarize(mean_rmse = mean(errornum))


joined.rmse <- left_join(x = evalsforbinding.sameyear.rmse,
                         y = mb.summed,
                         by = c("scale" = "scale",
                                "species" = "Species"))

joined.rmse$scale <- as.factor(joined.rmse$scale)

levels(joined.rmse$scale) <- c("statewide",
                               "large",
                               "medium",
                               "small")

plot(ratio ~ scale,
     data = joined.rmse)

rmse.model <- lmer(mean_rmse ~ ratio + (1|species),
                   data = joined.rmse)
summary(rmse.model)
plot(rmse.model)

runtime.model <- lmer(ratio ~ scale + (1|species),
                   data = joined.rmse)
summary(runtime.model)
plot(runtime.model)


                         