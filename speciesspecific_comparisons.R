#This file calculates whether sig diffs occur

setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current")

p.adjust
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
  
  eval.df.sep$scale <- factor(eval.df.sep$scale,
                              levels = c("statewide",
                                         "large",
                                         "medium",
                                         "small"))
  
  eval.df.sep$species <- SPECIES
  return(eval.df.sep)
}

listofeval <- lapply (specieslist,
                      FUN = eval.function)
evalsforbinding <- do.call(rbind,
                           lapply(listofeval, 
                                  data.frame, 
                                  stringsAsFactors=FALSE))


anova.for.species.error <- function (SPECIES,
                                     errortypehere,
                                     yearhere) {
errormodel <- lm(errornum ~ scale, data = evalsforbinding[evalsforbinding$errortype==errortypehere&
                                                            evalsforbinding$year==yearhere&
                                                            evalsforbinding$species==SPECIES,])
Anova(errormodel, type = 2)
plot(errornum ~ scale, data = evalsforbinding[evalsforbinding$errortype==errortypehere&
                                            evalsforbinding$year==yearhere&
                                              evalsforbinding$species==SPECIES,])


summary.extracted <- data.frame(summary(errormodel)[[4]])

beta <- summary.extracted$Estimate
SE <- summary.extracted$Std..Error
P <- summary.extracted$Pr...t..

return(list(beta,
            SE,
            P,
            summary.extracted,
            errormodel))
}

sumanova <- anova.for.species.error("NOBO",
                        "auc",
                        "sameyear")

str(sumanova)





#Then combine elements 1, 2, 3 into a data frame
#get adjusted p-value column
#paste
#spread

p.adjust(sumanova[[1]]$Pr...t..,
         method = "holm")
