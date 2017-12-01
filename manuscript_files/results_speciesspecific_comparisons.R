#This file calculates whether significant differences
#occur between AUC/RMSE values for each scale, then outputs them into files for a table.
library(tidyr)
library(dplyr)
library(car)

#One of the working directories
setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current") #table 3 results
#setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Downscale_current") #table 4 results

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
plot(errornum ~ scale, 
     data = evalsforbinding[evalsforbinding$errortype==errortypehere&
                                            evalsforbinding$year==yearhere&
                                              evalsforbinding$species==SPECIES,],
     main = SPECIES,
     ylab = errortypehere,
     xlab = yearhere)


summary.extracted <- data.frame(summary(errormodel)[[4]])

beta <- summary.extracted$Estimate
SE <- summary.extracted$Std..Error
P <- summary.extracted$Pr...t..
summary.extracted$species <- SPECIES
summary.extracted$var <- c("intercept",
                           "large",
                           "medium",
                           "small")

return(summary.extracted)
}

list.auc.sameyear <- lapply (specieslist,
                      FUN = anova.for.species.error,
                      "auc",
                      "sameyear")

df.auc.sameyear <- do.call(rbind,
                           lapply(list.auc.sameyear, 
                                  data.frame, 
                                  stringsAsFactors=FALSE))

df.auc.sameyear$padjust <- p.adjust(df.auc.sameyear$Pr...t..,
                                    method = "holm")

df.auc.sameyear.filter <- df.auc.sameyear %>%
  filter(var != "intercept")

df.auc.sameyear.filter$pastedfortable <- paste0(round(df.auc.sameyear.filter$Estimate, 3),
                                               "\u00b1",
                                               round(df.auc.sameyear.filter$Std..Error, 3),
                                               " (",
                                               round(df.auc.sameyear.filter$padjust, 4),
                                               ")"
                                               )

df.auc.sameyear.filter$pastedfortable <- gsub(pattern = "(0)",
                                              replacement = "(<0.0001)",
                                              df.auc.sameyear.filter$pastedfortable,
                                              fixed = TRUE)

df.auc.sameyear.filter$sign <- sign(df.auc.sameyear.filter$Estimate)
df.auc.sameyear.filter$whichbest <- ifelse(df.auc.sameyear.filter$sign == 1 &
                                             df.auc.sameyear.filter$padjust>=0.05,
                                           yes = "scale",
                                           no = "statewide")

spread.auc.sameyear <- df.auc.sameyear.filter %>%
  select(var,
         pastedfortable,
         species) 

spread.auc.sameyear$species <- factor(spread.auc.sameyear$species,
                                    levels=specieslist) 
spread.auc.sameyear<- spread.auc.sameyear[do.call(order,
                            spread.auc.sameyear[c('species')]),] 
write.csv(spread.auc.sameyear,
          "table3_auc_sameyear_downscale.csv")

#rmse
list.rmse.sameyear <- lapply (specieslist,
                             FUN = anova.for.species.error,
                             "rmse",
                             "sameyear")

df.rmse.sameyear <- do.call(rbind,
                           lapply(list.rmse.sameyear, 
                                  data.frame, 
                                  stringsAsFactors=FALSE))

df.rmse.sameyear$padjust <- p.adjust(df.rmse.sameyear$Pr...t..,
                                    method = "holm")

df.rmse.sameyear.filter <- df.rmse.sameyear %>%
  filter(var != "intercept")

df.rmse.sameyear.filter$pastedfortable <- paste0(round(df.rmse.sameyear.filter$Estimate, 3),
                                                "\u00b1",
                                                round(df.rmse.sameyear.filter$Std..Error, 3),
                                                " (",
                                                round(df.rmse.sameyear.filter$padjust, 4),
                                                ")"
)

df.rmse.sameyear.filter$pastedfortable <- gsub(pattern = "(0)",
                                              replacement = "(<0.0001)",
                                              df.rmse.sameyear.filter$pastedfortable,
                                              fixed = TRUE)

df.rmse.sameyear.filter$sign <- sign(df.rmse.sameyear.filter$Estimate)
df.rmse.sameyear.filter$whichbest <- ifelse(df.rmse.sameyear.filter$sign == 1 &
                                             df.rmse.sameyear.filter$padjust>=0.05,
                                           yes = "scale",
                                           no = "statewide")

spread.rmse.sameyear <- df.rmse.sameyear.filter %>%
  select(var,
         pastedfortable,
         species) 

spread.rmse.sameyear$species <- factor(spread.rmse.sameyear$species,
                                      levels=specieslist) 
spread.rmse.sameyear<- spread.rmse.sameyear[do.call(order,
                                                  spread.rmse.sameyear[c('species')]),] 
write.csv(spread.rmse.sameyear,
          "table3_rmse_sameyear_downscale.csv")


#diff year auc
#diff year rmse

list.auc.diffyear <- lapply (specieslist,
                             FUN = anova.for.species.error,
                             "auc",
                             "diffyear")

df.auc.diffyear <- do.call(rbind,
                           lapply(list.auc.diffyear, 
                                  data.frame, 
                                  stringsAsFactors=FALSE))

df.auc.diffyear$padjust <- p.adjust(df.auc.diffyear$Pr...t..,
                                    method = "holm")

df.auc.diffyear.filter <- df.auc.diffyear %>%
  filter(var != "intercept")

df.auc.diffyear.filter$pastedfortable <- paste0(round(df.auc.diffyear.filter$Estimate, 3),
                                                "\u00b1",
                                                round(df.auc.diffyear.filter$Std..Error, 3),
                                                " (",
                                                round(df.auc.diffyear.filter$padjust, 4),
                                                ")"
)

df.auc.diffyear.filter$pastedfortable <- gsub(pattern = "(0)",
                                              replacement = "(<0.0001)",
                                              df.auc.diffyear.filter$pastedfortable,
                                              fixed = TRUE)

df.auc.diffyear.filter$sign <- sign(df.auc.diffyear.filter$Estimate)
df.auc.diffyear.filter$whichbest <- ifelse(df.auc.diffyear.filter$sign == 1 &
                                             df.auc.diffyear.filter$padjust>=0.05,
                                           yes = "scale",
                                           no = "statewide")

spread.auc.diffyear <- df.auc.diffyear.filter %>%
  select(var,
         pastedfortable,
         species) 

spread.auc.diffyear$species <- factor(spread.auc.diffyear$species,
                                      levels=specieslist) 
spread.auc.diffyear<- spread.auc.diffyear[do.call(order,
                                                  spread.auc.diffyear[c('species')]),] 
write.csv(spread.auc.diffyear,
          "table3_auc_diffyear_downscale.csv")

#rmse
list.rmse.diffyear <- lapply (specieslist,
                              FUN = anova.for.species.error,
                              "rmse",
                              "diffyear")

df.rmse.diffyear <- do.call(rbind,
                            lapply(list.rmse.diffyear, 
                                   data.frame, 
                                   stringsAsFactors=FALSE))

df.rmse.diffyear$padjust <- p.adjust(df.rmse.diffyear$Pr...t..,
                                     method = "holm")

df.rmse.diffyear.filter <- df.rmse.diffyear %>%
  filter(var != "intercept")

df.rmse.diffyear.filter$pastedfortable <- paste0(round(df.rmse.diffyear.filter$Estimate, 3),
                                                 "\u00b1",
                                                 round(df.rmse.diffyear.filter$Std..Error, 3),
                                                 " (",
                                                 round(df.rmse.diffyear.filter$padjust, 4),
                                                 ")"
)

df.rmse.diffyear.filter$pastedfortable <- gsub(pattern = "(0)",
                                               replacement = "(<0.0001)",
                                               df.rmse.diffyear.filter$pastedfortable,
                                               fixed = TRUE)

df.rmse.diffyear.filter$sign <- sign(df.rmse.diffyear.filter$Estimate)
df.rmse.diffyear.filter$whichbest <- ifelse(df.rmse.diffyear.filter$sign == 1 &
                                              df.rmse.diffyear.filter$padjust>=0.05,
                                            yes = "scale",
                                            no = "statewide")

spread.rmse.diffyear <- df.rmse.diffyear.filter %>%
  select(var,
         pastedfortable,
         species) 

spread.rmse.diffyear$species <- factor(spread.rmse.diffyear$species,
                                       levels=specieslist) 
spread.rmse.diffyear<- spread.rmse.diffyear[do.call(order,
                                                    spread.rmse.diffyear[c('species')]),] 
write.csv(spread.rmse.diffyear,
          "table3_rmse_diffyear_downscale.csv")


