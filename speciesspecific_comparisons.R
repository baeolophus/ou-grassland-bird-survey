#This file calculates whether sig diffs occur

setwd("E:/Documents/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current")

SPECIES <- "NOBO"
SPECIES <- "UPSA"
SPECIES <- "HOLA"
SPECIES <- "CASP"
SPECIES <- "FISP"
SPECIES <- "LASP"
SPECIES <- "GRSP"
SPECIES <- "DICK"
SPECIES <- "EAME"
SPECIES <- "WEME"
SPECIES <- "BHCO"


#loading Rdata gets the rest!
load(paste0("~/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ensemble_results/Current/",
            SPECIES,
            "/",
            SPECIES,
            "_rdata.RData"))

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

aucsameyear <- lm(errornum ~ scale, data = eval.df.sep[eval.df.sep$errortype=="auc"&eval.df.sep$year=="sameyear",])
Anova(aucsameyear, type = 2)
plot(errornum ~ scale, data = eval.df.sep[eval.df.sep$errortype=="auc"&eval.df.sep$year=="sameyear",])
summary(aucsameyear)

aucdiffear <- lm(errornum ~ scale, data = eval.df.sep[eval.df.sep$errortype=="auc"&eval.df.sep$year=="diffyear",])
Anova(aucdiffear, type = 2)
plot(errornum ~ scale, data = eval.df.sep[eval.df.sep$errortype=="auc"&eval.df.sep$year=="diffyear",])
summary(aucdiffear)


rmsesameyear <- lm(errornum ~ scale, data = eval.df.sep[eval.df.sep$errortype=="rmse"&eval.df.sep$year=="sameyear",])
Anova(rmsesameyear, type = 2)
plot(errornum ~ scale, data = eval.df.sep[eval.df.sep$errortype=="rmse"&eval.df.sep$year=="sameyear",])
summary(rmsesameyear)

rmsediffear <- lm(errornum ~ scale, data = eval.df.sep[eval.df.sep$errortype=="rmse"&eval.df.sep$year=="diffyear",])
Anova(rmsediffear, type = 2)
plot(errornum ~ scale, data = eval.df.sep[eval.df.sep$errortype=="rmse"&eval.df.sep$year=="diffyear",])
summary(rmsediffear)



test.of.var <- data.frame("numbioclim" = c(5,8,9,8,2,6,5,3,2,10,2),
                          "numnonstate" = c(3,2,0,0,1,1,1,0,1,0,3))
modelvar <- glm(numnonstate ~ numbioclim,
               data = test.of.var,
               family = "poisson")
summary(modelvar)
