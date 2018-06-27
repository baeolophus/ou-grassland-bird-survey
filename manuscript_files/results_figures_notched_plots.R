rmse_downscale <-readRDS("manuscript_files/rmse_downscale.rds")
auc_downscale <- readRDS("manuscript_files/auc_downscale.rds")


rmse_current <- readRDS("manuscript_files/rmse_current.rds")
auc_current <- readRDS("manuscript_files/auc_current.rds")

rmse_current$predictor_res <- "high"
auc_current$predictor_res <- "high"
rmse_downscale$predictor_res <- "low"
auc_downscale$predictor_res <- "low"

errors <- rbind(auc_current,
                auc_downscale,
                rmse_current,
                rmse_downscale)

library(ggplot2)

ggplot()+
  geom_boxplot(data = errors,
               aes(x = scale,
                   y = evalue,
                   fill = predictor_res),
               notch = TRUE)+
  facet_wrap(errortype~species,
             scales = "free",
             nrow = 2,
             ncol = 11
             )+
  theme_bw()
