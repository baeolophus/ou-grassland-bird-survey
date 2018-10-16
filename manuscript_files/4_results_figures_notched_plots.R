rmse_downscale <-readRDS("manuscript_files/rmse_downscale.rds")
auc_downscale <- readRDS("manuscript_files/auc_downscale.rds")


rmse_current <- readRDS("manuscript_files/rmse_current.rds")
auc_current <- readRDS("manuscript_files/auc_current.rds")

rmse_current$predictor_res <- "fine"
auc_current$predictor_res <- "fine"
rmse_downscale$predictor_res <- "coarse"
auc_downscale$predictor_res <- "coarse"

errors <- rbind(auc_current,
                auc_downscale,
                rmse_current,
                rmse_downscale)

#Non-banding-code names for species
species_names <- c("BHCO" = "Brown-headed Cowbird",
                   "CASP" = "Cassin's Sparrow",
                   "DICK" = "Dickcissel",
                   "EAME" = "Eastern Meadowlark",
                   "FISP" = "Field Sparrow",
                   "GRSP" = "Grasshopper Sparrow",
                   "HOLA" = "Horned Lark",
                   "LASP" = "Lark Sparrow",
                   "NOBO" = "Northern Bobwhite",
                   "UPSA" = "Upland Sandpiper",
                   "WEME" = "Western Meadowlark")

library(ggplot2)
library(lemon)
pdf(file = "manuscript_files/figures/Figure6.pdf",
    width = 8.5,
    height = 11)
fig6 <- ggplot()+
  geom_boxplot(data = subset(errors, errortype == "rmse"),
               aes(x = scale,
                   y = evalue,
                   fill = predictor_res),
               notch = TRUE)+
  facet_wrap(~species,
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(species_names)
             )+
  theme_bw()+
  scale_y_continuous(minor_breaks = seq(0, 1, 0.0025))+
  scale_fill_manual(values=c("white", "darkgray"),
                    name = "Predictor resolution")+
  labs( x = "Model scale",
          y = "RMSE")

#Get panel name for lower rightmost panel.
#gtable_show_names(fig6)
#put legend in that empty facet panel.
reposition_legend(fig6,
                  position = "center",
                  panel = "panel-3-4")
dev.off()

#AUC
pdf(file = "manuscript_files/figures/Figure7.pdf",
    width = 8.5,
    height = 11)
fig7 <- ggplot()+
  geom_boxplot(data = subset(errors, errortype == "auc"),
               aes(x = scale,
                   y = (1-evalue),
                   fill = predictor_res),
               notch = TRUE)+
  facet_wrap(~species,
             scales = "free",
             nrow = 4,
             ncol = 3,
             labeller = as_labeller(species_names)
  )+
  theme_bw()+
  scale_y_continuous(minor_breaks = seq(0, 1, 0.0025))+
  scale_fill_manual(values=c("white", "darkgray"),
                    name = "Predictor resolution")+
  labs( x = "Model scale",
        y = "1 - AUC")

#Get panel name for lower rightmost panel.
#gtable_show_names(fig7)
#put legend in that empty facet panel.
reposition_legend(fig7,
                  position = "center",
                  panel = "panel-3-4")
dev.off()
