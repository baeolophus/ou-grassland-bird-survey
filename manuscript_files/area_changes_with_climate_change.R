#comparing current and future areas by scale.

setwd("~/college/OU-postdoc/research/grassland_bird_surveys/ougrassland/ou-grassland-bird-survey")
thresholds <- read.csv("thresholds.csv")

library(lme4)
library(lmerTest)
library(tidyr)
library(dplyr)

thresholds.col <- thresholds %>% select(-X.1) %>% 
  spread(.,
                         key = model,
                         value = areakm2)

thresholds.col$diff <- thresholds.col$future-thresholds.col$current

thresholds.col$changeofcurrentarea <- (thresholds.col$diff)/(thresholds.col$current)

#change factor order so all compared to statewide
thresholds.col$X <-factor(thresholds.col$X,
                             levels = c("statewide.area",
                                        "large.area",
                                        "medium.area",
                                        "small.area"))

model.changes <- lmer(changeofcurrentarea ~ X + (1|Species),
                      data = thresholds.col)

summary(model.changes)

par(mfrow=c(1,1))
plot(changeofcurrentarea ~ X,
     data = thresholds.col,
     ylim = c(0, 50),
     xaxt = "n",
     xlab = "Scale",
     ylab = "Area in square km")
axis(side = 1,
     at = c(1:4),
     labels = c("statewide",
                "large",
                "medium",
                "small"))
