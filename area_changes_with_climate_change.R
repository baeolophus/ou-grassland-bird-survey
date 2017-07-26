#comparing current and future areas by scale.

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


plot(changeofcurrentarea ~ X,
     data = thresholds.col,
     ylim = c(0, 50))
