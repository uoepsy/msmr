# Week 3 Live R
# Re-analyze `TargetFix` data using logistic MLM (logistic GCA)

library(tidyverse)
library(lme4)
source("code_poly.R")
load("./data/TargetFix.rda")
summary(TargetFix)

ggplot(TargetFix, aes(Time, meanFix, color=Condition)) +
  stat_smooth()

# create 3rd-order orth poly

# fit maximal model

