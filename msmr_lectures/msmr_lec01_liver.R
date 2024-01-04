#####
# Week 1 liveR: a 2x2 repeated-measures anova in MLM framework
# Effect of caffeine consumption on processing speed in younger vs older adults
#####

# load packages
library(tidyverse)
library(lme4)
library(lmerTest)

# load and inspect the data
load("./data/caff_age.rda")

# exploratory plots

# fit the model(s)