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
summary(caff_age)
head(caff_age)
ggplot(caff_age, aes(Condition, RT, fill=Age)) +
  geom_boxplot()
ggplot(caff_age, aes(Age, RT, fill=Condition)) +
  geom_boxplot()

m <- lmer(RT ~ Condition*Age +
            (Condition | Participant),
          data = caff_age, REML=FALSE)
summary(m)
