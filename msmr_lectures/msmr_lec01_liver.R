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
head(caff_age_multiday)
# exploratory plots
ggplot(caff_age, aes(Age, RT, fill=Condition)) +
  geom_boxplot()

ggplot(caff_age_multiday, aes(Age, RT, fill=Condition)) +
  geom_boxplot()

# fit the model(s)
# m <- lmer(RT ~ Condition*Age +
#             (1 +  Condition | Participant),
#           data = caff_age, REML=F)
m <- lmer(RT ~ Condition*Age +
            (1 +  Condition | Participant),
          contrasts = list(Condition = contr.sum,
                           Age = contr.sum),
          data = caff_age_multiday, REML=F)
summary(m)

m_ols <- lm(RT ~ Condition*Age,
            contrasts = list(Condition = contr.sum,
                             Age = contr.sum),
            data = caff_age_multiday)
summary(m_ols)
