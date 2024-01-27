# Week 2 Live R
library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(broom.mixed)

# Part 2: Visual search - Longitudinal data analysis is not just for longitudinal data

# visual search data from stroke survivors and neurologically-intact control participants
load("./data/VisualSearchEx.rda")
str(VisualSearchEx)

# plot the data
ggplot(VisualSearchEx, aes(Set.Size, RT, color=Group)) +
  stat_summary(fun.data = mean_se, geom="pointrange")

# fit the models
m <- lmer(RT ~ Set.Size + 
            (Set.Size | Participant),
          data=VisualSearchEx, REML=F)

m_g <- lmer(RT ~ Set.Size + Group +
            (Set.Size | Participant),
          data=VisualSearchEx, REML=F)

m_full <- lmer(RT ~ Set.Size * Group + 
            (Set.Size | Participant),
          data=VisualSearchEx, REML=F)

anova(m, m_g, m_full)

summary(m_full)


# plot model fit
summary(augment(m_full))
ggplot(augment(m_full), aes(Set.Size, RT, color=Group)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(aes(y = .fitted), fun=mean, geom="line")

