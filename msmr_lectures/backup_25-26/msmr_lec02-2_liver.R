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
  stat_summary(fun.data=mean_se, geom="pointrange")

# fit the models
m.vs <- lmer(RT ~ Set.Size*Group +
               (Set.Size | Participant),
             contrasts = list(Group = "contr.sum"),
             data=VisualSearchEx, REML=F)
summary(m.vs)

m.vs.zc <- lmer(RT ~ Set.Size*Group +
               (Set.Size || Participant),
             contrasts = list(Group = "contr.sum"),
             data=VisualSearchEx, REML=F)
summary(m.vs.zc)

tidy(m.vs, effects="fixed")
tidy(m.vs.zc, effects="fixed")

# plot model fit
ggplot(augment(m.vs), aes(Set.Size, RT, color=Group)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=.fitted), fun=mean, geom="line")

