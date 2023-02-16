library(tidyverse) # for data wrangling
# for fitting and visualising models
library(lme4) 
library(lmerTest)
library(broom.mixed) 
library(effects)

############
# Q&A
############
# writing up results
# 1. model structure: fixed effects, random effects, family
# 1b. convergence issues
# 2. model building, model comparisons, p-value estimation
# 3. tables and figures

############ SNARC effect: mental number line; left-hand responses (parity judgments) are faster for smaller numbers, right-hand responses are faster for larger numbers
# data from Koch et al. (2023) Collabra:Psychology, 9(1):67908, doi.org/10.1525/collabra.67908
library(tidyverse)
library(lme4)
library(lmerTest)
load("./data/SNARC.RData")
summary(dat_snarc)
dat_snarc$math <- as.numeric(dat_snarc$math)
ggplot(dat_snarc, aes(stimulus_mag, dRT)) +
  geom_point() + stat_smooth(method="lm")

m_mag <- lmer(dRT ~ stimulus_mag +
                (stimulus_mag | subject),
              data=dat_snarc, REML=F)
summary(m_mag)

m_mag_math <- lmer(dRT ~ stimulus_mag*math +
                (stimulus_mag | subject),
              data=dat_snarc, REML=F)
summary(m_mag_math)

source("get_ranef.R")
re <- get_ranef(m_mag, "subject")
summary(re)
head(re)

indiv_diffs <- unique(merge(re, dat_snarc[, 1:5]))
summary(indiv_diffs)

with(indiv_diffs, cor.test(stimulus_mag, Age))
ggplot(indiv_diffs, aes(Age, stimulus_mag)) + geom_point() + stat_smooth(method="lm")

with(indiv_diffs, cor.test(stimulus_mag, math))
ggplot(indiv_diffs, aes(math, stimulus_mag)) + geom_point() + stat_smooth(method="lm")

############
source(url("https://uoepsy.github.io/msmr/functions/get_ranef.R"))
