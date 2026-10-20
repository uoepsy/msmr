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
library(broom.mixed)
load("./data/SNARC.RData")
summary(dat_snarc)
head(dat_snarc)
# key variables:
# stimulus_mag
# dRT
# math - individual difference in math ability

ggplot(dat_snarc, aes(stimulus_mag, dRT)) +
  geom_point() + stat_smooth(method="lm") +
  geom_hline(yintercept = 0, linetype="dashed")

m <- lmer(dRT ~ stimulus_mag + 
            (stimulus_mag | subject),
          data=dat_snarc)
summary(m)

m.excl <- lmer(dRT ~ stimulus_mag + 
                 (stimulus_mag | subject),
               data=subset(augment(m), 
                           abs(.resid) < 3*sd(.resid)))
summary(m.excl)

### individual difference analyses
# math scores are stored as character, but should be numeric, so convert them
dat_snarc$math <- as.numeric(dat_snarc$math)
summary(dat_snarc)

m.math <- lmer(dRT ~ stimulus_mag*math + 
                 (stimulus_mag | subject),
               data=dat_snarc)
summary(m.math)

m.hand <- lmer(dRT ~ stimulus_mag*Handedness + 
                 (stimulus_mag | subject),
               data=dat_snarc)
summary(m.hand)

############
source(url("https://uoepsy.github.io/msmr/functions/get_ranef.R"))
