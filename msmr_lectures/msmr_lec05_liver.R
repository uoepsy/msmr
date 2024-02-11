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


############
source(url("https://uoepsy.github.io/msmr/functions/get_ranef.R"))
