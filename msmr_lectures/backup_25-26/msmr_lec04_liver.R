library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)

load("./data/group_tx.RData")
summary(group_tx)
# The data: 44 participants across 4 groups (between-subjects) were tested 5 times (waves) in 11 domains. In each wave of testing, each domain received a score on a 20-point scale and a set of several questions, which could be answered correctly or incorrectly.
