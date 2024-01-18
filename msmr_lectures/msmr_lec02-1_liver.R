# Week 2 Live R
library(tidyverse)
library(lme4)
library(effects)

# Part 1: logistic MLM
# test-enhanced learning: Does attempting to recall information (i.e., a practice test) produce better learning than re-studying the material?
load("./data/TestEnhancedLearning.RData")
summary(tel)
ggplot(tel, aes(Delay, Correct, fill=Group)) +
  stat_summary(fun=mean, geom="col", position = "dodge")

# another example: cross-cultural bouba/kiki effect
# Cwiek, et al. (2022). The bouba/kiki effect is robust across cultures and writing systems. Philosophical Transactions of the Royal Society B, 377(1841), 20200390.
kiki <- read_csv('https://osf.io/4psw7/download')
summary(kiki)
# Match (0/1) outcomes for 917 participants across 25 languages that fall into 9 families, with roman and non-roman scripts
table(kiki$Family, kiki$Script) # 9 language families, 2 script types
table(kiki$Language, kiki$Family) # 25 languages, 9 language families

# Each participant did only 1 trial, what are the clusters?

