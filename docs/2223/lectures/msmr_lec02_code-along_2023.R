# Week 2 code-along
library(tidyverse)
library(lme4)
library(effects)

#######################
# Part 1: logistic MLM
#######################
# Example 1: test-enhanced learning
# after initially studying some material, is it better to do another round of study or to do a test? does this depend on the delay before the final test?
# 2 groups: study-study, study-test
# each participant tested 2 times: after 1 minute, after 1 week
load(url("https://uoepsy.github.io/msmr/data/TestEnhancedLearning.RData"))
summary(tel)

tel %>% group_by(Subject_ID, Group, Delay) %>% 
  summarize(Acc = mean(Correct)) %>% 
   ggplot(aes(Delay, Acc, fill=Group)) + geom_boxplot()

m <- glmer(Correct ~ Delay*Group +
             (Delay | Subject_ID),
           data=tel, family="binomial")
summary(m)

# Example 2: cross-cultural bouba/kiki effect
# Cwiek, et al. (2022). The bouba/kiki effect is robust across cultures and writing systems. Philosophical Transactions of the Royal Society B, 377(1841), 20200390.
kiki <- read_csv('https://osf.io/4psw7/download')
summary(kiki)
# Match outcomes for 917 participants across 25 languages that fall into 9 families, with roman and non-roman scripts
table(kiki$Family, kiki$Script)
# research questions:
# 1. is there an iconicity effect (match > 0.5)?
# 2. is that effect modulated by script type (roman vs non-roman)?
# 3. does it matter if "bouba" or "kiki" stimulus is presented first?


###########
# Part 2: linear LDA
###########
# following a weight-loss programme, participants were randomly assigned to one of three weight maintenance conditions: None (Control), MR (use a meal replacement to replace one meal and snack per day), or ED (book and educational materials on purchasing and preparing foods lower in energy density - reduced fat content and/or increased water content). Weight was assessed at baseline (start of maintenance), 12 months post, 24 months post, and 36 months post.
library(lmerTest)
load(url("https://uoepsy.github.io/msmr/data/WeightMaintain3.rda"))
summary(WeightMaintain3)

ggplot(WeightMaintain3, aes(Assessment, WeightChange, color=Condition)) + stat_summary(fun.data = mean_se, geom="pointrange")

# Q1: Overall, did the participants maintain their weight loss or did their weights change?

m0 <- lmer(WeightChange ~ Assessment +
            (Assessment | ID), 
          contrasts = list(Condition = "contr.sum"),
          data=WeightMaintain3,
          REML = F) 
mc <- lmer(WeightChange ~ Assessment+Condition +
             (Assessment | ID), 
           #contrasts = list(Condition = "contr.sum"),
           data=WeightMaintain3,
           REML = F) 
m1 <- lmer(WeightChange ~ Assessment*Condition +
             (Assessment | ID), 
           contrasts = list(Condition = "contr.sum"),
           data=WeightMaintain3,
           REML = F) 
anova(m0, mc, m1)
summary(m1)

# Q2: Did the experimental condition groups differ in overall weight change and rate of weight change (non-maintenance)?

