# Week 2 code-along
library(tidyverse)
library(lme4)
library(effects)

# Part 1: logistic MLM
# test-enhanced learning
load("./data/TestEnhancedLearning.RData")
summary(tel)
ggplot(tel, aes(Delay, Correct, fill=Group)) +
  stat_summary(fun=mean, geom="col", position = "dodge")

m.tel <- glmer(Correct ~ Group*Delay + (Delay | Subject_ID), 
               data=tel, family = "binomial")
summary(m.tel)

ef.tel <- as.data.frame(effect("Group:Delay", m.tel))
ef.tel
ggplot(ef.tel, aes(Delay, fit, color=Group)) +
  geom_pointrange(aes(ymin=lower, ymax=upper), 
                  position=position_dodge(width=0.2))

# Part 2: linear LDA: weight maintenance lab exercise
# following a weight-loss programme, participants were randomly assigned to one of three weight maintenance conditions: None (Control), MR (use a meal replacement to replace one meal and snack per day), or ED (book and educational materials on purchasing and preparing foods lower in energy density - reduced fat content and/or increased water content). Weight was assessed at baseline (start of maintenance), 12 months post, 24 months post, and 36 months post.
load(url("https://uoepsy.github.io/msmr/data/WeightMaintain3.rda"))

# Q: Overall, did the participants maintain their weight loss or did their weights change?
m.null <- lmer(WeightChange ~ 1 + (Assessment | ID), data=WeightMaintain3, REML=F)
m.base <- lmer(WeightChange ~ Assessment + (Assessment | ID), data=WeightMaintain3, REML=F)
anova(m.null, m.base)

# Q: Did the experimental condition groups differ in overall weight change and rate of weight change (non-maintenance)?
m.int <- lmer(WeightChange ~ Assessment + Condition + (Assessment | ID), data=WeightMaintain3, REML=F)
m.full <- lmer(WeightChange ~ Assessment*Condition + (Assessment | ID), data=WeightMaintain3, REML=F)
anova(m.null, m.base, m.int, m.full)

# Q: Interpret the coefficients of the full model
coef(summary(m.full))

# Q: Make a graph of the model fit and the observed data.
ggplot(WeightMaintain3, aes(Assessment, WeightChange, color=Condition)) + 
  stat_summary(fun.data=mean_se, geom="pointrange", size=1) + 
  stat_summary(aes(y=fitted(m.full)), fun.y=mean, geom="line") + 
  theme_bw(base_size=12) + scale_color_manual(values=c("black", "red", "blue"))

ggplot(fortify(m.full), aes(Assessment, WeightChange, color=Condition)) + 
  stat_summary(fun.data=mean_se, geom="pointrange", size=1) + 
  stat_summary(aes(y=.fitted), fun.y=mean, geom="line") + 
  theme_bw(base_size=12) + scale_color_manual(values=c("black", "red", "blue"))