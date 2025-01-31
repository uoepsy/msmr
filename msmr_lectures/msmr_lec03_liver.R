# Week 3 Live R
# Re-analyze `TargetFix` data using logistic MLM (logistic GCA)

library(tidyverse)
library(lme4)
source("code_poly.R")
load("./data/TargetFix.rda")
summary(TargetFix)
ggplot(TargetFix, aes(Time, meanFix, color=Condition)) + 
  stat_summary(fun.data=mean_se, geom="pointrange")

# prep for analysis
TargetFix.gca <- code_poly(TargetFix, 
                           predictor = "Time",
                           poly.order = 5)
summary(TargetFix.gca)

# fit the model
m.target_log <- glmer(cbind(sumFix, N-sumFix) ~
                        (poly1+poly2+poly3)*Condition +
                        ((poly1+poly2+poly3)*Condition | Subject),
                      data=TargetFix.gca, family = "binomial")
summary(m.target_log)

# plot model fit
ggplot(TargetFix, aes(Time, meanFix, color=Condition)) + 
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=fitted(m.target_log)), 
               fun=mean, geom="line")

# choosing poly order
ggplot(TargetFix, aes(Time, meanFix, color)) + 
  stat_smooth()

m.check <- glmer(cbind(sumFix, N-sumFix) ~
                   (poly1+poly2+poly3+poly4+poly5) +
                   ((poly1+poly2+poly3+poly4+poly5) | Subject),
                 data=TargetFix.gca, family = "binomial")
summary(m.check)


load("http://uoepsy.github.io/msmr/data/WordLearnEx.rda")
