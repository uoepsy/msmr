# Week 3 code-along
# Re-analyze `TargetFix` data using logistic MLM (logistic GCA)

library(tidyverse)
library(lme4)
source("https://uoepsy.github.io/msmr/functions/code_poly.R")
load(url("https://uoepsy.github.io/msmr/data/TargetFix.rda"))
summary(TargetFix)

#make 3rd-order orth poly
TargetFix <- code_poly(TargetFix, predictor="timeBin", poly.order=3, draw.poly=F)

# fit logisitc GCA model
m.log <- glmer(cbind(sumFix, N-sumFix) ~ (poly1+poly2+poly3)*Condition +
                 (poly1+poly2+poly3 | Subject) +
                 (poly1+poly2 | Subject:Condition),
               data=TargetFix, family=binomial)
summary(m.log)

# Simpler random effects: 
# note that the correlations between Subject-level random effects are all +1.00 or -1.00, so can simplify the structure by removing them:

m.log_zc <- glmer(cbind(sumFix, N-sumFix) ~ (poly1+poly2+poly3)*Condition +
                    (poly1+poly2+poly3 || Subject) +
                    (poly1+poly2 | Subject:Condition),
                  data=TargetFix, family=binomial)
summary(m.log_zc)

#Plot model fit
ggplot(TargetFix, aes(Time, meanFix, color=Condition)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=fitted(m.log)), fun.y=mean, geom="line") +
  stat_summary(aes(y=fitted(m.log_zc)), fun.y=mean, geom="line", linetype="dashed") +
  theme_bw() + expand_limits(y=c(0,1)) + 
  labs(y="Fixation Proportion", x="Time since word onset (ms)")
