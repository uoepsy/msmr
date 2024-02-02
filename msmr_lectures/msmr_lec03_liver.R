# Week 3 Live R
# Re-analyze `TargetFix` data using logistic MLM (logistic GCA)

library(tidyverse)
library(lme4)
source("code_poly.R")
load("./data/TargetFix.rda")
summary(TargetFix)

ggplot(TargetFix, aes(Time, meanFix, color=Condition)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_smooth()

# create 3rd-order orth poly
TargetFix.gca <- code_poly(TargetFix, predictor = "Time", poly.order=3)
summary(TargetFix.gca)
# fit maximal model

m.full <- glmer(cbind(sumFix, N-sumFix) ~ (poly1+poly2+poly3)*Condition +
                  (poly1+poly2+poly3 | Subject) +
                  (poly1+poly2+poly3 | Subject:Condition),
                family = "binomial",
                data=TargetFix.gca)
summary(m.full)

# remove Subject ranef correlations
m.full.zcp <- glmer(cbind(sumFix, N-sumFix) ~ 
                      (poly1+poly2+poly3)*Condition +
                  (poly1+poly2+poly3 || Subject) +
                  (poly1+poly2+poly3 | Subject:Condition),
                family = "binomial",
                data=TargetFix.gca)
summary(m.full.zcp)

# remove cubic ranefs
m.full.no_cube <- glmer(cbind(sumFix, N-sumFix) ~ 
                      (poly1+poly2+poly3)*Condition +
                      (poly1+poly2+poly3 || Subject) +
                      (poly1+poly2 | Subject:Condition),
                    family = "binomial",
                    data=TargetFix.gca)
summary(m.full.no_cube)

sjPlot::tab_model(m.full, m.full.no_cube)

# left ranef
m.left <- glmer(cbind(sumFix, N-sumFix) ~ 
                          (poly1+poly2+poly3)*Condition +
                          (((poly1+poly2)*Condition) || Subject),
                        family = "binomial",
                        data=TargetFix.gca)
summary(m.left)

sjPlot::tab_model(m.full, m.left)

# plot model fit
ggplot(TargetFix, aes(Time, meanFix, color=Condition)) +
  stat_summary(fun.data = mean_se, geom="pointrange") +
  stat_summary(aes(y=fitted(m.left)), fun=mean, geom="line") +
  expand_limits(y=c(0,1)) +
  labs(x="Time since word onset (ms)", y="Fixation proportion")
