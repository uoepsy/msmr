# Week 3 code-along
# Re-analyze `TargetFix` data using logistic MLM (logistic GCA)

library(tidyverse)
library(lme4)
source("https://uoepsy.github.io/msmr/functions/code_poly.R")
load(url("https://uoepsy.github.io/msmr/data/TargetFix.rda"))
summary(TargetFix)

ggplot(TargetFix, aes(Time, meanFix, color=Condition)) +
  stat_smooth()

# create 3rd-order orth poly
tf.gca <- code_poly(TargetFix, predictor = "Time", 
                    poly.order = 3)
str(tf.gca)
ggplot(tf.gca, aes(poly1, meanFix, color=Condition)) +
  stat_smooth()

# fit maximal model
m <- glmer(cbind(sumFix, N-sumFix) ~ (poly1+poly2+poly3)*Condition +
             (poly1+poly2+poly3 | Subject) +
             (poly1+poly2+poly3 | Subject:Condition),
           family="binomial", data=tf.gca)
summary(m)

# remove by-subject random covars b/c not estimated
m.s <- glmer(cbind(sumFix, N-sumFix) ~ (poly1+poly2+poly3)*Condition +
             (poly1+poly2+poly3 || Subject) +
             (poly1+poly2+poly3 | Subject:Condition),
           family="binomial", data=tf.gca)
summary(m.s)

# remove 3rd-order random slopes
m.ss <- glmer(cbind(sumFix, N-sumFix) ~ (poly1+poly2+poly3)*Condition +
               (poly1+poly2 || Subject) +
               (poly1+poly2 | Subject:Condition),
             family="binomial", data=tf.gca)
summary(m.ss)

# plot model fit
ggplot(tf.gca, aes(Time, meanFix, color=Condition)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(fun=mean, aes(y=fitted(m.ss)), geom="line") +
  expand_limits(y=c(0,1))

VarCorr(m)
VarCorr(m.s)
tf <- code_poly(TargetFix, predictor = "timeBin", 
                poly.order = 3, orthogonal = F, draw.poly = F)
