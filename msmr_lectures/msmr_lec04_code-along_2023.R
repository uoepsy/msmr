library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)

load("./data/group_tx.RData")
summary(group_tx)
# The data: 44 participants across 4 groups (between-subjects) were tested 5 times (waves) in 11 domains. In each wave of testing, each domain received a score on a 20-point scale and a set of several questions, which could be answered correctly or incorrectly.
ggplot(group_tx, aes(Domain, Score, color=Group)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  coord_flip()
# Looks like there are group differences and domain differences, but not much in the way of group-by-domain differences.

# Research question: test the overall group differences in performance
# Follow-up question: how about pairwise comparisons between groups?


####### example for using model residuals to trim outliers
# data from Geller et al. semantic relatedness judgment study
# participants saw pairs of words with different kinds of semantic relationships: taxonomic vs. thematic, high relatedness vs low relatedness

sem_rel <- read_csv(url("https://osf.io/pwnf8/download"))
summary(sem_rel)
ggplot(sem_rel, aes(txthm, rt, fill=hilo)) + 
  geom_boxplot() # note the outliers
