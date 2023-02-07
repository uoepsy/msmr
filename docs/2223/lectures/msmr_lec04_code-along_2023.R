library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(broom.mixed)

load(url("https://uoepsy.github.io/msmr/data/group_tx.RData"))

summary(group_tx)

# The data: 44 participants across 4 groups (between-subjects) were tested 5 times (waves) in 11 domains. In each wave of testing, each domain received a score on a 20-point scale and a set of several questions, which could be answered correctly or incorrectly.
ggplot(group_tx, aes(Domain, Score, color=Group)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  coord_flip()

ggplot(group_tx, aes(Wave, Score, color=Group)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_smooth(method="lm")

ggplot(group_tx, aes(Wave, Score, color=Domain)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_smooth(method="lm", se=F)

# Looks like there are group differences and domain differences, but not much in the way of group-by-domain differences.

# Research question: test the overall group differences in performance

m_gr <- lmer(Score ~ Wave*Group +
               (1 + Wave + Group | Domain) +
               (1 + Wave | Anonymous_Subject_ID),
             data=group_tx, REML=F)
summary(m_gr)

m_wa <- lmer(Score ~ Wave +
               (1 + Wave + Group | Domain) +
               (1 + Wave | Anonymous_Subject_ID),
             data=group_tx, REML=F)

m_wa_gr <- lmer(Score ~ Wave + Group +
               (1 + Wave + Group | Domain) +
               (1 + Wave | Anonymous_Subject_ID),
             data=group_tx, REML=F)

anova(m_wa, m_wa_gr, m_gr)

# Follow-up question: how about pairwise comparisons between groups?
tidy(m_gr)
coef(summary(m_gr))
library(multcomp)

h <- rbind("B-C" = c(0, 0, -1, 1, 0, 0, 0, 0),
           "B-W" = c(0, 0, -1, 0, 1, 0, 0, 0),
           "C-W" = c(0, 0, 0, -1, 1, 0, 0, 0),
           "Slope B-C" = c(0, 0, 0, 0, 0, -1, 1, 0),
           "Slope B-W" = c(0, 0, 0, 0, 0, -1, 0, 1),
           "Slope C-W" = c(0, 0, 0, 0, 0, 0, -1, 1))
comps <- glht(m_gr, h)
summary(comps, test=adjusted("none"))


####### example for using model residuals to trim outliers
# data from Geller et al. semantic relatedness judgment study
# participants saw pairs of words with different kinds of semantic relationships: taxonomic vs. thematic, high relatedness vs low relatedness

sem_rel <- read_csv(url("https://osf.io/pwnf8/download"))
summary(sem_rel)
ggplot(sem_rel, aes(txthm, rt, fill=hilo)) + 
  geom_boxplot() # note the outliers
