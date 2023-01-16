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

m_full <- lmer(Score ~ Group + 
                 (1 | Anonymous_Subject_ID) + 
                 (1 + Group | Domain), 
               data=group_tx, REML=FALSE)
# doesn't converge
# de-correlate by-Domain random effects
m_zcp <- lmer(Score ~ Group + 
                (1 | Anonymous_Subject_ID) + 
                (1 | Domain) + (0 + Group | Domain), 
               data=group_tx, REML=FALSE)
# doesn't converge
# random intercepts are more important than random slopes, so drop random slope
m_grp <- lmer(Score ~ Group + 
                (1 | Anonymous_Subject_ID) + 
                (1 | Domain), 
              data=group_tx, REML=FALSE)
summary(m_grp)

as.data.frame(effect("Group", m_grp)) %>% 
  ggplot(aes(Group, fit, ymax = upper, ymin=lower)) + geom_pointrange()
as.data.frame(effect("Group", m_grp)) %>% 
  ggplot(aes(Group, fit, ymax = fit+se, ymin=fit-se)) + geom_pointrange()

# how about pairwise comparisons between groups?

contrast.matrix <- rbind(
  "A-B" = c(0,1,0,0),
  "A-C" = c(0,0,1,0),
  "A-W" = c(0,0,0,1),
  "B-C" = c(0,-1,1,0),
  "B-W" = c(0,-1,0,1),
  "C-W" = c(0,0,-1,1) 
)
library(multcomp)
pair_comps <- glht(m_grp, contrast.matrix)
summary(pair_comps, test = adjusted("none"))

#############
dat <- read_csv(url("https://osf.io/8bj9h/download"))
ggplot(dat, aes(txthm, accuracy, fill=hilo)) + 
  stat_summary(fun=mean, geom="col", position="dodge")

ggplot(subset(dat, accuracy==1), aes(txthm, rt, fill=hilo)) + 
  stat_summary(fun=mean, geom="col", position="dodge")

load("C:/Users/dan/OneDrive - University of Edinburgh/TEACHING/MLR/data/lab5.RData")
summary(dat5)
ggplot(dat5, aes(Domain, Score, color=Group)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  coord_flip()
mod_grp <- lmer(Score ~ Group + 
                  (1 | Anonymous_Subject_ID) + 
#                  (1 | Group) +
                  (0 + Group | Domain), 
                data=dat5, REML=FALSE)
summary(mod_grp)

group_tx <- ungroup(dat5)
save(group_tx, file="./data/group_tx.RData")

####### data from Geller et al. semantic relatedness judgment study
# participants saw pairs of words with different kinds of semantic relationships: taxonomic vs. thematic, high relatedness vs low relatedness

sem_rel <- read_csv(url("https://osf.io/pwnf8/download"))
summary(sem_rel)
ggplot(sem_rel, aes(txthm, rt, fill=hilo)) + 
  geom_boxplot() # note the outliers

m_s <- lmer(rt ~ txthm*hilo + (txthm*hilo | subject), data=sem_rel)
summary(m_s)

# can use model residuals to trim outliers
m_dat <- augment(m_s)
summary(m_dat)

m_trim <- lmer(rt ~ txthm*hilo + (txthm*hilo | subject), 
               data=subset(m_dat, abs(.resid) < 3*sd(.resid)))
summary(m_trim)