library(tidyverse)
library(broom)
library(lme4)
library(lmerTest)

# for week 1 live coding: a 2x2 repeated-measures anova in MLM framework

########## data from Fazio (2020) Repetition Increases Perceived Truth Even for Known Falsehoods
# article: https://doi.org/10.1525/collabra.347
# data: https://osf.io/kt2p8/
# participants had to determine whether statements were true or false. the manipulations were whether the fact was known or unknown to the participant and whether the participant had been previously exposed to the statement
# we know prior exposure increases "truthiness" of false statements, key question is whether prior knowledge is a protective factor. that is, does *knowing* that the statement is false protect people from the repetition effect

falsehood <- read_csv(url("https://osf.io/skmw5/download")) %>% 
  select(c(1,12:19)) %>% 
  pivot_longer(cols=2:9) %>% #
  separate(name, c("Knowledge", "Repetition", "Truth"))

ggplot(falsehood, aes(Repetition, value, fill=Knowledge)) + 
  facet_wrap(~ Truth) +
#  geom_violin()
  geom_boxplot()

m_false <- lmer(value ~ Knowledge*Repetition + (Knowledge + Repetition | Subject), 
                data=subset(falsehood, Truth == "false"))
summary(m_false)

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