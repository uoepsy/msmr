library(tidyverse)
library(broom)
library(lme4)
library(lmerTest)

# for week 1 live coding: a 2x2 repeated-measures anova in MLM framework

########## data from Fazio (2020) Repetition Increases Perceived Truth Even for Known Falsehoods
# article: https://doi.org/10.1525/collabra.347
# data: https://osf.io/kt2p8/
# participants had to determine whether statements were true or false. the manipulations were whether the fact was known or unknown to the participant and whether the participant had been previously exposed to the statement
# we know prior exposure increases "truthiness" of false statements, key question is whether prior knowledge is a protective factor. that is, does *knowning* that the statement is false protect people from the repetition effect

falsehood <- read_csv(url("https://osf.io/skmw5/download")) %>% 
  select(c(1,12:19)) %>% 
  pivot_longer(cols=2:9) %>% #
  separate(name, c("Knowledge", "Repetition", "Truth"))

ggplot(falsehood, aes(Repetition, value, fill=Knowledge)) + 
  facet_wrap(~ Truth) +
  geom_violin()

m_false <- lmer(value ~ Knowledge*Repetition + (Knowledge + Repetition | Subject), 
                data=subset(falsehood, Truth == "false"))
summary(m_false)

########## data from Van Engen et al. (2020) Effects of age, word frequency, and noise on the time course of spoken word recognition
pupil <- read_csv(url("https://osf.io/e7c8g/download"))
summary(pupil)
pupil_behav <- pupil %>% select(Subject, Age, Group, SNR, Experiment, Condition, ClickS) %>% 
  group_by(Subject, Group, SNR, Condition) %>% 
  summarise(RT = unique(ClickS))
summary(pupil_behav)
ggplot(pupil_behav, aes(Group, RT, fill=Condition)) + 
         facet_wrap(~ SNR) + geom_violin()

m_s <- lmer(RT ~ Group*SNR*Condition + (1 | Subject), data=pupil_behav)
summary(m_s)
# can use model residuals to trim outliers
m_dat <- augment(m_s)
summary(m_dat)

m_trim <- lmer(RT ~ Group*SNR*Condition + (1 | Subject), 
               data=subset(m_dat, abs(.resid) < 3*sd(.resid)))
summary(m_trim)

####### data from Geller et al. semantic relatedness judgment study
# participants saw pairs of words with different kinds of semantic relationships: taxonomic vs. thematic, high relatedness vs low relatedness

sem_rel <- read_csv(url("https://osf.io/pwnf8/download"))
summary(sem_rel)
ggplot(sem_rel, aes(txthm, rt, fill=hilo)) + 
  geom_violin() + # note the outliers
  stat_summary(fun.data=mean_se, geom="pointrange", position = position_dodge(width=0.9))

m_s <- lmer(rt ~ txthm*hilo + (txthm*hilo | subject), data=sem_rel)
summary(m_s)

# can use model residuals to trim outliers
m_dat <- augment(m_s)
summary(m_dat)

m_trim <- lmer(rt ~ txthm*hilo + (txthm*hilo | subject), 
               data=subset(m_dat, abs(.resid) < 3*sd(.resid)))
summary(m_trim)