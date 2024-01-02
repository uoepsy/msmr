library(tidyverse)
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

