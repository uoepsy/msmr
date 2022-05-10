# see video of action
# see gesture, hear speech
# respond "does speech OR gesture match action?"
library(tidyverse)
library(lme4)
krep <- read_csv("~/Desktop/kellyrep.csv")
krep %>% select(subject_nr, hand_resp, trial_nr, condition, item, prime, modality, strength, congruence, rtime, response, correct) -> krep

# are people slower/more error prone when responding to incongruent vs congruent stimuli
ggplot(krep,aes(x=congruence,y=rtime))+geom_boxplot()
ggplot(krep,aes(x=congruence,y=correct))+
  stat_summary(geom="pointrange")

library(forecast)
mod1 <- lmer(log(rtime)~congruence+(1|subject_nr)+(1+congruence|prime), data=krep)
summary(mod1)
hist(resid(mod1))
shapiro.test(resid(mod1))

mod1a <- glmer(correct~congruence+(1|subject_nr)+(1+congruence|prime), data=krep,family=binomial) -> mod1a
summary(mod1a)

# are does the strength of incongruence and the modality (speech or gesture) influence comprehension (via RT/error)

ggplot(krep,aes(x=strength,y=rtime,col=modality))+geom_boxplot()
ggplot(krep %>% filter(congruence!="congruent") %>%
         mutate(strength = fct_relevel(strength,"baseline","weak")),
       aes(x=strength,y=correct,col=modality))+
  stat_summary(geom="pointrange")+ 
  stat_summary(geom="line",aes(group=modality))

lmer(log(rtime)~modality*strength+(1+modality+strength|subject_nr)+(1 + modality|prime), data=filter(krep,congruence=="incongruent"),control=lmerControl(optimizer = "bobyqa")) -> mod2
summary(mod2)

glmer(correct~modality*strength+(1+modality+strength|subject_nr)+(1+modality|prime), data=filter(krep,congruence=="incongruent"),family=binomial,control=glmerControl(optimizer = "bobyqa")) -> mod2a
summary(mod2a)
