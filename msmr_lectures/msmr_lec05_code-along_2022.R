library(tidyverse) # for everything
# for fitting and visualising models
library(lme4) 
library(lmerTest)
library(broom.mixed) 
library(effects)
# for factor analysis
library(psych)
library(GPArotation)

############
# Stern, J., Gerlach, T. M., & Penke, L. (2020). Probing ovulatory-cycle shifts in women's preferences for men's behaviors. Psychological Science, 31(4), 424-436. https://doi.org/10.1177/0956797619882022 

#  112 female participants were tested 4 times (2 in fertile phase, 2 in luteal phase). 
# Each time, they rated 70 males for short-term sexual attractiveness ("Rating") and long-term relationship attractiveness ("longterm_rating") on a 11-point scale: -5 (extremely unattractive), 0 (neutral), 5 (extremely attractive). 
# Males were shown in a 30-sec video interacting with an attractive female confederate. These videos were separately rated on various behavioural dimensions and factor analysis was used to define two composite scores: competitiveness and courtship

# Research question: do women's preferences for men displaying competitive and courtship behaviours change across the ovulatory cycle?

# Research question: do women's preferences for men displaying behavioral competitive and courtship behaviours change across the ovulatory cycle?

# read in data
ovu_dat <- read_csv2(url("https://osf.io/e4yxb/download")) %>% 
  mutate(Fertility = as_factor(Fertility))

# use factor analysis to define composite scores of male behavior
male_behav <- read_csv2(url("https://osf.io/hvgfz/download")) 
EFA <- male_behav %>% select("v150atr","v150flm","v150sel", "M_gaze", 
                             "arrogance", "assertiveness","confrontative",
                             "dominance","fight","respected") %>% 
  fa(., nfactors = 2, fm="pa", rotate="oblimin", scores = "regression", 
     use="complete.obs", max.iter=50)
male_efa <- data.frame(MaleID = male_behav$MaleID, 
                       competitiveness = EFA$scores[, 1], 
                       courtship = EFA$scores[, 2])
# merge the behavior scores with the main data
ovu_dat_efa <- merge(ovu_dat, male_efa) %>% 
  select(MaleID, Person, Fertility, Rating, longterm_rating, competitiveness, courtship)
summary(ovu_dat_efa)

# start with a basic model of effect of fertility
f_st <- lmer(Rating ~ Fertility + 
               (Fertility | Person) + (1 | MaleID), 
             data = ovu_dat_efa)
summary(f_st)
f_lt <- lmer(longterm_rating ~ Fertility + 
               (Fertility | Person) + (1 | MaleID), 
             data = ovu_dat_efa)
summary(f_lt)
# for both short-term and long-term relationships, there is a statistically significant effect of ovulatory cycle: women in fertile phase rate mean approx 0.1 points more attractive on a 11-point scale (approx 1%)
# visualise effect
f1 <- as.data.frame(effect("Fertility", f_st)) %>% 
  mutate(Relationship = "Short-term")
f2 <- as.data.frame(effect("Fertility", f_lt)) %>% 
  mutate(Relationship = "Long-term")
rbind(f1, f2) %>% 
  ggplot(aes(Relationship, fit, color=Fertility)) + 
  geom_pointrange(aes(ymin=fit-se, ymax=fit+se), position = position_dodge(width=0.1)) +
  expand_limits(y=c(-5, 5)) +
  labs(y="Attractiveness rating")

# add effects of competitiveness
compet_st <- lmer(Rating ~ Fertility * competitiveness + 
                    (Fertility | Person) + (0+competitiveness | Person) +
                    (1 | MaleID), 
                  data = ovu_dat_efa)
summary(compet_st)

compet_lt <- lmer(longterm_rating ~ Fertility * competitiveness + 
                    (Fertility | Person) + (0+competitiveness | Person) +
                    (1 | MaleID), 
                  data = ovu_dat_efa)
summary(compet_lt)
# main effects of competitiveness for both short- and long-term relationships, no interaction with fertility
compet1 <- as.data.frame(effect("Fertility:competitiveness", compet_st)) %>% 
  mutate(Relationship = "Short-term")
compet2 <- as.data.frame(effect("Fertility:competitiveness", compet_lt)) %>% 
  mutate(Relationship = "Long-term")

rbind(compet1, compet2) %>% 
  ggplot(aes(competitiveness, fit, color=Fertility)) + 
  facet_wrap(~ Relationship) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Fertility), alpha=0.5, color=NA) +
  expand_limits(y=c(-5, 5)) +
  labs(y="Attractiveness rating")

# courtship behaviour
court_st <- lmer(Rating ~ Fertility * courtship + 
                   (Fertility | Person) + (0+courtship | Person) +
                   (1 | MaleID), 
                 data = ovu_dat_efa)
summary(court_st)

court_lt <- lmer(longterm_rating ~ Fertility * courtship + 
                   (Fertility | Person) + (0+courtship | Person) +
                   (1 | MaleID), 
                 data = ovu_dat_efa)
summary(court_lt)
# main effects of courtship for both short- and long-term relationships, no interaction with fertility
court1 <- as.data.frame(effect("Fertility:courtship", court_st)) %>% 
  mutate(Relationship = "Short-term")
court2 <- as.data.frame(effect("Fertility:courtship", court_lt)) %>% 
  mutate(Relationship = "Long-term")

rbind(court1, court2) %>% 
  ggplot(aes(courtship, fit, color=Fertility)) + 
  facet_wrap(~ Relationship) +
  geom_line() +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Fertility), alpha=0.5, color=NA) +
  expand_limits(y=c(-5, 5)) +
  labs(y="Attractiveness rating")

# follow-up individual differences question: at the group level, we're seeing very similar patterns of short-term and long-term attractiveness ratings, is this also true at the individual level. For example, we saw a small effect of fertility on both kinds of attractiveness, are those effects of fertility correlated across individual participants
summary(f_st)

source(url("https://uoepsy.github.io/msmr/functions/get_ranef.R"))
ran_st <- get_ranef(f_st, "Person") %>% 
  rename(Fertility_ST = "Fertility1") %>% 
  select(-Intercept)
summary(ran_st)
ran_lt <- get_ranef(f_lt, "Person") %>% 
  rename(Fertility_LT = "Fertility1") %>% 
  select(-Intercept)
ran <- merge(ran_st, ran_lt)
summary(ran)
cor.test(ran$Fertility_ST, ran$Fertility_LT)
ggplot(ran, aes(Fertility_ST, Fertility_LT)) + geom_point()
