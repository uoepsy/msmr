# Week 2 code-along
library(tidyverse)
library(lme4)
library(effects)

# Part 1: logistic MLM
# test-enhanced learning
load("./data/TestEnhancedLearning.RData")
summary(tel)
ggplot(tel, aes(Delay, Correct, fill=Group)) +
  stat_summary(fun=mean, geom="col", position = "dodge")

m.tel <- glmer(Correct ~ Group*Delay + (Delay | Subject_ID), 
               data=tel, family = "binomial")
summary(m.tel)

ef.tel <- as.data.frame(effect("Group:Delay", m.tel))
ef.tel
ggplot(ef.tel, aes(Delay, fit, color=Group)) +
  geom_pointrange(aes(ymin=lower, ymax=upper), 
                  position=position_dodge(width=0.2))

# another example: cross-cultural bouba/kiki effect
# Cwiek, et al. (2022). The bouba/kiki effect is robust across cultures and writing systems. Philosophical Transactions of the Royal Society B, 377(1841), 20200390.
kiki <- read_csv('https://osf.io/4psw7/download')
# Match outcomes for 917 participants across 25 languages that fall into 9 families, with roman and non-roman scripts
table(kiki$Family, kiki$Script)

# a simple logistic model
kiki_mdl <- glmer(Match ~ 1 + Order + Script +
                    (1 + Order | Language) + (1 + Order | Family),
                  data = kiki,
                  family = binomial(link = 'logit'))
summary(kiki_mdl)

# simplify RE by removing extraneous ones
# use sum contrasts to get main effects
kiki_mdl <- glmer(Match ~ 1 + Order + Script + (1 |Language), 
                  contrasts = list(Order=contr.sum, Script=contr.sum),
                  data = kiki, 
                  family = binomial(link = 'logit'))
summary(kiki_mdl)

# plot estimated effects
kiki_ef <- as.data.frame(Effect(c("Order", "Script"), kiki_mdl))
ggplot(kiki_ef, aes(Script, fit, colour=Order, ymin=lower, ymax=upper)) +
  geom_pointrange(position=position_dodge(width = 0.2)) +
  geom_hline(yintercept = 0.5, linetype="dashed") +
  expand_limits(y=c(0,1)) +
  labs(y="Proportion Match")