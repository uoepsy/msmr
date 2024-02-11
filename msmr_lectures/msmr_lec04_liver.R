library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)

load("./data/group_tx.RData")
summary(group_tx)
# The data: 44 participants across 4 groups (between-subjects) were tested 5 times (waves) in 11 domains. In each wave of testing, each domain received a score on a 20-point scale and a set of several questions, which could be answered correctly or incorrectly.
ggplot(group_tx, aes(Wave, Score, color=Domain)) +
  facet_wrap(~ Group, scales="free") +
  stat_summary(fun.data = mean_se, geom="pointrange") + 
  stat_smooth(method = "lm", se=F)

# make an meaningful intercept of Wave
group_tx$Time <- group_tx$Wave-1
# fit a model
m <- lmer(Score ~ Group*Time +
            (Time | Anonymous_Subject_ID) +
            (Time | Domain),
          data=group_tx, REML = F)
summary(m)
coef(summary(m))

# what about other pairwise comparisons?
summary(group_tx)
# relevel to set group B as reference and estimate effects relative to it
group_tx$Group <- relevel(as.factor(group_tx$Group), "B")
mb <- lmer(Score ~ Group*Time +
            (Time | Anonymous_Subject_ID) +
            (Time | Domain),
          data=group_tx, REML = F)
summary(mb)
coef(summary(mb))

# can estimate pairwise comparisons from a single model
# emmeans... not sure how it is supposed to work
library(emmeans)
emmeans::contrast(m, "Group")

# multcomp...
library(multcomp)
# specify contrast vector(s)
ctrst <- rbind("B vs C" = c(0,0,0,0,0,1,-1,0),
                "B vs W" = c(0,0,0,0,0,1,0,-1),
                "C vs W" = c(0,0,0,0,0,0,1,-1))
summary(glht(m, ctrst))
