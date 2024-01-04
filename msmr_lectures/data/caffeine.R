# generating some data for examples: effect of caffeine on processing speed
# referencepoints: 
#   UK Biobank shows about 13ms improvement if caffeine consumed within last hour: 550ms vs 543ms, SD=100 (Cornelis, M. C., Weintraub, S., & Morris, M. C. (2020). Recent caffeine drinking associates with cognitive function in the UK Biobank. Nutrients, 12(7), 1969.)
#   effect is larger for older adults than younger adults (Rees, K., Allen, D., & Lader, M. (1999). The influences of age and caffeine on psychomotor and cognitive function. Psychopharmacology, 145, 181-188.)

library(tidyverse)
library(lme4)
N <- 30

# between-subjects
set.seed(8675309)
caff_lm <- data.frame(Participant = seq(1, N*2),
                      Caffeine = rep(c(TRUE, FALSE), each=N),
                      RT = c(540 + rnorm(N, sd=15), 553 + rnorm(N, sd=15)))
t.test(RT ~ Caffeine, data=caff_lm)
summary(lm(RT ~ Caffeine, data=caff_lm))

# within-subjects
set.seed(90210)
caff_mlm <- data.frame(Participant = seq(1, N),
                       Caffeine = 540 + rnorm(N, sd=15)) %>% 
  mutate(NoCaffeine = Caffeine + 13 + rnorm(N, sd=15)) %>% 
  pivot_longer(2:3, names_to = "Condition", values_to = "RT")

t.test(RT ~ Condition, data=caff_mlm)
t.test(RT ~ Condition, data=caff_mlm, paired=TRUE)
summary(lmer(RT ~ Condition + (1 | Participant), data=caff_mlm))

# 2x2 age and caffeine
caff_age <- data.frame(Participant = seq(1, N*2),
                       Age = rep(c("Younger", "Older"), each=N),
                       Caffeine = c(540 + rnorm(N, sd=25),
                                    550 + rnorm(N, sd=25))) %>% 
  mutate(NoCaffeine = Caffeine + 13 + 
           c(rep(0, N), 20 + rnorm(N, sd=20)) +
           rnorm(N*2, sd=20)) %>%
  pivot_longer(3:4, names_to = "Condition", values_to = "RT")
summary(caff_age)
ggplot(caff_age, aes(Condition, RT, fill=Age)) + geom_boxplot()

caff_age %>% group_by(Condition, Age) %>% 
  summarize(RT = mean(RT))
summary(lmer(RT ~ Condition*Age + (1 | Participant), data=caff_age))

caff_age2 <- caff_age %>% 
  mutate(RT = RT + rnorm(120, sd=10))
caff_age_multiday <- rbind(caff_age, caff_age2)
summary(lmer(RT ~ Condition*Age + (1 + Condition | Participant), data=caff_age_multiday))

save(caff_lm, caff_mlm, caff_age, caff_age_multiday, file="./data/caff_age.rda")
