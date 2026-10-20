require(tidyverse)
set.seed(11)
Ngroups = 20
NperGroup = round(rnorm(Ngroups, 7,1))
N = sum(NperGroup)
groups = map(seq_along(NperGroup), ~rep(.,NperGroup[.])) %>% unlist() %>% factor
re_int = rnorm(Ngroups, sd = .3)
re_slope = rnorm(Ngroups, sd = .5)
dd<-MASS::mvrnorm(n=Ngroups, mu = c(0,0), Sigma = matrix(c(3,.1,0.1,1),byrow = T, nrow=2))
x = rnorm(N,4,1)
x2 = rnorm(N,8,2)
e = rnorm(N, sd = 2)
y = -2 + dd[groups,1] + (1.07 + dd[groups,2])*x + (.5 * x2) + e
d = data.frame(x,x2, y, groups)

ggplot(d,aes(x=x,y=y,col=factor(groups)))+
    geom_point()+
    geom_smooth(method="lm",se=F)+
    guides(col=FALSE)

lm(y~x+x2,d) %>% summary

require(lme4)
m1<-lmer(y~x+(1+x|groups), d)
summary(m1)
dotplot.ranef.mer(ranef(m1))

broom.mixed::augment(m1) %>%
    ggplot(.,aes(x=x,y=.fitted,group=groups))+
    geom_line()

lmer(y~x+x2+(1+x|groups), d) %>% summary()


read_csv("toys.csv") %>%
    fill(type) %>%
    group_by(groups) %>%
    mutate(toyn = 1:n()) %>%
left_join(.,
          d %>% group_by(groups) %>%
              mutate(toyn = 1:n()) %>% ungroup %>%
              mutate(groups = as.numeric(as.character(groups)))
) %>% ungroup %>%
    select(-groups, -toyn) %>%
    rename(R_AGE = y, hrs_week = x, age = x2, toy_type = type) %>%
    sample_n(n()) -> toydata

write.csv(toydata, "data/toyexample.csv",row.names=F)







 

