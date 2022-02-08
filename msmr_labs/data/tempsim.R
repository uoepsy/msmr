library(tidyverse)
library(lme4)
simdat<-function(nchild = 20, nobs = 7, b_x = 1.3, b_b = 2,b_bx=-1,bi=0){
    n_groups = nchild                         # number of groups
    N = nchild*nobs                              # total sample size
    g = rep(1:n_groups, e = N/n_groups)      # the group identifier
    x = rep(scale(1:7)[,1],nchild)
    b = rbinom(n_groups, size = 1, prob=.1)
    b = sample(c(2,2.4,2.5,5), size=n_groups, replace=T,prob = c(.15,.15,.35,.35))
    b = b[g]
    
    sd_g = 1  # standard deviation for the random intercept
    sd_x = .5 # standard deviation for the random slopes
    sigma = 2 # standard deviation for the observation
    
    re0 = rnorm(n_groups, sd = sd_g)  # random intercept
    re  = re0[g]
    rex = rnorm(n_groups, sd = sd_x)  # random effect
    re_x  = rex[g]
    lp = (bi + re) + (b_x + re_x)*x - (b_x + rnorm(N,0,.02))*0.3*I(x^2) + b_b*b + (b_bx*b*x)-(b_x + rnorm(N,0,.02))*0.1*I(x^2)*b
    
    ynorm = rnorm(N, mean = lp, sd = sigma) # continuous target variable
    ybin = rbinom(N, size = 1, prob = plogis(lp)) # binary target variable
    ypois = rpois(N, lambda=exp(lp)) # poisson target variable
    
    data.frame(ynorm,ybin,ypois,x,g = factor(g),b)
}
df=simdat(b_b=0,b_bx=-.7)
ggplot(ungroup(df), aes(x=x,y=ynorm,col=factor(b)))+
    stat_summary(geom="pointrange",aes(group=b,fill=b),alpha=.4)+
    geom_line(aes(group=g),alpha=.1)

tibble(
    schoolid = 1:30,
    b_bx = rnorm(30,-.5,1),
    b_x = rnorm(30, 1, 1),
    bi = rnorm(30,0,1),
    bb = rnorm(30,0,1)
) %>%
    mutate(
        df = pmap(list(b_x,b_bx,bi,bb), ~simdat(b_x=..1,b_bx=..2,b_b=..4,bi=..3))
    ) %>% select(schoolid,df) %>% unnest() ->df

ggplot(ungroup(df), aes(x=x,y=ynorm,col=factor(b)))+
    stat_summary(geom="pointrange",aes(group=b))+
    geom_line(aes(group=interaction(schoolid,g)),alpha=.1)+facet_wrap(~b)

# lmer(ynorm~poly(x,2)*b+(1+poly(x,2)|schoolid)+(1+poly(x,1)|schoolid:g),data=df %>% mutate(b=factor(b)),control=lmerControl(optimizer="bobyqa")) ->m
# VarCorr(m)
# summary(m)

df %>% ungroup %>% 
    arrange(schoolid,g,x) %>%
    transmute(
    schoolid=paste0("school",schoolid),
    ABS = scale(ynorm)[,1],
    year = rep(12:18,n()/7),
    #year = x,
    childid = paste0("child",g),
    home_loc = factor(b)
) -> df2

ggplot(ungroup(df2), aes(x = year, y = ABS,col=home_loc)) +
    stat_summary(geom="pointrange",aes(group=home_loc),alpha=.6)+
    geom_line(aes(group=interaction(schoolid,childid)),alpha=.1)+facet_wrap(~home_loc)

ggplot(ungroup(df2), aes(x = year, y = ABS,col=home_loc)) +
    stat_summary(geom="pointrange",aes(group=home_loc),alpha=.6)+
    stat_summary(geom="line",aes(group=home_loc),alpha=.6)

levels(df2$home_loc)<-c("rural","village","suburban","urban")

df2 %>% jkr::qsample(.,500,schoolid,childid) %>%
     sample_n(.,size=n()*.95) %>%
     write_csv("../../data/schoolsabs.csv")





