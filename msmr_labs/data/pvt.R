require(tidyverse)
require(lme4)

simm2<-function(seed=NULL,b0=0,b1=1,b2=-.2,z0=1,z1=1,e=1){
  if(!is.null(seed)){
    set.seed(seed)
  }
  n_groups = round(runif(1,4,30))
  npg = 5
  g = rep(1:n_groups, e = 5)      # the group identifier
  x = rep(1:5,n_groups)
  re0 = rnorm(n_groups, sd = z0)  # random intercepts
  re  = re0[g]
  rex = rnorm(n_groups, sd = z1)  # random effects
  re_x  = rex[g]
  lp = (b0 + re) + (b1 + re_x)*poly(x,2)[,1] + (b2)*poly(x,2)[,2]
  y = rnorm(length(g), mean = lp, sd = e) # create a continuous target variable
  # y_bin = rbinom(N, size = 1, prob = plogis(lp)) # create a binary target variable
  data.frame(x, g=factor(g), y)
}


  big = tibble(
    school = 1:30,
    b = rep(0:1,e=15),
    int = rnorm(30),
    sl = rnorm(30,7,1),
    qu = rnorm(30,-2,.3),
    z0 = runif(30,.5,2),
    z1 = runif(30,.5,2),
    e = runif(30,.5,2)
  )
  big = big |> mutate(
    int= ifelse(b==1,int-1,int),
    qu = ifelse(b==1,qu+4,qu),
    data = pmap(list(int,sl,qu,z0,z1,e), ~simm2(b0=..1,b1=..2,b2=..3,z0=..4,z1=..5,e=..6))
  ) |> unnest(data)
  
  m = lmer(y~poly(x,2)*b+(1+poly(x,1)|school/g),big)
  summary(m)
  fm = broom::augment(m)
  fm$x = round(fm$`poly(x, 2)`[,1],3)
  fm |> ggplot(aes(x=x,y=.fitted,col=factor(b)))+
    stat_summary(geom="line",aes(group=school))
  
ggplot(big, aes(x=x,y=y,col=factor(b)))+
  geom_line(aes(group=g))+
  facet_wrap(~school)

childnames = randomNames::randomNames(5e2,which.names="first")
bigwrite = big |> transmute(
  child = childnames[as.numeric(paste0(school,g))],
  school=paste0("School ", school),
  isBilingual = b,
  year = x+7,
  PVT = round(pmin(60,pmax(0,scale(y)[,1]*9.75 + 27.4)))
)

lmer(PVT~poly(year,2)*isBilingual+(1+poly(year,1)|school/child),bigwrite) |>
  summary()

hist(bigwrite$PVT)
write_csv(bigwrite,"data/pvt_bilingual.csv")

