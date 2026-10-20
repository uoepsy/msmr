gconf = function(){
  N = 100
  n_groups = 20
  g = rep(1:n_groups, e = N/n_groups)
  u = rnorm(n_groups,0,1)[g]
  x = rnorm(N,u)
  
  re = MASS::mvrnorm(n_groups, mu=c(0,0),Sigma=matrix(c(1,0.5,0.5,1),ncol=2))
  re1 = re[,1][g]
  re_x = re[,2][g]
  lp = (0 + re1) + (1 + re_x) * x + -10*u
  y = rnorm(N, mean = lp, sd = 1)
  
  df = data.frame(x, g = factor(g), y)
  ggplot(df,aes(x=x,y=y,col=g))+
    geom_point()+guides(col="none")+
    geom_smooth(method=lm,se=F)
  
  c(
    ri=fixef(lmer(y~1+x+(1|g),df))['x'],
    rs=fixef(lmer(y~1+x+(1+x|g),df))['x'],
    mui=fixef(lmer(y~1+x+xm+(1|g),df |> group_by(g) |>mutate(xm=mean(x))))['x'],
    mu=fixef(lmer(y~1+x+xm+(1+x|g),df |> group_by(g) |>mutate(xm=mean(x))))['x'],
    mwb=fixef(lmer(y~1+xd+xm+(1+xd|g),df |> group_by(g) |>mutate(xm=mean(x),xd=x-xm)))['xd']
  )
}

res = t(replicate(500,gconf()))
par(mfrow=c(3,2))
hist(res[,1]);hist(res[,2]);hist(res[,3]);hist(res[,4]);hist(res[,5])
par(mfrow=c(1,1))
colMeans(res)
apply(res,2,sd)
as_tibble(res) |>
  pivot_longer(everything()) |> 
  mutate(value=value-1) |>
  ggplot(aes(x=name,y=value,col=name))+
  #geom_jitter(height=0,width=.2)+
  stat_summary(geom="pointrange",position=position_nudge(x=.25))
  # ggplot(aes(x=value,col=name))+
  # geom_density()
 
