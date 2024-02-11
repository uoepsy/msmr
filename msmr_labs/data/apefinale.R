
PCOG ~ age + enclosure_size * SUBDOM + 
  (1 + age + SUBDOM | zoo) + 
  (1 + age | zoo:id)

simzoo <- function(seed=NULL,b1,b2,b3,z0,z1,e){
  if(!is.null(seed)){
    set.seed(seed)
  }
  n_groups = round(runif(1,4,20))
  npg = round(runif(n_groups,2,9))
  g = unlist(sapply(1:n_groups, function(x) rep(x, npg[x])))
  x = unlist(sapply(1:n_groups, function(x) 1:npg[x]))
  b = rbinom(n_groups,1,prob=.4)
  b = rnorm(n_groups)
  b = b[g]
  re0 = rnorm(n_groups, sd = z0)
  re  = re0[g]
  rex = rnorm(n_groups, sd = z1)
  re_x  = rex[g]
  lp = (b1 + re) + (b2+re_x)*x + b3*b
  y = rnorm(length(g), mean = lp, sd = e) # create a continuous target variable
  # y_bin = rbinom(N, size = 1, prob = plogis(lp)) # create a binary target variable
  data.frame(x, b, g=factor(g), y)
}

many = tibble(
  zoo = 1:13,
  enc = rnorm(13),
  int = rnorm(13),
  slx = rnorm(13),
  sd = rnorm(13,-2) + enc,
  z0 = runif(13,.5,2),
  z1 = runif(13,.5,2),
  e = runif(13,.5,2)
) |> mutate(
    data = pmap(list(int,slx,sd,z0,z1,e), ~simzoo(b1=..1,b2=..2,b3=..3,z0=..4,z1=..5,e=..6))
  ) |> unnest(data)

lmer(y ~ x + b * enc + 
       (1 + x + b | zoo) +
       (1 + x | zoo:g),
     data = many) |> 
  summary()
  #sjPlot::plot_model(type="int")


# add in zoo location
# add in ape nationality


PCOG ~ age + enclosure_size * SUBDOM + 
  (1 + age + SUBDOM | zoo) + 
  (1 + age | zoo:id)
