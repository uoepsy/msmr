library(gridExtra)
wis.sub <- subset(wisqars.suicide, Year <= 2001 & State %in% c("New Mexico", "Vermont", "Pennsylvania", "Oklahoma"), select=c("State", "Year", "Crude.Rate"))

p1 <- ggplot(wis.sub, aes(Year, Crude.Rate, color=State, shape=State)) + 
  geom_point(size=3) + stat_smooth(aes(group=NA), method="lm", se=F) + 
  scale_x_continuous(breaks=1999:2001) + expand_limits(x=c(1997,2003)) +
  theme_bw() + theme(legend.position="none", axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + labs(x="", y="") +
  ggtitle("No random effects (OLS)")

m.wis <- lmer(Crude.Rate ~ Year + (1 | State), data=wis.sub)
p2 <- ggplot(fortify(m.wis), aes(Year, Crude.Rate, color=State, shape=State)) + 
  geom_point(size=3) + stat_smooth(aes(y=.fitted), method="lm", se=F) + 
  scale_x_continuous(breaks=1999:2001) + expand_limits(x=c(1997,2003)) +
  theme_bw() + theme(legend.position="none", axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + labs(x="", y="") +
  ggtitle("Random intercepts (1 | Subject)")

p3 <- ggplot(wis.sub, aes(Year, Crude.Rate, color=State, shape=State)) + 
  geom_point(size=3) + stat_smooth(method="lm", se=F) + 
  scale_x_continuous(breaks=1999:2001) + expand_limits(x=c(1997,2003)) +
  theme_bw() + theme(legend.position="none", axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank()) + labs(x="", y="") +
  ggtitle("Full random effects (1 + Time | Subject)")

g <- grid.arrange(p1, p2, p3, nrow=1)

ggsave("RandomEffectsLinDemo.png", g, width = 11, height = 4)
