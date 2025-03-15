library(pwr)
library("ggplot2")



power.t.test(power=.80, sig.level=.05, delta=.5, sd=1,type = "two.sample")

power.t.test(n=64, sig.level=.05, delta=.5, sd=1, type = "two.sample")
#paired
power.t.test(power=.80, sig.level=.05, delta=0.527, sd=1,type = "paired")


N<-seq(20,140,by=20)

dat<-data.frame(n=N)
for (i in seq(N)) {
  one.power<-power.t.test(power=.80, sig.level=.05, sd=1, n=N[i])
  dat$d[i]<-one.power$delta
}

ggplot(dat,aes(y=d,x=n, label = round(d,digits = 2))) +
  stat_smooth(method = "loess",se=FALSE) +
  labs(x="Totale sample size",y="Effect size d") +
  scale_x_continuous(breaks=seq(min(N),max(N),by=20))+
  geom_label()




