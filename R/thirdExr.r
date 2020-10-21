library(MASS)
attach(survey)
mean(Height)
var(Height,na.rm=T)
sd(Height,na.rm=T)^2
y = Height
quantile(Height,na.rm=T)
quantile(Height,0.99,na.rm=T)
hist(y)
boxplot(y)


m.m = mean(Height[Sex=="Male"],na.rm=T)
s.m = sd(Height[Sex=="Male"],nar.rm=T)

#p = sum (m - s < Height & Height>)

#zad*
HM = Height[Sex=="Male"]
hist(HM)
lines(h$mids,H&count)
hist(HM)

lines(density(HM,na.rm=T,col=red))
#hist(HM.probability=T,na.rm=T)


#pulse
hist(Pulse)

#homedata
library(UsingR)
attach(homedata)
old = y1970
new = y2000
plot(old)
cor(y1970,y2000)
abline(lm(new~old),col="red")

