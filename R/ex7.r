x = rnorm(20,3,2)
m = mean(x)
q = pnorm(0.95,0,1)
#a)
m+q*sqrt(4/20)
m-q*sqrt(4/20)
#b)
s.2 = var(x)
m+q*sqrt(s.2/20)
m-q*sqrt(s.2/20)

t.test(x,conf.level=0.95)


#2
x = scan()
t.test(x)
t.test(x,conf.level=0.90)

#3
library(UsingR)
rat
boxplot(rat,horizontal = T)
qqnorm(rat)
qqline(rat)
t.test(rat,conf.level = 0.96)

#b
#ne sa norm razprl
wilcox.test(exec.pay,conf.level = 0.96)

#4
#binomno razpredelenie!
prop.test(87,150,0.92)#87 ot 150 kazvat da
prop.test(870,1500,0.92)#870 ot 150
prop.test(8700,15000,0.92)


#6
attach(survey)
Sex
Smoke
table(Sex,Smoke)
prop.test(89,117,conf.level=0.9)#89 smoers of 117
