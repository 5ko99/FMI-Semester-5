#Simple R Book
library(MASS)
attach(survey)
t1 = table(Sex,Smoke)
table("Man",Smoke)
sum(t1[2,-2])

mean(Height[Sex=="Male"],na.rm=TRUE)
p=order(Age)[1:6]

t=table(Smoke)
prop.table(t)

t2=table(Sex,Smoke)
prop.table(t2)

p=factor(Smoke, levels=levels(Smoke)[c(2,3,4,1)])
barplot(table(p))
pie(table(p))
barplot(table(Sex,p),legend=TRUE,beside=TRUE)