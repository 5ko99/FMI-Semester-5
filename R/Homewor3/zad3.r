data = read.csv(file.choose(),sep=',',dec='.')
x1=data$x1
x2=data$x2
x3=data$x3
x4=data$x4
x5=data$x5
x6=data$x6

boxplot(x1,horizontal=T)
hist(x1)
qqnorm(x1)
qqline(x1)

boxplot(x2,horizontal=T)
hist(x2)
qqnorm(x2)
qqline(x2)

boxplot(x3,horizontal=T)
hist(x3)
qqnorm(x3)
qqline(x3)

boxplot(x4,horizontal=T)
hist(x4)
qqnorm(x4)
qqline(x4)

boxplot(x5,horizontal=T)
hist(x5)
qqnorm(x5)
qqline(x5)

boxplot(x6,horizontal=T)
hist(x6)
qqnorm(x6)
qqline(x6)

#proverka
t = table(cut(x1, breaks = c(0,2,4,6,12)))
prob = c(0,0,0,0)
prob[1] = pexp(2, 4) - pexp(0, 4)
prob[2] = pexp(4,4) - pexp(2,4)
prob[3] = pexp(6,4) - pexp(4,4)
prob[4] = 1 - pexp(6,4)
chisq.test(t,p=prob)
