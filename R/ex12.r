x1 = rnorm(25,4,2)
x2 = rnorm(25,3,3)
x3 = rnorm(25,2,5)

y=x1+5*x3+rnorm(25,0,0.4)

d = data.frame(y,x1,x2,x3)
str(d)
plot(d)

#tursim model y=Bo+B1X1+B2X2+B3X3
#H0: Bk=0
#H1: Bk!=0
#ako p-value <0.05 to oth
#ako p-value >=0.05 to mozhem da priemem H0

#dolnoto p-value to hip sa
#H0: vsqko B e = 0
#H1: nqkoe B e !=0
l=lm(y~x1+x2+x3)
summary(l)

#stroim 2 model
l2=lm(y~x1+x3-1)
summary(l2)
#nai dobriqt model
anova(l,l2)
#golqmo p-value -> mozhe da priemem che 2ta modela suvpadata
plot(l2)

#zad1
?mtcars
plot(mtcars)
l=lm(mpg~.,data=mtcars)
summary(l)

#best model
l2=lm(mpg~wt+cyl,data=mtcars)
summary(l2)


#zad2
f= read.table(file.choose(),header=TRUE,sep="",dec=".")
f = 2.54 * f
f
attach(f)
l = lm(Height~momheight+dadheight)
summary(l)
#adj R square 0.42 -> 42% se opredelq

l1 = lm(f$Height~f$momheight)
l2 = lm(f$Height~f$dadheight)
summary(l1)
summary(l2)

#b
m=c(160,162,166)
d=c(176,180,185)

x=data.frame(momheight=m,dadheight=d)
predict.lm(l,x)
predict.lm(l,x,interval = "confidence",level=0.98)

#3zad
h=scan()
L=scan()

#R-suqre = 0.91
l = lm(L~h)
summary(l)
abline(l)
plot(l)

#L=B0+B1h+B2h^2
l1=lm(L~h+I(h^2))
#98%
summary(l1)

coefficients(l1)
t=coefficients(l1)[1]+coefficients(l1)[2]*h+coefficients(l1)[3]*h^2
lines(h,t,col="green",type="l")
