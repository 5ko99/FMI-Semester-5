x= rnorm(10,2,2)
m = mean(x)
s = sd(x)

t= (m-5)*sqrt(10)/s
pt(t,df=9)
pt(t,df=9)*2

t.test(x,mu=5,alternative = "two.sided")
#<5% -> otxvurlqme
#>=5% -> priemame
#sega imame 0.004 -> otxvurlqme!

#b)
x=rnorm(10,2,2)
t.test(x,mu=3,alternative="two.sided")


library(UsingR)
boxplot(vacation,horizontal = T) #що годе нормално разпрл.
hist(vacation) #прилича на норм. разпр
qqnorm(vacation)
qqline(vacation)
shapiro.test(vacation)#тест за норм. разпр с/у те не са
#при тези 13%, можем да приемем че са
#извод: правим t.test
t.test(vacation,mu=24,alternative = "two.sided")
#понеже pvalue = 0.03045<0.05 отхвърляме хип. H0 и приемаме
# алтернативате H1 mu!=24

#3
#H0: p = 1/2
#H1: p!=1/2

prop.test(42,100,p=1/2,alternative="two.sided")
#pvalue = 0.1336 > 0.05 sled. mozhe da priemem che H0 e vqrno

prop.test(420,1000,p=1/2,alternative = "two.sided")

prop.test(42,100,p=1/2,alternative="less")
#p-value = 0.06... > 0.05 mozhe da priemem hip. za vqrna

#zad.4
#H0: klientite gov 5 min:mu=5
#H1: mu>5
x=scan() #12.8 3.5 2.9 9.4 8.7 0.7 0.2 2.8 1.9 2.8 3.1 15.8
boxplot(x,horizontal = T)
hist(x,probability = T)
qqnorm(x)
qqline(x)
mean(x)
#zakl. che dannite ne sa normalno razpredeleni
#izvod-> pravim wilcox.test
wilcox.test(x,mu=5,alternative = "greater")
#приемаме хипотезата!

#zad5
#iskame tezi za stomax
x=cancer$stomach
#proverqvame dali dannite sa normalno razp.
boxplot(x,horizontal = T)
hist(x,probability = T)
qqnorm(x)
qqline(x)
shapiro.test(x)
#p-val= 0.002075 < 0.05
#dannite ne sa norm. razpredeleni

#H0: mu>=100
#H1 mu<100
wilcox.test(x,mu=100,alternative = "less")
#p-val=0.863 > 0.05 izvod: priemame hipotezata

#6
attach(survey)

table(Sex,Smoke)
#H0: p>=0.8
#H1: p<0.8
prop.test(89,5+89+10+12,p=0.8,alternative = "less")
#p-val = 0.2218> 0.05 -> mozhem da priemem hipotezata
