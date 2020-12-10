#pri izchislena 6ca na ruka, vij upr 9 v djoba
pwilcox(6,4,4)
#pvalue =
pwilcox(6,4,4)*2 # = 6857
# >0.05 Moze da priemem hipotezata x=y

#Za da ne smqtame na ruka
x=c(4,1,7,9)
y=c(10,3,2,11)
wilcox.test(x,y)

#zad 2
#x-no somkers
#y- smokers
#hipoteza Ho: Px=Py
#Alternative H1: Px!=Py
prop.test(c(351,71),c(605,195),alternative = "two.sided")
#p-value = 2.303e-07 < 0.05 -> Othvurlqme hip.

#3zad
#x=15 10 13 7 9 8 21 9 14 8
#y=15 14 12 8 14 10 7 16 10 15 12
x = scan()
y= scan()

#proverqvamen dannite x dali sa normalno raprl
#hip X<=Y
#alt X>Y
#prov x
boxplot(x,horizontal = T)
hist(x)
qqnorm(x)
qqline(x)
shapiro.test(x)

#prov i y
boxplot(y,horizontal = T)
hist(y)
qqnorm(y)
qqline(y)
shapiro.test(y)

#ne sa norm razpr
wilcox.test(x,y,alternative = "greater")
#mozhe da priemem hip p val>0.05

#zad 4
x = scan()
y = scan()

#Ho:X=Y
#H1:X!=Y

#x = 70 85 63 54 65 80 75 95 52 55
#y= 72 86 62 55 63 80 78 90 53 57

#prov x
boxplot(x,horizontal = T)
hist(x)
qqnorm(x)
qqline(x)
shapiro.test(x)
#priemame che dannite x sa norm razpr

#prov i y
boxplot(y,horizontal = T)
hist(y)
qqnorm(y)
qqline(y)
shapiro.test(y)
#priemame che sa norm razprdeleni
t.test(x,y,alternative = "two.sided",paired=T)
#p-value = 0.7937 > 0.05 -> mozhe da priemem testa :)

#5 zad
library(UsingR)
x = ewr$AA
y = ewr$NW

#Hipoteza Ho: X = Y
#Alternative H1: X!=Y

#prov x
boxplot(x,horizontal = T)
hist(x)
qqnorm(x)
qqline(x)
shapiro.test(x)
#priemame che dannite x ne sa norm razpr

#prov i y
boxplot(y,horizontal = T)
hist(y)
qqnorm(y)
qqline(y)
shapiro.test(y)
# i tezi ne sa

#izvo->wilcox
wilcox.test(x,y,alternative = "two.sided")
