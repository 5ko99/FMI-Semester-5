library(UsingR)
#zad1
#Ho : pi=1/7 za vsqko i = 1,...,7
#H1: pi!=1/7 (druigi sa)

#125  410  310  300  318  298  148
x = scan()
barplot(x)

sum(x)
#n*pi
e = sum(x)/7
t=sum((x-e)^2)/e
t
pchisq(t,6,lower.tail = F) #st. na svoboda = brneshta - 1
#Izvod othurlqme hipotezata i priemame alternativata

#2nachin
chisq.test(x)

#zad2
#Ho: pi=1/10 i = 0,...,9
#H1: p1!= 1/10 (drugi sa)
data(pi2000)
piDiggits = pi2000[1:200]
table(piDiggits)
#19 20 24 20 22 20 15 12 25 23 
x = scan()
chisq.test(x)
#p-val = 0.6163 izvod mozhe da priemem hip Ho

#zad3
#Ho: The text is in English
#H1: not in English
#102  108  90  95  82  40
x=scan()
x[7] = 1036 - sum(x)
#12.7  9.56  8.17  7.51  6.97  6.75  48.34
pr = scan()
prob= pr/100
chisq.test(x,p=prob)
#p-value=0.0001878 -> oth. hip

#Zad4
#H0: H i Y sa nezavisimi
#H1: X i Y sa zavisimi
#12813 647 359 42 65963 4000 2642 303
x=scan()
mat= matrix(x,nrow=2,byrow=T)
mat
chisq.test(mat)
#izvod: Mozhe da priemem hip

#zad5
#Ho: X i Y sa zav
#H1: X i y za nezavisimi
#44  74  79  72  31 14  25  27  24  10 15  20  20  23  9 3   5   5   0   0
x = scan()
mat = matrix(x,nrow=4,byrow = T)
mat
#Zaradi 0te v posledniqt red na tablicata
newMat = mat[1:3,1:5]
newMat [3,] = newMat [3,] + mat[4,]
newMat
#Tova proverqva dali sa nezavisimi 
chisq.test(newMat)
#Izvod: X i Y sa nezavisimi -> othvurlqme hip.

#zad6.
#tursq x1=1/8 p1=0.22
pexp(1/8,2)
#tursq x2=1/4 p2=0.17
pexp(1/4,2)=pexp(1/8,2)
#tursq x3=1/2 p3 = 0.23
pexp(1/2,2) = pexp(1/4,2)
#tursq x4 = 1 p4 = 0.23
pexp(1,2)= pexp(1/2,2)

#p5=1-p4-p3-p2-p1
#p=c(p1,p2,p3,p4,p5)