#dbinom(n,p) връща вероятността биномно разпрделената 
#величина да има стойност k
#т.е. P(x=k)
dbinom(4,30,1/6)

#т.е. p(x<=k) where x is binomno razprdelelenie
#сумата на всички които е <= 4.същото като
#dbinom(4,30,1/6) + dbinom(3,30,1/6) + ... +(dbinom 0, 30, 1/6)
pbinom(4,30,1/6)

#rbinom(k,n,p) masib s k ex. na binomno razpredelenie
n=100000
x=rbinom(1000,30,1/6)
sum(x<5)/n

#pbinom(k,n,p,lower.tail=FALSE) vrushta P(x>K)
pbinom(4,30,1/6,lower.tail = FALSE)

#sumata na koito e po malka ot 0.60
#sushto moje da polzvame lower.tail
qbinom(0.60,30,1/6)

pbinom(4,30,1/6,lower.tail = FALSE)


#c mejdu 5 i 8 izstrela
pnbinom(5,3,0.2) - pnbinom(1,3,0.2)
