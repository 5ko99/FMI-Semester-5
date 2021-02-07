#Danni koito ne sa podhodqashti za lineina regresiq
z = 12 + rnorm(91,5,12)
plot(x,z)
lr2=lm(z~x)
summary(lr2)


#Generira redica ot 1 do 10 sus stupka 0.1
x = seq (1,10,0.3)
#Dulgina na x
length(x)
#Razglezdame danni s greshka err, koqto e normalno razpredelena (0,1)
y = 6*x+10+rnorm(length(x),0,1)

plot(x,y)
#Srednite na x i y
bar_x = mean(x)
bar_y = mean(y)
#Presmqtame koef b1
b1 = sum((y-bar_x)*(x-bar_x))/sum((x-bar_x)^2)
b1
#Presmqtame koef b0
b0 = bar_y - b1*bar_x
b0
#Chertae pravata y=b0+b1x
abline(b0,b1,col = 'red')

lr = lm(y~x)
lr
sm = summary(lr)
#Ho: B1 = 8
#H1: B1 != 8

tab = sm$coefficients
tab[2,2]
t = (tab[2,1]-8)/tab[2,2]

pt(t,length(x)-2)*2
#p - value = 4.14317e-66 < 0.05 othuvrlqme hipotezata che B1 = 8, t.e. B1 != 8.

#Ho: B0 = 10
#H1: B0 != 10

t = (tab[1,1]-10)/tab[1,2]
t
(1-pt(t,length(x)-1))*2

pt(t,length(x)-1,lower.tail = F)*2
#p-value = 0.9537201 > 0.05 priemame hipotezata

sm
new = data.frame(x = c(2,5,7))

prd = predict.lm(lr,new)

points(c(2,5,7),prd,col="blue")

prd1 = predict.lm(lr,new, interval ="confidence" )
prd2 = predict.lm(lr,new, interval ="prediction" )
prd1
prd2
new = data.frame(x=1:8)
prd1 = predict.lm(lr,new, interval ="confidence" )
lines(1:8,prd1[,'lwr'],col='green')
lines(1:8,prd1[,'upr'],col='green')

prd1 = predict.lm(lr,new, interval ="prediction" )
lines(1:8,prd1[,'lwr'],col='blue')
lines(1:8,prd1[,'upr'],col='blue')



