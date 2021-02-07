# Ho: y = 220 - x 
# H1: y != 220 - x
x= scan()
y = scan()
lr = lm(y~x)
s = summary(lr)

new = data.frame(x = c(30,40,50))

predict.lm(lr,new)

s

t = (-0.79773 -(-1))/0.06996
(1-pt(t,length(x)-2))*2
#p - value = 0.01261869 < 0,05 ohvurlqme hipotezata
predict.lm(lr,new,interval="prediction",level = 0.9)



