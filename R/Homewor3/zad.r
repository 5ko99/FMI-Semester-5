#Ho: ne zavisi ot ranka
#H1: zavisi ot ranka
x = c(43, 12, 8, 96, 32, 18, 123, 35, 44)
m = matrix(x, nrow=3, ncol=3)
chisq.test(m)
#p-val>0.05 -> priemame Ho. Ne zavisi

#zad 2
file = read.csv(file.choose(),sep=";",dec=",")
y = file$y
x1 = file$x1
x2 = file$x2
x3 = file$x3
x4 = file$x4
x5 = file$x5
data = data.frame(file$y,file$x1,file$x2,file$x3,file$x4,file$x5)
plot(data)
l1=lm(y~x1+x2+x3+x4+x5)
summary(l1)
l2=lm(y~x1+x3+x5-1)
summary(l2)

#g
z = data.frame(x1=2,x2=4.5,x3=3.9,x4=4,x5=6.5)
predict.lm(l2,z,interval='confidence',level=0.95)
