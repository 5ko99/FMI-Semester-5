#a)
x = seq(1,10,0.2)
#dnorm zadava stoinostta na f v x, kudeto f e plutnostta
y = dnorm(x,5,sqrt(2))
plot(x,y,type="l")
abline(v=5)
#----------------
x = rnorm(1000,5,sqrt(2))
boxplot(x,horizontal = T)
hist(x,probability = T)
lines(density(x))
tx = seq(1,10,0.2)
ty= dnorm(tx,5,sqrt(2))
lines(tx,ty,col="blue")


qqnorm(x)
qqline(x)

#b)
x=runif(10000,1,5)
boxplot(x,horizontal = T)
hist(x,probability = T)
lines(density(x))
qqnorm(x)
qqline(x)

#v)
x=rexp(100,3)
x
boxplot(x,horizontal = T)
hist(x)
lines(density(x))

qqnorm(x)
qqline(x)

#д)
x1 = rnorm(1000,1,sqrt(2))
x2=rnorm(1000,5,sqrt(2))
x=c(x1,x2)
boxplot(x,horizontal = T)
hist(x,probability = T)
lines(density(x))


sumx = function(n) {
  res = rep(0,100);
  for(i in 1:n) {
    res = res + runif(100,1,5);#100 nabludeniq normalno razpr v int 1 do 5
  }
  return(res);
}
x=sumx(10000)
hist(x)
qqnorm(x)
qqline(x)



#zad 3)
p=pnorm(20,25,6)
qnorm(p+((1-p)/2),25,6)

#zad 4)
#Като последната задача от семинара
buffon = function(L,k,n) {
  x = runif(n,0,L/2);
  phi = runif(n,0,pi/2);
  
  return(sum(x < k/2*sin(phi))/n);
}
prob = buffon(1,0.2,10000)
2*0.2/1/prob
