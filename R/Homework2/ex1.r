#S vrushtane
#a Pika=1 Kupa=2 Karo=3 Spatiq=4
generateA =function(){
  x = 1:100
  for(i in 1:100){
    c = sample(1:4, 10, replace = T)
    x[i] = sum(c==2)
  }
  return(x)
}
a=generateA()
a
#b
EX = function(x){
  mean(a)
}
EX(a)

#Ver za po malko ot 6 kupi
lessThen = function(а) {
  lessThenArr = 1:6
  res = 0
  for(i in 1:6){
    lessThenArr[i] = sum(а == (i-1))
    res = res + lessThenArr[i]
  }
  return(res/100)
}
lessThen(a)


DX = function(a){
 var(a)
}
DX(a)

#Dov. interval
wilcox.test(a, conf.int = TRUE, conf.level = 0.9)

#g

#S vrushtabe
#a
generateA2 =function(){
  x = 1:100
  for(i in 1:100){
    c = sample(1:52, 10, replace = FALSE)
    x[i] = sum(c <= 13)
  }
  return(x)
}
a2=generateA2()
#b
EX(a2)
DX(a2)
#v
wilcox.test(a, conf.int = TRUE, conf.level = 0.9)

#g

