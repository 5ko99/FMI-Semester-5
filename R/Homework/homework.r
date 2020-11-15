library(UsingR)
attach(babies)
# Зад. 1
# а)
length(age[age<24])
# б)
length(age[age<24&smoke==1])

# в)
gestLen = length(gestation)
SortedLenOfGestation = sort(gestation)
mean(wt[SortedLenOfGestation[1:20]],na.rm=T)
mean(wt[SortedLenOfGestation[21:gestLen]],na.rm=T)

# г)
wt1[which(wt1==999)]=NA
inc[which(inc==98|inc==99)]=NA
boxplot(wt1 ~ inc,
        ylab="Тегло на майките в унции.",
        xlab="Семеен годишен доход в $. Започвайки от под 2500$, вървейки през 2500$.",
        main="Тегло на майките според 
        дохода на семейството")

# д)
smoke[which(smoke==9)]=NA
white = (race==0 | race==1 | race==2 | race==3 | race==4 | race==5)
black = (race==7)
others = (race==6 | race ==8 | race == 9)

lbls <- c("Никога", "В момента", "Допреди бременността", "Някога да, сега не")

tableWhite = table(smoke, white)
tableBlack = table (smoke,black)
tableOthers = table (smoke,others)
pie(tableWhite[ ,2],labels = lbls, main = "Пушене на белите майки")
pie(tableBlack[,2],labels=lbls, main = "Пушене на черните майки")
pie(tableOthers[ ,2],labels=lbls, main = "Пушене на другите майки")

# e)
f = cut(race,c((-1:5),(6),(7),(8),(9)))
levels(f) = c("Бяла","Бяла","Бяла","Бяла","Бяла","Бяла","Друга","Черна","Друга","Друга")
#f
wt1[which(wt1==999)]=NA
plot(wt1 ~ f,horizontal=T,main="Тегло според расата",xlab="Раса",ylab="Тегло")


# Зад. 2
allStickers = function(arr) {
  for(i in 1:20) {
    if(arr[i]==F) return(F);
  }
  return(T);
}

stickers = function() {
  N = 20;
  x = sample(1:20,N,replace=T);
  arr = rep.int(F,20);
  for(i in 1:N) {
    arr[x[i]] = T;
  }
  N = N + 1;
  while (!allStickers(arr)) {
    x = sample(1:20,N,replace=T);
    arr = rep.int(F,20);
    for(i in 1:N) {
      arr[x[i]] = T;
    }
    N = N + 1;
  }
  return(N);
}

prob.stickers = function( n ) {
  x = rep.int(0,n)
  c = 0
  for(i in 1 : n ) {
    c = c + stickers()
    x[ i ] = c / (1 * i )
  }
  return(x)
}
x = prob.stickers(10000)
plot(x,type="l",ylab = "Брой закупени стикери",xlab = "Опити")
abline(h = 48.34, col = 'red' )
