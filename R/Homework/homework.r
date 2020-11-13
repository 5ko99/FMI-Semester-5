library(UsingR)
attach(babies)
# Зад. 1
# а)
length(age[age<24])
# б)
length(age[age<24&smoke==1])

# в)
SortedLenOfGestation = sort(gestation)
mean(wt[SortedLenOfGestation[1:20]],na.rm=T)
mean(wt[B[21:1236]],na.rm=T)

# г)
MotherWt = wt1[wt1<999]
boxplot(sort(wt1) ~ inc,subset = c(1:length(MotherWt)),na.rm=T,
        ylab="Тегло на майките",
        xlab="Семеен доход",
        main="Тегло на майките според 
        дохода на семейството")

# д)
factor(race,levels=race[race==7])
f = cut(race,c((-1:5),(7)))
levels(f) = c("a","a","a","a","a","a","b")
