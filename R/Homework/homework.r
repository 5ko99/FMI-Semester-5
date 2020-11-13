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

# разбиване в категории
f = cut(race,c((-1:5),(6),(7),(8),(9)))
levels(f) = c("Бяла","Бяла","Бяла","Бяла","Бяла","Бяла","Друга","Черна","Друга","Друга")
f

# д)
plot(smoke ~ f,ylim=c(0,3),na.rm=T,horizontal=T,main="Пушене според расата",xlab="Раса",ylab="Пушене")

# e)
plot(wt1 ~ f,ylim=c(85,252),horizontal=T,main="Тегло според расата",xlab="Раса",ylab="Тегло")
