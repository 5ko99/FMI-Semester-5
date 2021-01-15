x = rnorm(50,2,1)
y = rnorm(50,3,1)
z = rnorm(50,2,1)

mx= mean(x)
my= mean(y)
mz = mean(z)

# broq na vsko -1: whitin 
wSS = 49*(var(x)+var(y)+var(z))
wSS

m = mean(c(x,y,z))
m
#between
bSS = 50*((mx-m)^2+(my-m)^2+(mz-m)^2)
bSS
#total sum of squares
tSS = 149*var(c(x,y,z))
tSS

bSS+wSS

F = (bSS/(3-1))/(wSS/(150-3))
F
1-pf(F,2,147)
#malko p-val -> sa razlichni

#Sega chrez vgradenite funkcii
dat= data.frame(x,y,z)

sat = stack(dat)
sat

#obratna na stack
dat1=unstack(sat)
dat1

a = aov(values~ind,data=sat) 
summary(a)
#nisko pr, sledovatelno za razlichni. 
#Othvurlqme hipotezata

#zad1
#5 4 4 6 4 6 3 3 4 5
#3 2 4 5 3 4 3 4 2 4
#4 6 4 2 4 5 5 3 6 4
x = scan()
y=scan()
z=scan()

#proverqvame dali dannite sa norm razpr
boxplot(dat)
shapiro.test(x)
shapiro.test(y)
shapiro.test(z)
#za vsichki p-value e >0.05 -> priemame che sa norm razpredeleni

dat = data.frame(x,y,z)
sat = stack(dat)

a= aov(values~ind,data=sat)
summary(a)
#p-val = 0.102 > 0.05 -> mozhe da priemem che prov
#imat ednakuv kriterii

#zad 2
insSpr = InsectSprays
attach(InsectSprays)
boxplot(insSpr$count~insSpr$spray)

uns = unstack
shapiro.test(uns$A) #0.7487 >0.05 norm
shapiro.test(uns$B) #norm
shapiro.test(uns$C) #po skoro ne
shapiro.test(uns$D) #tvurdo ne
shapiro.test(uns$E)
shapiro.test(uns$F) #norm

k = kruskal.test(count~spray,data=InsectSprays)
k
#p-value <0.05 -> othv. hipoteza Ho

#Ho : Ma = Mb = Mc = MD = Me = Mf
#H1 : !H0

dat = data.frame(uns$C,uns4D,uns$E)

#zad 3
#drug.txt
f=read.table(file.choose(),sep=",",header=T)
f$patient = as.factor(f$patient)
f$drug = as.factor(f$drug)
#Ho: M1 = M2= M3 = M4
#H1:     !=
#Greshen test e otchitame che pacientite suvpadat
a = aov(response~drug,data = f)
summary(a)
#0.166 > 0.05 
a2 = aov(response~drug+Error(patient),data =f)
summary(a2)
# 4.5e-05 < 0.5 izbod othuvrlqme Ho priemame H1, t.e. razlichni sa.
