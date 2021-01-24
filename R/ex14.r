f = read.table(file.choose(),dec='.',sep=',',header=T)
str(f)
boxplot(gre~admit,data=f)

#Ho : Mgre0 = Mgre1
#H1 : razlich
t.test(gre~admit,data=f)
#p-value <0.05 -> Priemame H1.

table(f$admit,f$rank)

#Ho: admit ne zavisi ot rank
#H1: admit zavisi ot rank

chisq.test(table(f$admit,f$rank))
#p-value<0.05 -> priemame H1 da sa zavisimi
#t.e. obosnovano e da vzemem rank v logistichen model

l = glm(admit~gre+gpa+rank,data=f,family = "binomial")
#Ho: Bi = 0
#H1: Bi \= 0
#Ako pri premahvaneto ili dobavqneto na nqkoq probenliva
# kum modela, namali stoinostta na AIC, to modela se e podobril!
#Tursim model predstavqsht dannite s minimalno AIC.
#Aic:467.44
summary(l)
l1 = glm(admit~gre+rank,data=f,family = "binomial")
summary(l1)
#AIC=471.2 -> otshabihme modela pri premahvaneo na gpa
l2 = glm(admit~gre+gpa,data=f,family = "binomial")
summary(l2)
#Izvod-> sushtiq. l e nai-dobriqt model zasega :)

#JackKnife
ljk = glm(admit~gre+rank,data=f[-1,],family="binomial")
f[1,]
predict.glm(ljk,f[1,],type="response")

JKtest = function(d) {
  sum=0;
  for(i in 1:400) {
    ljk = glm(admit~gre+rank,data=f[-i,],family="binomial")
    if(predict.glm(ljk,f[i,],type="response")>0.5) {
      p = 1;
    } else {
      p = 0;
    }
    if(p==d$admit[i]) {
      sum = sum + 1;
    }
  }
  return (sum/400);
}
JKtest(f)

#shte bude li priet chovek
newData = data.frame(gre=700,gpa=3.5,rank=2)
predict.glm(l,newData,type="response")
#Veroqtnostta da vleze ~44% <50% -> nqma da bude priet

#zad2
library(mlbench)
data("BreastCancer")
str(BreastCancer)
complete.cases(BreastCancer)
bc = BreastCancer[complete.cases(BreastCancer),]
bc = bc[,-1]
for(i in 1:9) {
  bc[,i] = as.numeric((bc[,i]))
}
str(bc)
bc$Class = ifelse(bc$Class=="benign",0,1)
str(bc)

#delim dannite na 30%, 70%- s ednite stroim s dr testvamme
ind = sample(1:483,480)

bcfit = bc[ind,]
bctest = bc[-ind,]

l = glm(Class~.,data=bcfit,family = "binomial")
summary(l)

l1 = glm(Class~.-Cell.size,data=bcfit,family = "binomial")
summary(l1)

l2 = glm(Class~.-Cell.size -Epith.c.size,data=bcfit,family = "binomial")
summary(l2)

#l3 best
p = predict.glm(l2,bctest,type="response")
pr = ifelse(p>0.60,1,0)

sum(pr==bctest$Class)/length(bctest$Class)

sum(bctest$Class == 0 & pr ==0)
sum(bctest$Class==1 & pr==1)
sum(bctest$Class==0 & pr==1) #Gr II rod
sum(bctest$Class==0 & pr==1) # Gr I rod
