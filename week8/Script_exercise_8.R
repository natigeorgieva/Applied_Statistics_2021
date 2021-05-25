######### APPLIED STATISTICS -> EXERCISE 8  #######


rm(list=ls())                    ### Logistic regression ###  

install.packages("ISLR")
library(ISLR)
str(Default)
?Default
edit(Default)
Default=Default[order(Default$balance),]

#### Задача 1 ####
# Като изплозвате данните 'Default' от пакета 'ISLR', конструирайте и
# изследвайте прост линеен модел за предсказване на променливата 'default'
# чрез променливата 'balance'. Постройте графика илюстрираща наблюденията 
# и получената регресионна права.

Default$y=as.numeric(Default$default=="Yes")
Default$x=Default$balance
model1=lm(y~x,data=Default)
summary(model1)
plot(Default$x,Default$y,pch=21,cex=1.5,bg=rgb(2/3,2/3,2/3))
lines(Default$x,predict(model1),col="red",lwd=2)

##########################################################################################

logitModel=glm(default~balance,data=Default,family=binomial)
summary(logitModel)
plot(Default$balance, Default$y, xlim=range(Default$balance), 
     ylim=range(Default$y),pch=21,cex=1.5,bg=rgb(2/3,2/3,2/3))
lines(Default$balance,predict(logitModel,type="response"), col=rgb(220/255,0,0), lwd=3)
contrasts(Default$default)
modelProb=predict(logitModel,type="response")
modelPred=rep("No",10000)
modelPred[modelProb>0.5]="Yes"
table(modelPred,Default$default)

# The Stock Market Data #
str(Smarket)
?Smarket
edit(Smarket)
summary(Smarket)
plot(Smarket)
cor(Smarket) # WRONG! -> Factor variable
cor(Smarket[,-9])
plot(Smarket$Volume)

#### Задача 2 ####
# Като изплозвате данните 'Smarket' от пакета 'ISLR', конструирайте и изследвайте 
# прост линеен модел за предсказване на процента на възвращаемост ('Today') на  
# база на най-подходящата друга променлива в данните. Какво може да заключите?

Smarket$y=Smarket$Today
Smarket$x=Smarket$Lag5

modelLag5=lm(y~x,data=Smarket)
summary(modelLag5)



########################################################################################
### Multiple Logistic Regression ###
attach(Smarket)
model1=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(model1)
coef(model1)
summary(model1)$coef
summary(model1)$coef[,4]
model1Prob=predict(model1,type="response")
model1Prob[1:10]
contrasts(Direction)
model1Pred=rep("Down",1250)
model1Pred[model1Prob>0.5]="Up"

table(model1Pred,Direction)
(507+145)/1250
mean(model1Pred==Direction)

table(Direction)
648/(602+648)
sum(Direction=="Up")/length(Direction)

train=(Year<2005)
Smarket2005=Smarket[!train,]
dim(Smarket2005)
Direction2005=Direction[!train]

model2=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
summary(model2)
model2Prob=predict(model2,Smarket2005,type="response")
model2Pred=rep("Down",dim(Smarket2005)[1])
model2Pred[model2Prob>0.5]="Up"

table(model2Pred,Direction2005)
mean(model2Pred==Direction2005)
mean(model2Pred!=Direction2005)

table(Direction2005)
sum(Direction2005=="Up")/length(Direction2005)

model3=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
summary(model3)
model3Prob=predict(model3,Smarket2005,type="response")
model3Pred=rep("Down",dim(Smarket2005)[1])
model3Pred[model3Prob>0.5]="Up"
table(model3Pred,Direction2005)
mean(model3Pred==Direction2005)
sum(Direction2005=="Up")/length(Direction2005)

predict(model3,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

detach(Smarket)

#### Задача 3 ####
# Като изплозвате данните 'Default', конструирайте многомерен логистичен модел за предсказване
# на променливата 'default'. Коефициентите на кои предиктори в модела са значими?
# Каква е точността на модела? Ако използвате само променливата 'student' като предиктор, как 
# може да интерпретирате получения модел?



