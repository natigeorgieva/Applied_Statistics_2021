rm(list=ls())
########## Помощна функция за конструирането на доверителни криви ########## 
plotConfLines<-function(model,y,x){
  minX=floor(min(x))
  maxX=ceiling(max(x))
  predData <- data.frame(x=minX:maxX)
  pp <- predict(model, int="p", newdata=predData,level=0.95)
  pc <- predict(model, int="c", newdata=predData)
  plot(x, y, xlim=range(x),
       ylim=range(y, pp),pch=21,cex=1.5,bg=rgb(2/3,2/3,2/3))
  lines(x,predict(model), col=rgb(220/255,0,0), lwd=3)
  matlines(predData, pc[,2:3], lty=c(2,2),col=rgb(0,1/2,0),lwd=c(2,2))
  matlines(predData, pp[,2:3], lty=c(3,3),col=rgb(0,0,0),lwd=c(2,2))
  legend('topright',
         legend = c('Regression line','Confidence lines','Prediction lines'),
         col=c(rgb(220/255,0,0),rgb(0,1/2,0),rgb(0,0,0)),lty=c(1,2,3),lwd=c(3,2,2))
}
############################################################################
#### Задача 1 ####
# Като изплозвате данните 'Boston' от пакета 'MASS', конструирайте и изследвайте 
# прост линеен модел за предсказване на средната цена на домовете ('medv') на  
# база на най-подходящата друга променлива в данните. Какво може да заключите?
# Проверете предположението на модела за нормалност на получените остатъци.

#instal.packages("MASS")
library(MASS)
str(Boston)
edit(Boston)

cor(Boston$medv,Boston$crim)

plot(Boston)
cor(Boston)

Boston$y=Boston$medv
Boston$x=Boston$lstat

model1=lm(y~x,data=Boston)
summary(model1)

plotConfLines(model1,Boston$y,Boston$x)

hist(model1$residuals)
qqnorm(model1$residuals)
qqline(model1$residuals)
shapiro.test(model1$residuals)

edit(Boston)
model2=lm(y~poly(x,degree=2,raw=TRUE),data=Boston)
summary(model2)
order(Boston$x)
Boston=Boston[order(Boston$x),]
plotConfLines(model2,Boston$y,Boston$x)

anova(model1,model2)

model3=lm(y~poly(x,degree=3,raw=TRUE),data=Boston)
order(Boston$x)
Boston=Boston[order(Boston$x),]
plotConfLines(model2,Boston$y,Boston$x)

anova(model1,model3)

model5=lm(y~poly(x,degree=5,raw=TRUE),data=Boston)
summary(model5)
order(Boston$x)
Boston=Boston[order(Boston$x),]
plotConfLines(model5,Boston$y,Boston$x)

model6=lm(y~poly(x,degree=6,raw=TRUE),data=Boston)
summary(model6)
order(Boston$x)
Boston=Boston[order(Boston$x),]
plotConfLines(model5,Boston$y,Boston$x)

shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)
shapiro.test(model5$residuals)
shapiro.test(model6$residuals)

###Трансформация за получаване на линеен модел###
logModel=lm(y~log(x),data = Boston)
summary(logModel)
anova(logModel,model1)
shapiro.test(logModel$residuals)
order(Boston$x)
Boston=Boston[order(Boston$x),]
plotConfLines(logModel,Boston$y,Boston$x)
