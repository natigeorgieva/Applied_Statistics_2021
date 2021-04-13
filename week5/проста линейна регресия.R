
rm(list=ls())                   #### Simple Linear Regression ####         
install.packages("ISwR")
data("thuesen",package = "ISwR")
??thuesen
str(thuesen)
summary(thuesen)
# edit(thuesen)
thuesen[!is.na(thuesen$short.velocity),]
cleanData=thuesen[!is.na(thuesen$short.velocity),]

attach(cleanData)
plot(blood.glucose, short.velocity, xlim=c(0,20), ylim=c(1,2))
lm(short.velocity ~ blood.glucose)
model1=lm(short.velocity ~ blood.glucose)
detach(cleanData)

theFormula = short.velocity ~ blood.glucose
model1=lm(formula = theFormula,data=cleanData)
model1
str(model1)
summary(model1)               # analysis summary
abline(model1,col=2)          # add regression line
coef(model1)                  # regression coefficients
confint(model1,level = 0.95)  # confidence interval for regression coefficients
resid(model1)                 # residuals
fitted(model1)                # fitted values
deviance(model1)              # residual sum of squares
predict(model1,newdata=ndat)  # predict for new data

predict(model1)
x=as.data.frame(thuesen$blood.glucose[is.na(thuesen$short.velocity)])
names(x)="blood.glucose"
predict(model1,newdata=x)
sum(coef(model1)*c(1,as.numeric(x)))

plot(fitted(model1),resid(model1))
qqnorm(resid(model1))

plot(cleanData$blood.glucose, cleanData$short.velocity, xlim=c(0,20), ylim=c(1,2))
abline(model1$coefficients, col='red', lwd=2)
points(cleanData$blood.glucose,predict(model1),pch=10)    
segments(cleanData$blood.glucose,fitted(model1),
         cleanData$blood.glucose,cleanData$short.velocity)

############
options(na.action=na.exclude) # exclude NA's
model1=lm(formula = theFormula,data=thuesen)
fitted(model1)
plot(thuesen$blood.glucose, thuesen$short.velocity, xlim=c(0,20), ylim=c(1,2))
abline(model1$coefficients, col='red', lwd=2)
segments(thuesen$blood.glucose,fitted(model1), thuesen$blood.glucose,thuesen$short.velocity)
############


############################ Prediction and Confidence lines ################################
minBG=floor(min(thuesen$blood.glucose))
maxBG=floor(max(thuesen$blood.glucose))+1
predData <- data.frame(blood.glucose=minBG:maxBG)
pp <- predict(model1, int="p", newdata=predData)
pc <- predict(model1, int="c", newdata=predData)
plot(thuesen$blood.glucose,thuesen$short.velocity,
     ylim=range(thuesen$short.velocity, pp, na.rm=T))
matlines(predData, pc[,2:3], lty=c(2,2),col=c(4,4))
matlines(predData, pp[,2:3], lty=c(3,3),col=c(2,2))

