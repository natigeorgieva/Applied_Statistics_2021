
################################### Summary #############################
rm(list=ls())
data("thuesen",package = "ISwR")
attach(thuesen)
model1=lm(short.velocity ~ blood.glucose)
summary(model1)
minBG=floor(min(blood.glucose))
maxBG=floor(max(blood.glucose))+1
predData <- data.frame(blood.glucose=minBG:maxBG)
pp <- predict(model1, int="p", newdata=predData,level=0.95)
pc <- predict(model1, int="c", newdata=predData)
plot(blood.glucose, short.velocity, xlim=range(blood.glucose),
     ylim=range(short.velocity, pp, na.rm=T),pch=21,cex=1.5,bg=rgb(1/3,1/3,1/3))
abline(model1, col=rgb(220/255,0,0), lwd=3)
# segments(blood.glucose,fitted(model1),blood.glucose,short.velocity,lty=2)
matlines(predData, pc[,2:3], lty=c(2,2),col=rgb(0,1/2,0),lwd=c(2,2))
matlines(predData, pp[,2:3], lty=c(3,3),col=rgb(0,0,0),lwd=c(2,2))
legend('bottomright',
       legend = c('Regression line','Confidence lines','Prediction lines'),
       col=c(rgb(220/255,0,0),rgb(0,1/2,0),rgb(0,0,0)),lty=c(1,2,3),lwd=c(3,2,2))
detach(thuesen)
##################################################################################################