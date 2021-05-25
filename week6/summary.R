rm(list=ls())
# install.packages("readxl")
library(readxl)
# Запазване на данните в променливата 'rawData'
rawData=as.data.frame(read_excel("Data\\COVID-19_data.xlsx",sheet=1))

# Описание на променливите
read_excel("Data\\COVID-19_data.xlsx",sheet=2) 
str(rawData)

# Преобразуване на категорните данни във факторни променливи
rawData$dateRep=as.factor(rawData$dateRep)
rawData$countries=as.factor(rawData$countries)
rawData$countryCode=as.factor(rawData$countryCode)
rawData$continent=as.factor(rawData$continent)
str(rawData)
# edit(rawData)

# Основни дескриптивни статистики на променливите
summary(rawData)

# Премахване на наблюденията (страните) без брой на населението
rawData[is.na(rawData$population),]
cleanData=rawData[!is.na(rawData$population),] 
str(cleanData)
summary(cleanData)

################### Данни за България ####################
countryName='Bulgaria'
dataBGR=cleanData[cleanData$countries==countryName,]
summary(dataBGR)
plot(dataBGR$totalDays,dataBGR$cases,
     col=rgb(215/255,0,0),pch=21,bg=rgb(215/255,0,0),cex=1.5)
lines(dataBGR$totalDays,dataBGR$cases,lwd=2)

plot(dataBGR$totalDays,dataBGR$deaths,
     col=rgb(215/255,0,0),pch=21,bg=rgb(215/255,0,0),cex=1.5)
lines(dataBGR$totalDays,dataBGR$deaths,lwd=2)

################################### Задача 1 #################################
# Проверете дали настъпилият ежедневен брой на смъртните случаи в България е
# Поасоново разпределен.
# Chi-square test
xf=table(dataBGR$deaths)
lambdaEst=mean(dataBGR$deaths)
p0_3=lambdaEst^(0:3)*exp(-lambdaEst)/factorial(0:3)
p4=1-sum(p0_3)
p0_4=c(p0_3,p4)
chisq.test(xf,p=p0_4)

# Kolmogorov-Smirnov test
lambdaEst=mean(dataBGR$deaths)
ks.test(dataBGR$deaths,"ppois",lambda=lambdaEst)
#######################################################################################

############################# Разделяне по континенти #################################
lastDay=cleanData[cleanData$dateRep=='2020-04-06',]
rowCases=tapply(lastDay$totalCases,lastDay$continent,FUN=sum)
rowDeaths=tapply(lastDay$totalDeaths,lastDay$continent,FUN=sum)
rowPopulation=tapply(lastDay$population,lastDay$continent,FUN=sum)

################################### Задача 2 #################################
# Използвайки горните данни, проверете процента на разпространение на COVID-19
# е еднакъв на всички континенти. А процентът на смъртните случаи?

tableCases=rbind(rowPopulation-rowCases,rowCases)
rownames(tableCases)=c("Non-Infected","Infected")
tableCases
chisq.test(tableCases)
100*rowCases/rowPopulation

tableDeaths=rbind(rowCases-rowDeaths,rowDeaths)
rownames(tableDeaths)=c("Total Non-Deaths","Total Deaths")
tableDeaths
chisq.test(tableDeaths)
100*rowDeaths/rowCases

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
        legend('bottomright',
               legend = c('Regression line','Confidence lines','Prediction lines'),
               col=c(rgb(220/255,0,0),rgb(0,1/2,0),rgb(0,0,0)),lty=c(1,2,3),lwd=c(3,2,2))
}
############################################################################

dataBGR$y=dataBGR$totalCases
dataBGR$x=dataBGR$totalDays

#### Linear model Y=a+bX+e
model1BGR=lm(y~x,data=dataBGR)
summary(model1BGR)
plotConfLines(model1BGR,dataBGR$y,dataBGR$x)

#### Quadratic model Y=a+bX+cX^2+e
model2BGR=lm(y~x+x^2,data=dataBGR) # WRONG!!!!!!!!!!!!!!!!
summary(model2BGR)
model2BGR=lm(y~x+I(x^2),data=dataBGR)
summary(model2BGR)
model2BGR=lm(y~poly(x,degree=2,raw=TRUE),data=dataBGR)
summary(model2BGR)
anova(model1BGR,model2BGR) # Compare two models
plotConfLines(model2BGR,dataBGR$y,dataBGR$x)

#### Cubic model Y=a+bX+cX^2+dx^3+e
model3BGR=lm(y~poly(x,degree=3,raw=TRUE),data=dataBGR)
summary(model3BGR)
anova(model2BGR,model3BGR)
plotConfLines(model3BGR,dataBGR$y,dataBGR$x)

#### Polynomial model
model4BGR=lm(y~poly(x,degree=4,raw=TRUE),data=dataBGR)
summary(model4BGR)
anova(model3BGR,model4BGR)
plotConfLines(model4BGR,dataBGR$y,dataBGR$x)

plot(dataBGR$totalDays,dataBGR$totalCases,pch=21,cex=1.5,bg=rgb(2/3,2/3,2/3))
lines(dataBGR$totalDays,predict(model1BGR),col=rgb(220/255,0,0), lwd=3)
lines(dataBGR$totalDays,predict(model2BGR),col=rgb(0,115/255,0), lwd=3)
lines(dataBGR$totalDays,predict(model3BGR),col=rgb(0,0,233/255), lwd=3)
legend('bottomright',
       legend = c('Linear','Quadratic','Cubic'),
       col=c(rgb(220/255,0,0),rgb(0,115/255,0),rgb(0,0,233/255)),lwd=3)

###### Прогнози за следващата седмица ###############
newX=as.data.frame((max(dataBGR$totalDays)+1):(max(dataBGR$totalDays)+7))
names(newX)="x"

predict(model1BGR,newdata=newX,int="p",level = 0.95)
predict(model2BGR,newdata=newX,int="p",level = 0.95)
predict(model3BGR,newdata=newX,int="p",level = 0.95)
predict(model4BGR,newdata=newX,int="p",level = 0.95)
#####################################################

