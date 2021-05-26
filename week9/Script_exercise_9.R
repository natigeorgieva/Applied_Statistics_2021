######### APPLIED STATISTICS -> EXERCISE 9 (Multicollinearity + Forward selection method) #######
rm(list=ls()) 
###################################################################################################
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
#### Задача 1 ####
# Намерете корелацията между х1 и х2 и начертайте scatterplot, за да изобразите връзката между двете
# променливи. Значими ли са коефициентите пред х1 и х2 в модела y~x1+x2 ? А в моделите y~x1 и y~x2?
# Обяснете получените резултати.



###################################################################################################
rm(list=ls())   
library(readxl)
read_excel("Data\\cereals.xls",sheet=2)
cereals=as.data.frame(read_excel("Data\\cereals.xls",sheet=1))
str(cereals)
row.names(cereals)=cereals[,1]
cereals=cereals[,-1]
str(cereals)
edit(cereals)

#### Задача 2 ####
# Като изплозвате данните 'cereals', конструирайте и изследвайте прост линеен
# модел за предсказване на рейтинга на зърнените закуски ('rating') на база
# на най-подходящата друга променлива в данните. Коя друга променлива може да
# бъде използвана за предсказване на rating? Направете проверка за мултиколинеарност.
# Можем ли да добавим втората променлива като допълнителен предиктор в линеен модел?
# Повторете горната процедура с добавяне на нови променливи при условие, че те не са
# мултикорелирани с предикторите в модела.



###################################################################################################
rm(list=ls())   
library(readxl)
fifa19=as.data.frame(read_excel("Data\\fifa19_forward.xls",sheet=1))


#### Задача 3 ####
# Като изплозвате данните 'fifa19' и постъпкова регресия, конструирайте линеен модел 
# за предсказване на заплатата ('wage') на водещите нападатели във футбола. От колко 
# променливи се състои окончателният модел?



#################################################################################
indexList=order(abs(corY),decreasing = TRUE) # Ordered list of indices for predictors 
currentIndexList=indexList[2] # List of indices of predictors included in the current step

# QMC: Function that returns logic expression
# TRUE-> if the predictor with index 'indexQ' is quasi-multicorrelated (R^2>0.36)
#       with the predictors in the current step (currentIndexList)
# FALSE-> if the predictor with index 'indexQ' is NOT quasi-multicorrelated (R^2<=0.36)
#       with the predictors in the current step (currentIndexList)
QMC<-function(indexQ,currentIndexList){
        
        qmcFifa=fifa19[,1:(length(currentIndexList)+1)]
        qmcFifa[,1]=fifa19[,indexQ]
        colnames(qmcFifa)=c("Y",1:length(currentIndexList))
        for (i in 1:length(currentIndexList)) {
                qmcFifa[,i+1]=fifa19[,currentIndexList[i]]
        }
        qmcModel=lm(Y~.,data=qmcFifa)
        varY=(sd(qmcFifa$Y))^2
        varEps=(sd(qmcModel$residuals))^2
        qmcR2=1-varEps/varY
        summary(qmcModel)
        return(qmcR2>0.36)
}

# selectVar: Function that returns index
# 0 -> if there are no new predictors for the linear model
# indexQ -> the index of the "best" next predictor that is NOT quasi-multicorrelated (R^2<=0.36)
#       with the predictors in the current step (currentIndexList)
selectVar<-function(indexList,currentIndexList){
        lastIndex=which(indexList==currentIndexList[length(currentIndexList)])
        if (length(indexList)<(lastIndex+1)) {return(0)}
        else{
                counter=1
                indexQ=indexList[lastIndex+counter]
                while ( QMC(indexQ,currentIndexList) & length(indexList)>(lastIndex+counter) ) {
                        counter=counter+1
                        indexQ=indexList[lastIndex+counter]
                }
                if (QMC(indexQ,currentIndexList)) {return(0)}  else{return(indexQ)}
        }
}

# Repeatedly selecting the new "best" predictor 
# and updating the current list of predictors (currentIndexList)
while (selectVar(indexList,currentIndexList)!=0) {
        currentIndexList=c(currentIndexList,selectVar(indexList,currentIndexList))
}

# Final predictors
finalPredIndex=currentIndexList
colnames(fifa19)[finalPredIndex]

# Constructing the final model
modelData=fifa19[,1:(length(finalPredIndex)+1)]
colnames(modelData)=c("Wage",colnames(fifa19)[finalPredIndex])
for (i in 1:length(finalPredIndex)) {
        modelData[,i+1]=fifa19[,finalPredIndex[i]]
}
finalModel=lm(Wage~.,data=modelData)
summary(finalModel)


### The first 7 models (with the 7 best predictors): manually constructed

model1=lm(Wage~Reactions,data=fifa19)
summary(model1)

which(corTable[order(abs(corY),decreasing = TRUE),"Reactions"]<0.6)
model2=lm(Wage~Reactions+YC,data=fifa19)
summary(model2)

which((corTable[order(abs(corY),decreasing = TRUE),"Reactions"]<0.6) & 
              (corTable[order(abs(corY),decreasing = TRUE),"YC"]<0.6))
QMC_Dribbling=lm(Dribbling~Reactions+YC,data=fifa19)
summary(QMC_Dribbling)

model3=lm(Wage~Reactions+YC+Dribbling,data=fifa19)
summary(model3)

which((corTable[order(abs(corY),decreasing = TRUE),"Reactions"]<0.6) & 
              (corTable[order(abs(corY),decreasing = TRUE),"YC"]<0.6) & 
              (corTable[order(abs(corY),decreasing = TRUE),"Dribbling"]<0.6))

QMC_LongShots=lm(LongShots~Reactions+YC+Dribbling,data=fifa19)
summary(QMC_LongShots)

model4=lm(Wage~Reactions+YC+Dribbling+LongShots,data=fifa19)
summary(model4)

which((corTable[order(abs(corY),decreasing = TRUE),"Reactions"]<0.6) & 
              (corTable[order(abs(corY),decreasing = TRUE),"YC"]<0.6) & 
              (corTable[order(abs(corY),decreasing = TRUE),"Dribbling"]<0.6) &
              (corTable[order(abs(corY),decreasing = TRUE),"LongShots"]<0.6) )

QMC_Penalties=lm(Penalties~Reactions+YC+Dribbling+LongShots,data=fifa19)
summary(QMC_Penalties)

model5=lm(Wage~Reactions+YC+Dribbling+LongShots+Penalties,data=fifa19)
summary(model5)

which((corTable[order(abs(corY),decreasing = TRUE),"Reactions"]<0.6) & 
              (corTable[order(abs(corY),decreasing = TRUE),"YC"]<0.6) & 
              (corTable[order(abs(corY),decreasing = TRUE),"Dribbling"]<0.6) &
              (corTable[order(abs(corY),decreasing = TRUE),"LongShots"]<0.6) &
              (corTable[order(abs(corY),decreasing = TRUE),"Penalties"]<0.6) )

QMC_SprintSpeed=lm(SprintSpeed~Reactions+YC+Dribbling+LongShots+Penalties,data=fifa19)
summary(QMC_SprintSpeed)

QMC_Stamina=lm(Stamina~Reactions+YC+Dribbling+LongShots+Penalties,data=fifa19)
summary(QMC_Stamina)

model6=lm(Wage~Reactions+YC+Dribbling+LongShots+Penalties+Stamina,data=fifa19)
summary(model6)

which((corTable[order(abs(corY),decreasing = TRUE),"Reactions"]<0.6) & 
              (corTable[order(abs(corY),decreasing = TRUE),"YC"]<0.6) & 
              (corTable[order(abs(corY),decreasing = TRUE),"Dribbling"]<0.6) &
              (corTable[order(abs(corY),decreasing = TRUE),"LongShots"]<0.6) &
              (corTable[order(abs(corY),decreasing = TRUE),"Penalties"]<0.6) &
              (corTable[order(abs(corY),decreasing = TRUE),"Stamina"]<0.6) )

QMC_Age=lm(Age~Reactions+YC+Dribbling+LongShots+Penalties+Stamina,data=fifa19)
summary(QMC_Age)

QMC_Number=lm(Number~Reactions+YC+Dribbling+LongShots+Penalties+Stamina,data=fifa19)
summary(QMC_Number)

model7=lm(Wage~Reactions+YC+Dribbling+LongShots+Penalties+Stamina+Number,data=fifa19)
summary(model7)
