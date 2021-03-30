### Задача 5 ### (Chi-square test for independence)
# Като използвате данните "Titanic.csv", проверете дали класата на билета на един пътуващ 
# оказва влияние на това дали той е оцелял.

dataTitanic$Survived=as.factor(dataTitanic$Survived)
levels(dataTitanic$Survived)=c("Died","Survived")
str(dataTitanic)
tableSC=table(dataTitanic$Survived,dataTitanic$PClass)
chisq.test(tableSC)
summary(tableSC)