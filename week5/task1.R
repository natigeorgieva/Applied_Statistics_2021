#### Задача 1 ####
# Начертайте и проектирайте в различен цвят наблюденията от данните 'thuesen',
#чиито грешки в линейния модел са извън 90%-доверителен интерал, базиран на 
# нормалното разпределение и оценената стандартна грешка от модела.

alpha=0.10
model1=lm(short.velocity ~ blood.glucose,data=cleanData)
residuals1=resid(model1)
se=sqrt(sum(residuals1^2)/(model1$df.residual))
cv=qnorm(alpha/2,0,se,lower.tail=TRUE)
plot(cleanData$blood.glucose, cleanData$short.velocity, 
     col = 1+(abs(residuals1)>=abs(cv)), pch=16, xlim=c(0,20), ylim=c(0,2))
abline(model1,col=2, lwd=2)
segments(cleanData$blood.glucose,fitted(model1), 
         cleanData$blood.glucose,cleanData$short.velocity,
         col = 1+(abs(residuals1)>=abs(cv)), lty=2)
#############################################################################################



