### Задача 4 ### (Chi-square test for goodness-of-fit)

roll=as.factor(sample(1:6,size=100,replace=TRUE))
(freq = table(roll))
probs = c(1,1,1,1,1,1)/6 # or use rep(1/6,6)
chisq.test(freq,p=probs)

dieFair = as.factor(sample(1:6,100,p=c(1,1,1,1,1,1)/6,replace=T))
dieBias = as.factor(sample(1:6,100,p=c(0.5,0.5,0.5,1.5,1.5,1.5)/6,replace=T))
chisq.test(dieBias,dieFair) # WRONG!!!!
tFair = table(dieFair)
tBias = table(dieBias)
chisq.test(rbind(tFair,tBias))