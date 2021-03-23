### Задача 2 ### 

p=runif(1000,0,1)>9/10;
simData=rnorm(1000,0+(10-0)*p,sqrt(4)+(sqrt(9)-sqrt(4))*p)

hist(simData,prob=TRUE,xlab = 'Observations')
curve(dnorm(x, mean=mean(simData), sd=sd(simData)), add=TRUE, col=2,lwd=2)
d=density(simData,bw="SJ")
lines(d,col='blue',lwd=2)

any(abs((simData-median(simData)))>3*mad(simData))  
