### Задача 1 ### 
?rnorm # повече за rnorm, който не знае
normData<-rnorm(10000,0,1) # генерираме: n=10000, mean=0, sd=1

hist(normData,prob=TRUE,xlab = 'Simulated observations') #добра практика е да видите как изглеждат нашите данни на графика
curve(dnorm(x, mean=mean(normData), sd=sd(normData)), add=TRUE, col=2,lwd=2) # Изчертава крива, съответстваща на функция през интервала.

?dnorm # Density, distribution function, quantile function and random generation for the normal distribution with mean equal to mean and standard deviation equal to sd.
?curve #Draws a curve corresponding to a function over the interval [from, to]. curve can plot also an expression in the variable xname, default x.

mean(normData) #Generic function for the (trimmed) arithmetic mean.
median(normData) 
mad(normData) #Median Absolute Deviation
sd(normData) #Standard Deviation


cutoff<-median(normData)+3*mad(normData)
