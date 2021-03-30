########### Statistical tests for normality ##########


normal<-rnorm(100)
hist(normal,probability = TRUE)
qqnorm(normal,main = "Normal Dist")
qqline(normal,col=2)

tdist<-rt(100,df=3)
hist(tdist,probability = TRUE)
qqnorm(tdist,main = "T Dist")
qqline(tdist,col=2)

shapiro.test(normal)
shapiro.test(tdist)
