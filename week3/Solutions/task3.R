### Задача 3 ###

install.packages("UsingR")
data("pi2000",package = "UsingR")
str(pi2000)
t=table(pi2000)
n=length(t)
barplot(t/length(pi2000),
        col=rgb(1:n,1:n,1:n,maxColorValue=n),cex.axis=1.5, cex.lab=1.5,cex.names=1.5,
        xlab = "Digit",ylab = "Probability", main = "First 2000 Digits in PI")
abline(h=1/n,col=2,lty=2,lwd=3)
chisq.test(t,p=rep(1/n,n))
