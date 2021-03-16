### Корелация и независимост ###

ndata<-rnorm(1000,mean = mean(mtcars$mpg), sd= sd(mtcars$mpg))
qqnorm(ndata,main = "Sample Dist")
qqline(ndata,col=2)
ks.test(mtcars$mpg,ndata)
ks.test(mtcars$mpg,"pnorm",mean(mtcars$mpg),sd(mtcars$mpg))

cor.test(normal,tdist,method = ("pearson"))
cor.test(mtcars$hp,mtcars$cyl,method = ("pearson"))
cor.test(mtcars$hp,mtcars$cyl,method = ("kendall"))
cor.test(mtcars$hp,mtcars$cyl,method = ("spearman"))

mean(mtcars$cyl)
t.test(mtcars$cyl,alternative = ("two.sided"),mu=6)
mean(mtcars$mpg)
t.test(mtcars$mpg,alternative = ("less"),mu=20)

t.test(mpg~am,data=mtcars)
t.test(mpg~am,data=mtcars,var.equal=TRUE)
var.test(mpg~am,data=mtcars)

install.packages("HSAUR")         # Install package for datasets