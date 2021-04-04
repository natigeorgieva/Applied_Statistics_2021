mtcars

hist(mtcars$mpg,probability = TRUE)
qqnorm(mtcars$mpg,main = "MPG Dist")
qqline(mtcars$mpg,col=2)
shapiro.test(mtcars$mpg)
