### R as a Calculator ###
1*2*3*4*5
sqrt(10)

pi; exp(1)  #; for Multiple commands

aVariable1<-1
aVariable2<-2
aVariable3=3  # '<-' и '=' са еквивалентни
aVariable1
print(aVariable1+10) # print() може и да се пропусне
(aVariable1+5)
print(aVariable4<-4)
avariable1+aVariable3      # Главните букви са от значение
aVariable1+aVariable3

############################################################
rm(list = ls()) # rm() -----> remove objects; ls() -----> list of all objects

### Variable types in R ###

# Variable types
numericX <- as.numeric(10.1)
integerX <- as.integer(10)     # 10L
complexX <- as.complex(10+2i)
charX <- as.character("10+2i")
factorX <- as.factor(10)
logicalX <- as.logical(TRUE)
logicalX <- as.logical(0)

# Casts
as.numeric(TRUE)
as.character(10)
as.integer(10.6)     
as.numeric("Hello")

# Class of variable
x<-2
class(x)
x+1
class(numericX)
class(integerX)
class(factorX)

# 'is.()' function
is.numeric(logicalX)
is.logical(logicalX)

# Cast factor variables
factorX <- as.factor(10)
10+as.numeric(as.character(factorX))


### Vectors ###
vectorX <- c(3,2,1) # Define a vector

vectorX[2:3]
vectorX[3:1]
vectorX[c(TRUE,FALSE,TRUE)]

vectorX <- c("one",2:10,as.factor(99))
class(vectorX)
as.integer(vectorX)

vectorX<-1:5
vectorX[1]<-"One"
class(vectorX)

vectorX[c(2,3,1)]<-c(6,7,8)
class(vectorX) #character

vectorX[]<-5:1
class(vectorX) #character

vectorX<-5:1
class(vectorX) #integer


#mean();sd();median();mad()
vectorX<-c(1:20,100.5)
vectorX <- as.integer(vectorX)
mean(vectorX)
sd(vectorX)
median(vectorX)
mad(vectorX)

# which & any
any(abs((vectorX-median(vectorX)))>3*mad(vectorX))  
which(abs((vectorX-median(vectorX)))>3*mad(vectorX))

##############################################################################
rm(list = ls()) # rm() -----> remove objects; ls() -----> list of all objects
