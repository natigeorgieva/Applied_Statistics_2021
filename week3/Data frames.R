### Data frames ###
v1<-1:10
v2<-seq(0,1,length=10)
v3<-letters[1:10]
d1<-data.frame(vector1=v1,vector2=v2,ABC=v3)
str(d1)
d1$ABC
d1$binary <-c(rep(TRUE,times=5),rep(FALSE,times=5))

d2<-data.frame(vector1=v1,ABC=v3,stringsAsFactors =FALSE)
d2$factor<-as.factor(rep(c("male","female"),times=5))
str(d2)

# mtcars dataset
data(mtcars)
mtcars
head(mtcars)
edit(mtcars)
str(mtcars)
colnames(mtcars)[1:5]
rownames(mtcars)[1:5]
mtcars$mpg
mtcars[,1]
mtcars[1,]

##############################################################################################

#### Import/Export ####

edit(mtcars)
data()
data("longley")
edit(longley)

save(mtcars,file = "Data/testData1.rda")
save(mtcars,iris,file = "Data/testData2.rda")
rm(list=ls())
load("Data/testData2.rda")
ls()
list.files()
file.path("Data/testData2.rda")

install.packages("data.table")        # install package
installed.packages()                  # installed packages
#update.packages()                     # keep up to date all packages
library(data.table)                   # load package 
require(data.table)                   # load package 
?data.table                           # read
?fread                                # read

write.table(mtcars,file = "Data/mmtcars.txt", row.names = FALSE)
df <- read.table("Data/mmtcars.txt")

write.csv(women,"Data/wData.csv")
read.csv("Data/wData.csv")

x<-read.table("https://s3.amazonaws.com/assets.datacamp.com/blog_assets/test.txt")
x
str(x)
nrow(x)
ncol(x)
colnames(x)<-c("Var1", "Var2","Var3") 
rownames(x)<-x[,1]
names(x)
y<-x[1:5,2:3]
y

install.packages("readxl")
library(readxl)
datasets<-system.file("extdata/datasets.xlsx",package = "readxl")
data_iris=read_excel(datasets)