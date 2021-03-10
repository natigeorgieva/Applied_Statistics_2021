### Functions ###

mean.and.sd <- function(x){
  av <- mean(x)
  sdev <- sd(x)
  return(c(mean=av, SD=sdev))
}
uniData<-runif(1000,min = 0,max = 1)  # runif - генерира равномерно разпределени сл.в.
1/2; sqrt(1/12)
mean.and.sd(uniData)

### if, else, for, repeat, while, break, next ###
# if Statement
a<-sample((-100):100,1)
if (a>0) print(paste("Числото",a,"е положително!",sep = " "))

# if () {} else {}
a<-sample(100:1000,1)
b=2
deliSe=paste("Числото",a,"се дели на",b,"!",sep = " ")
neSeDeli=paste("Числото",a,"не се дели на",b,"!",sep = " ")

if (a%%b==0) { print(deliSe) 
} else {print(neSeDeli)}

#ifelse()
ifelse(a%%b==0, deliSe, neSeDeli)

# for(... in ...){}
n=30
for (i in 1:n){
  a<-sample(100:1000,1)
  if (a%%2==0 & a%%3==0) print(paste("Числото",a,"се дели на 6!",sep = " "))
}

# repeat + break
a=1
repeat {
  print(a)
  a=a+1
  if (a==10){
    break
  }
}

# while
a=1
while (a<10){
  print(a)
  a=a+1
}

# next
for(i in 1:10){
  if(i%%2==0) { print(i)
  } else next
}

