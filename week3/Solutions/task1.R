
### Задача 1 ###

square.approx.Pi<- function(trials){
  x=runif(trials,min = -1,max = 1);
  y=runif(trials,min = -1,max = 1);
  return(4*sum(sqrt(x^2+y^2)<=1)/trials);
}

square.approx.Pi(100000)


