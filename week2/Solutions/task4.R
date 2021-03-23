### Задача 4 ###

rand.Circle <- function(n,x_0=0,y_0=0,r=1){
  theta = 2*pi*runif(n,0,1);
  R = r*sqrt(runif(n,0,1));
  x = x_0 + R*cos(theta);
  y = y_0 + R*sin(theta);
  return(cbind(x,y))
}
plot(rand.Circle(1000,0,0,4),asp=1)
