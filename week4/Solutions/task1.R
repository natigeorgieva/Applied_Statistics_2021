meanCI = function(x,sigma=sd(x),confLevel=0.95){
  if (confLevel>=1 || confLevel<=0)
  {return ("The value of confLevel should be in the interval (0,1)")}
  else{
    n = length(x)
    meanX = mean(x)
    alpha = 1 - confLevel
    se = sigma/sqrt(n)
    if (missing(sigma))       
    {cv = qt(1-alpha/2,df=n-1)}
    else {cv=qnorm(1-alpha/2)}
    return(c(lowerBound=meanX-cv*se,upperBound=meanX+cv*se))
    
  }
}
### now test it
x=rnorm(100,0,1.5)
meanCI(x)
meanCI(x,sd(x))                 
