### Задача 2 ###
data(sleep)
?sleep # extra - increase in hours of sleep; group - drug given; ID - patient ID
sleep_wide <- data.frame(
  ID=1:10, 
  group1=sleep$extra[1:10],
  group2=sleep$extra[11:20]
)
count.plus=sum((sleep_wide$group1-sleep_wide$group2)>=0);
n=length(sleep_wide$group1);
binom.test(count.plus,n,0.5,alternative = c("two.sided"))
