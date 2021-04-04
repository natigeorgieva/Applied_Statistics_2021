install.packages("HSAUR") 
data(water,package = "HSAUR")
??water
str(water)
edit(water)

morSouth=water$mortality[water$location=="South"]
morNorth=water$mortality[water$location=="North"]
hardSouth=water$hardness[water$location=="South"]
hardNorth=water$hardness[water$location=="North"]
ks.test(morSouth,morNorth)
ks.test(hardSouth,hardNorth)

cor.test(~mortality + hardness, data = water)
t.test(mortality~location,data=water)
t.test(hardness~location,data=water)
row1=tapply(water$mortality,water$location,mean)
row2=tapply(water$hardness,water$location,mean)
tab=rbind(row1,row2)
rownames(tab)=c("mortality","hardness")
tab
