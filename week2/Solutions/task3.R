### Задача 3 ### 


a<-c(8,3,8,7,15,9,12,4,9,10,5,1)
matrr<-matrix(a,6,2) # create matrix 6x2
matrr
rownames(matrr)<-c("r1","r2","r3","r4","r5","r6") # named rows
matrr 
coll=seq(1,12,2) # create a vector from 1 to 12 with step 2 
# so this seq. contains odd numbers 
matr2=cbind(matrr,coll) 
matr2  #create matrix 6x3 where the last column is named

matr2=cbind(matrr,seq(1,12,2)) # create a matrix 6x3 where thee 3column is without name
matr2
ord=order(matr2[,1]) 
matr2[ord,] # sorting   by 1 coloumn

