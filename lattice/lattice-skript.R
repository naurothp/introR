library(lattice)
#install.packages("mlmRev")
library("mlmRev")

data(Chem97, package = "mlmRev") 

data <- Chem97[1:1000,]

pairs(data)
xtabs(~ gcsescore + factor(score), data = Chem97) 
histogram(~gcsescore|factor(score), data = Chem97) 
histogram(~gcsescore|gender, data = Chem97) 

densityplot(~  gcsescore|gender,  data  =  Chem97,  groups  =  score,  plot.points  =  FALSE,  ref  = 
              TRUE, auto.key = list(columns = 3)) 

densityplot(~  gcsescore|factor(score),  data  =  data,  groups  = gender ,  plot.points  =  FALSE,  ref  = 
              TRUE, auto.key = list(columns = 1)) 


qqmath(~ gcsescore | factor(score), Chem97, groups = gender, f.value = ppoints(100), auto.key = TRUE, type = c("p", "g"), aspect = "xy") 

qqmath(score ~ gcsescore, Chem97, groups = gender, f.value = ppoints(100), auto.key = TRUE, type = c("p", "g"), aspect = 1) 

bwplot(factor(score) ~ gcsescore | gender, Chem97) 

data(Earthquake, package = "nlme") 

xyplot(accel ~ distance, data = Earthquake) 
