# explore imputation options within CARET
# preProcess function, K-NN or bagged trees

library(caret)
library(MASS)
library(dplyr)
library(RANN)

set.seed(seed=45226)

d <- MASS::Boston #predict MEDV from 13 factors


str(d) #CHAS and RAD are categorical, others continuous

c <- cor(x=d[1:13], y=d[14], method="spearman") #LSTAT, RM seem most correlated with MEDV
                                                #CHAS, BLACK least correlated
plot(x=d$lstat, y=d$medv)
plot(x=d$rm, y=d$medv)

#TODO ROUND not needed for continuous predictors
#TODO: functionalize and clean up the above (%missing, var name, reverse scaling, etc.)

learnImpute <- function (VAR1, VAR2, VAR3, VAR4){
  #VAR1 = variable to perform random removal and imputation upon
  #VAR2 = % of observations for VAR1 to randomly remove and impute, as a decimal
  #VAR3 = imputation method {"knnImpute", "bagImpute", "medianImpute"}
  #VAR4 = dataset
  
  d2 <<- VAR4
  d2$index <- seq(from=1, to=nrow(d2), by=1)

  sampsize <<- round(nrow(VAR4)*VAR2)
  
  s <<- sample(x=d2$index, size=sampsize, replace=F)
  
  d2$imputed <- if_else(d2$index %in% s, d2[,VAR1]*NA, VAR4[,VAR1])
  
  d2 <- select(d2, -index)
  
  d2PreProc <<- caret::preProcess(d2, method=VAR3)
  meanPreProc <<- d2PreProc$mean
  sdPreProc <<- d2PreProc$std
  
  d2_Imp <<- predict(d2PreProc, d2) #scaled and imputed
  d2_Imp$imputed2 <- round((d2_Imp$imputed * sdPreProc["imputed"]) + meanPreProc["imputed"])
  
  d2$imputed2 <- d2_Imp$imputed2
  d2$check <- d2[,VAR1] - d2$imputed2 
  incorrect <<- sum(d2$check)/sampsize*100
}

learnImpute("chas", 0.10, "knnImpute", d)
learnImpute("chas", 0.01, "knnImpute", d)

save <- vector(mode="numeric", length=100)
for (ii in 1:100){
  learnImpute("chas", ii/100, "knnImpute", d)
  save[ii] <- incorrect
}

save
plot(save, main="knnImpute")

#TODO resolve errors with bagged and median imputations
save2 <- vector(mode="numeric", length=100)
save2 <- save2-100
for (ii in 1:100){
  learnImpute("chas", ii/100, "bagImpute", d)
  save2[ii] <- incorrect
}

plot(save2, main="bagImpute")

save3 <- vector(mode="numeric", length=100)
save3 <- save3-100
for (ii in 1:100){
  learnImpute("chas", ii/100, "medianImpute", d)
  save3[ii] <- incorrect
}

plot(save3, main="medianImpute")

save4 <- vector(mode="numeric", length=100)
save4 <- save4-100
