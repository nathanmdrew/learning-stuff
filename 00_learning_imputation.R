# explore imputation options within CARET
# preProcess function, K-NN or bagged trees

library(caret)
library(MASS)
library(dplyr)
library(RANN)

set.seed(seed=45226)

d <- MASS::Boston #predict MEDV from 13 factors

d$index <- seq(from=1, to=nrow(d), by=1)

str(d) #CHAS and RAD are categorical, others continuous

c <- cor(x=d[1:13], y=d[14], method="spearman") #LSTAT, RM seem most correlated with MEDV
                                                #CHAS, BLACK least correlated
plot(x=d$lstat, y=d$medv)
plot(x=d$rm, y=d$medv)

#randomly pick ~10% of CHAS to be missing
d$chas2 <- d$chas #VAR1 -> variable to impute

sampsize <- round(nrow(d)*0.10) #VAR2 -> %missing

s <- sample(x=d$index, size=sampsize, replace=F)
d$chas2 <- if_else(d$index %in% s, d$chas2*NA, d$chas2)

d2 <- select(d, -chas, -index)

d2PreProc <- caret::preProcess(d2, method="knnImpute") #VAR3 -> impute method
meanPreProc <- d2PreProc$mean
sdPreProc <- d2PreProc$std

d2_knnImp <- predict(d2PreProc, d2) #scaled and imputed
d2_knnImp$chas2_knnImp <- round((d2_knnImp$chas2 * sdPreProc["chas2"]) + meanPreProc["chas2"])
#TODO ROUND not needed for continuous predictors

d$chasImp <- d2_knnImp$chas2_knnImp
d$check <- d$chas - d$chasImp #incorrect 3/51 = 6%
incorrect <- sum(d$check)/sampsize*100



#TODO: functionalize and clean up the above (%missing, var name, reverse scaling, etc.)
learnImpute <- function (VAR1, VAR2, VAR3, VAR4){
  #VAR1 = variable to perform random removal and imputation upon
  #VAR2 = % of observations for VAR1 to randomly remove and impute, as a decimal
  #VAR3 = imputation method {"knnImpute", "bagImpute", "medianImpute"}
  #VAR4 = dataset; must have a variable named INDEX for rownums
  
  d2 <<- VAR4

  sampsize <<- round(nrow(VAR4)*VAR2)
  
  s <<- sample(x=VAR4$index, size=sampsize, replace=F)
  
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
  return(incorrect)
}

learnImpute("chas", 0.10, "knnImpute", d)


