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
d$chas2 <- d$chas

s <- sample(x=d$index, size=51, replace=F)
d$chas2 <- if_else(d$index %in% s, d$chas2*NA, d$chas2)

d2 <- select(d, -chas, -index)

d2PreProc <- caret::preProcess(d2, method="knnImpute")
meanPreProc <- d2PreProc$mean
sdPreProc <- d2PreProc$std

d2_knnImp <- predict(d2PreProc, d2) #scaled and imputed
# TODO: how to call the appropriate SD and Means instead of manual insertion
d2_knnImp$chas2_knnImp <- round((d2_knnImp$chas2 * 0.2559832) + 0.07032967)

d$chasImp <- d2_knnImp$chas2_knnImp
d$check <- d$chas - d$chasImp #incorrect 3/51 = 6%

