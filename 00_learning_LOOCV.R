library(caret)
library(MASS)
library(dplyr)
library(randomForest)

d <- Boston
s <- summary(d$medv)
s

d$medvqrt <- case_when(
  d$medv <= 17.02 ~ 1,
  d$medv <= 21.20 ~ 2,
  d$medv <= 25.00 ~ 3,
  d$medv <= 50.00 ~ 4
)

d$medvqrt <- as.factor(d$medvqrt)
names(d)

set.seed(91388)

rf1 <- randomForest(medvqrt ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,
                    data=d,
                    importance=T,
                    proximity=T)

d$pred <- rf1$predicted

table(d$medvqrt, d$pred)
a <- (111+89+75+106)/506

l1 <- train(medvqrt ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,
            method="rf",
            data=d,
            trControl=trainControl(method="LOOCV")
            ) #~9 minutes

l1

#info <- l1$modelInfo
#v <- info$varImp
#v

trellis.par.set(caretTheme())
plot(l1)  


rf2 <- randomForest(medvqrt ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,
                    data=d,
                    importance=T,
                    proximity=T,
                    mtry=2)

d$pred2 <- rf2$predicted

table(d$medvqrt, d$pred2)
a2 <- (112+88+75+107)/506

varImpPlot(rf2)


#want a function to manually LOO rf, record pred, varimp
d$index <- seq(1:nrow(d))

save.rf <- vector(mode="list", length=nrow(d))
save.pred <- vector(mode="list", length=nrow(d))

for (ii in 1:nrow(d)){
  
  current.train <- filter(d, index != ii)
  current.test  <- filter(d, index == ii)
  
  current.rf <- randomForest(medvqrt ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,
                             data=current.train,
                             importance=T,
                             proximity=T,
                             mtry=2)
  save.rf[[ii]] <- current.rf
  
  current.pred <- predict(current.rf, newdata=current.test)
  save.pred[[ii]] <- current.pred
  
} #about 5 minutes

#extract predictions
p <- as.vector(unlist(save.pred))
d$pred3 <- as.factor(p)
qc <- filter(d, pred2 != pred3) #hmm not empty?
table(d$medvqrt, d$pred3)
a3 <- (112+92+76+109)/506
a3 #weird - slightly more accurate?

#extract varImps
v1 <- varImpPlot(save.rf[[1]])
v1
str(v1)
v1[1,2]

v2 <- varImpPlot(save.rf[[2]])
v2
v2[1,2]
