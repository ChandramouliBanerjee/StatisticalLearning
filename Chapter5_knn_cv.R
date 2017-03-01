## This script uses the caret package to estimate a cross validated KNN classifier using the Auto dataset. 

library("caret")
library("ISLR")
library("ggplot2")
library("MASS")
library("class")
library("gridExtra")

library("doMC")
registerDoMC(cores = 4)

data(Auto)
View(Auto)

Auto$mpg01[Auto$mpg < median(Auto$mpg)] = 0
Auto$mpg01[Auto$mpg > median(Auto$mpg)] = 1
Auto$mpg01 = as.factor(Auto$mpg01)
Auto$cylinders = as.factor(Auto$cylinders)
Auto$year = as.factor(Auto$year)

set.seed(107)
train_ind = createDataPartition(y = Auto$mpg01, p = 0.75, list = FALSE)
Auto.train = Auto[train_ind, ]
Auto.test = Auto[-train_ind, ]

Auto.train = Auto.train[, -c(1,8,9)]
Auto.test = Auto.test[, -c(1,8,9)]

#knn using caret
set.seed(400)
ptm = proc.time()
ctrl = trainControl(method = "repeatedcv", repeats = 3)
knnfit = train(mpg01 ~ ., data = Auto.train, method = "knn", trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 20)
proc.time() - ptm
knnfit
plot(knnfit)

knnpred = predict(knnfit, newdata = Auto.test)
confusionMatrix(knnpred, Auto.test$mpg01)
TeER.knn = mean(knnpred != Auto.test$mpg01)

#Choose own k values
ptm = proc.time()
ctrl = trainControl(method = "repeatedcv", repeats = 3)
knngrid = expand.grid(k = 1:10)
knnfit = train(mpg01 ~ ., data = Auto.train, method = "knn", trControl = ctrl, preProcess = c("center", "scale"), tuneGrid= knngrid)
proc.time() - ptm
knnfit
plot(knnfit)

knnpred = predict(knnfit, newdata = Auto.test)
confusionMatrix(knnpred, Auto.test$mpg01)
TeER.knn = mean(knnpred != Auto.test$mpg01)

#Experiment with different cross validation methods:
#Just k-fold cross validation
ptm = proc.time()
ctrl = trainControl(method = "cv")
knnfit = train(mpg01 ~ ., data = Auto.train, method = "knn", trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 20)
proc.time() - ptm
knnfit
plot(knnfit)

#LOOCV
ptm = proc.time()
ctrl = trainControl(method = "LOOCV")
knnfit = train(mpg01 ~ ., data = Auto.train, method = "knn", trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 20)
proc.time() - ptm
knnfit
plot(knnfit)

knnpred = predict(knnfit, newdata = Auto.test)
confusionMatrix(knnpred, Auto.test$mpg01)
TeER.knn = mean(knnpred != Auto.test$mpg01)


