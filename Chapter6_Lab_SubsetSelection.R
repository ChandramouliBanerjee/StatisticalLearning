library("ISLR")
library("ggplot2")
library("reshape2")

data(Hitters)
View(Hitters)

Hitters = Hitters[complete.cases(Hitters),]

#Use package `leaps` to evaluate all the best-subset models.
library("leaps")
regfit1 = regsubsets(Salary~.,data=Hitters)
summary(regfit1)

#Increasing Number of Variables in Subset:
regfit2=regsubsets(Salary~.,data=Hitters, nvmax=19)
reg.summary=summary(regfit2)

#Best Model according to Various Selection Criteria
Plot.data1 = data.frame(d = seq_along(reg.summary$cp), reg.summary$rsq, reg.summary$adjr2)
Plot.data1 = melt(Plot.data1, id = "d")

p1 = ggplot(data = Plot.data1, aes(x = d, y = value, color = variable)) + geom_point(shape = 1) + geom_line() + theme_bw() + scale_x_continuous(breaks = Plot.data1$d) + labs(list(x =  "No. of Covariates", y = ""))

#Of course R-squared rises as more variables are added. So use measures penalized for number of regressors.

Plot.data2 = data.frame(d = seq_along(reg.summary$cp), reg.summary$cp, reg.summary$bic)
Plot.data2 = melt(Plot.data2, id = "d")

p2 = ggplot(data = Plot.data2, aes(x = d, y = value, color = variable)) + geom_point(shape = 1) + geom_line() + theme_bw() + scale_x_continuous(breaks = Plot.data2$d) + labs(list(x =  "No. of Covariates", y = ""))

#Find out which model is the best:
which.min(reg.summary$cp)
which.min(reg.summary$bic)

#Coefficeints for best models: How to get standard errors and p-values??
coef(regfit2, 10)
coef(regfit2, 6)

#Forward and Backward Selection:
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

which.min(summary(regfit.fwd)$cp)
which.min(summary(regfit.fwd)$bic)

regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

which.min(summary(regfit.bwd)$cp)
which.min(summary(regfit.bwd)$bic)

#Validation Set Approaches:
set.seed(1)
train = sample(seq_len(nrow(Hitters)), 180)
Hitters.train = Hitters[train,]
Hitters.test = Hitters[-train,]

regfit.best = regsubsets(Salary~., data = Hitters.train , nvmax=19)

#Empty vector for Prediction Errors
cv.errors = rep(0,19)
x.test = model.matrix(Salary~.,data = Hitters.test)

for(i in 1:19){
  coefi = coef(regfit.best, id=i)
  pred = x.test[,names(coefi)]%*%coefi
  cv.errors[i]=sqrt(mean((Hitters.test$Salary-pred)^2))
}

Plot.data3 = data.frame(d = seq(1,19,1), cv.errors, sqrt(regfit.best$rss[-1]/180))
Plot.data3 = melt(Plot.data3, id = "d")

p3 = ggplot(data = Plot.data3, aes(x = d, y = value, color = variable)) + geom_point(shape = 1) + geom_line() + theme_bw() + scale_x_continuous(breaks = Plot.data3$d) + labs(list(x =  "No. of Covariates", y = "RMSE"))

#10-fold CV:
set.seed(11)
folds = sample(rep(1:10, length=nrow(Hitters)), replace = TRUE)
folds
table(folds)

kfcv.errors = matrix(0,10,19)
for(k in 1:10){
  best.fit=regsubsets(Salary~., data = Hitters[folds != k,], nvmax=19)
  x.test = model.matrix(Salary~.,data = Hitters[folds == k,])
  for(i in 1:19){
    coefi = coef(regfit.best, id=i)
    pred = x.test[,names(coefi)]%*%coefi
    kfcv.errors[k,i] = sqrt(mean((Hitters$Salary[folds == k]-pred)^2))
  }
}
mean.kfcv.errors = apply(kfcv.errors,2,mean)

p4 = ggplot(data = data.frame(mean.kfcv.errors), aes(x = seq_along(mean.kfcv.errors), y = mean.kfcv.errors))  + geom_line(color = "aquamarine") + geom_point(shape = 1, color = "aquamarine4") + theme_bw() + scale_x_continuous(breaks = seq_along(mean.kfcv.errors)) + labs(list(x =  "No. of Covariates", y = "RMSE"))

#Best Model by 10-fold CV
coef(regfit.best, which.min(mean.kfcv.errors))