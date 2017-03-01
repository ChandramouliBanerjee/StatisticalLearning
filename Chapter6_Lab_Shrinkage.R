library("ISLR")
library("ggplot2")
library("reshape2")
library("glmnet")

data(Hitters)
View(Hitters)

Hitters = Hitters[complete.cases(Hitters),]

x = model.matrix(Salary~.-1,data=Hitters) 
y = Hitters$Salary

#Ridge
fit.ridge = glmnet(x, y, alpha = 0)
plot(fit.ridge, xvar = "lambda", label = TRUE)

cv.ridge = cv.glmnet(x, y, alpha=0)
plot(cv.ridge)

#Lasso
fit.lasso = glmnet(x,y)
plot(fit.lasso, xvar="lambda", label=TRUE)
cv.lasso = cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

#Choosing on the basis of Test Error Rate
train = sample(seq_len(nrow(Hitters)), 180)
lasso.tr = glmnet(x[train,],y[train])
lasso.tr
pred=predict(lasso.tr,x[-train,])
dim(pred)
rmse= sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr,s=lam.best)