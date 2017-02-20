library("ISLR")
library("ggplot2")
library("MASS")
library("class")
library("gridExtra")
library("boot")
library("parallel")

data(Portfolio)
View(Portfolio)

#Define Function
alpha = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}

#Test
set.seed(1)
alpha(Portfolio, 1:100)

#Bootstrapping alpha: boot(data, statistic, Repetitions)
boot(Portfolio, alpha, R = 1000)

#Bootstrapping the parameters of a linear regression:
data(Auto)
View(Auto)

coef.fn = function(data, index){
  return(coef(lm(mpg~poly(horsepower, 2), data = data, subset = index)))
}

#Test
coef.fn(Auto, 1:190)
set.seed(373)

ptm = proc.time()
boot(Auto, coef.fn, 1000)
proc.time() - ptm


#Parallelizing
ptm = proc.time()
cl = makeCluster(4)
clusterExport(cl, 'coef.fn')

coef.boot = boot(Auto, coef.fn, 1000, parallel = 'snow', ncpus = 4, cl = cl)

stopCluster(cl)
proc.time() - ptm