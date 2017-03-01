## This script reproduces the Cross Validation portion of the lab from Chapter 5

library("ISLR")
library("ggplot2")
library("MASS")
library("class")
library("reshape2")
library("boot")

set.seed(1)

data(Auto)
View(Auto)

#5.3.1 Validation Set Approach
p1 = ggplot(data = Auto, aes(x = horsepower, y = mpg)) + geom_point(aes(color = mpg), shape = 1) + labs(list(x =  "Horsepower", y = "MPG")) + theme_bw()

train = sample(seq_len(nrow(Auto)), size = 196)
Auto.train = Auto[train, ]
Auto.test = Auto[-train, ]

#Testing for different order of polynomials
MSE1 = rep(0,10)
for(i in 1:10){
lm.model = glm(mpg~poly(horsepower, i), data = Auto.train)
MSE1[i] = mean((Auto.test$mpg - predict(lm.model, newdata = Auto.test))^2)
}


#Trying the same thing for different random samples
set.seed(2)
train = sample(seq_len(nrow(Auto)), size = 196)
Auto.train = Auto[train, ]
Auto.test = Auto[-train, ]

#Testing for different order of polynomials
MSE2 = rep(0,10)
for(i in 1:10){
  lm.model = glm(mpg~poly(horsepower, i), data = Auto.train)
  MSE2[i] = mean((Auto.test$mpg - predict(lm.model, newdata = Auto.test))^2)
}

MSE.data = data.frame(Degree = seq(1,10,1), MSE1, MSE2) 
MSE.data = melt(MSE.data, id = "Degree")

p2 = ggplot(data = MSE.data, aes(x = Degree, y = value, color = variable)) + geom_point(shape = 1) + geom_line() + theme_bw() + scale_x_continuous(breaks = MSE.data$Degree)

#5.3.2: LOOCV
cv.err = rep(0,10)
ptm = proc.time()
for(i in 1:10){
lm.model1 = glm(mpg~poly(horsepower,i), data = Auto)
cv.err[i] = cv.glm(Auto, lm.model1)$delta[1]
}
proc.time() - ptm

p3 = ggplot(data = data.frame(cv.err), aes(x = seq_along(cv.err), y = cv.err)) + geom_point(shape = 1, color = "aquamarine4") + geom_line(color = "aquamarine4") + theme_bw() + scale_x_continuous(breaks = seq_along(cv.err))

#5.3.3: k-fold CV
set.seed(3)
cv.err1 = rep(0,10)
ptm = proc.time()
for(i in 1:10){
  lm.model1 = glm(mpg~poly(horsepower,i), data = Auto)
  cv.err1[i] = cv.glm(Auto, lm.model1, K=10)$delta[1]
}
proc.time() - ptm

#Trying the same thing for different random folds
set.seed(17)
cv.err2 = rep(0,10)
ptm = proc.time()
for(i in 1:10){
  lm.model1 = glm(mpg~poly(horsepower,i), data = Auto)
  cv.err2[i] = cv.glm(Auto, lm.model1, K=10)$delta[1]
}
proc.time() - ptm

kfcv.data = data.frame(Degree = seq(1,10,1), cv.err1, cv.err2) 
kfcv.data = melt(kfcv.data, id = "Degree")

p4 = ggplot(data = kfcv.data, aes(x = Degree, y = value, color = variable)) + geom_point(shape = 1) + geom_line() + theme_bw() + scale_x_continuous(breaks = kfcv.data$Degree)