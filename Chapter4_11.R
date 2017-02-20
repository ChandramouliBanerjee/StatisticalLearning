library("ISLR")
library("ggplot2")
library("MASS")
library("class")
library("gridExtra")

data(Auto)
View(Auto)

#a) Generate Dependent Variable
Auto$mpg01[Auto$mpg < median(Auto$mpg)] = 0
Auto$mpg01[Auto$mpg > median(Auto$mpg)] = 1

Auto$mpg01 = as.factor(Auto$mpg01)
Auto$cylinders = as.factor(Auto$cylinders)
Auto$year = as.factor(Auto$year)
attach(Auto)

#b) Visualizations
p1 = ggplot(data = Auto, aes(x = mpg01, y = displacement, fill = mpg01)) + geom_boxplot() + theme_bw()
p2 = ggplot(data = Auto, aes(x = mpg01, y = horsepower, fill = mpg01)) + geom_boxplot() + theme_bw()
p3 = ggplot(data = Auto, aes(x = mpg01, y = weight, fill = mpg01)) + geom_boxplot() + theme_bw()
p4 = ggplot(data = Auto, aes(x = mpg01, y = acceleration, fill = mpg01)) + geom_boxplot() + theme_bw()

grid.arrange(p1, p2, p3, p4, ncol=2, nrow =2)

plot(as.factor(year), as.factor(mpg01), ylab = "mpg01", xlab = "year")

plot(as.factor(cylinders), as.factor(mpg01), ylab = "Cylinders", xlab = "year")

#c) Splitting Data into Training (75%) and Test (25%) Sets
smpsize <- floor(0.75 * nrow(Auto))
set.seed(123)
train_ind <- sample(seq_len(nrow(Auto)), size = smpsize)

Auto.train <- Auto[train_ind, ]
Auto.test <- Auto[-train_ind, ]

#d) LDA
lda.model1 = lda(mpg01 ~ displacement + acceleration + horsepower + weight, data = Auto.train)
lda.model1
lda.pred = predict(lda.model1, newdata = Auto.test)
table(lda.pred$class, Auto.test$mpg01)

TeER.lda = mean(lda.pred$class != Auto.test$mpg01)

#e) QDA
qda.model1 = qda(mpg01 ~ displacement + acceleration + horsepower + weight, data = Auto.train)
qda.model1
qda.pred = predict(qda.model1, newdata = Auto.test)
table(qda.pred$class, Auto.test$mpg01)

TeER.qda = mean(qda.pred$class != Auto.test$mpg01)

#f) Logit: Can add categorical predictors; add year because cylinders is same as horsepower
glm.model1 = glm(mpg01 ~ displacement + acceleration + horsepower + weight + year, data = Auto.train, family = binomial(link = 'logit'))
summary(glm.model1)

glm.probs = predict(glm.model1, newdata = Auto.test, type = "response")
glm.pred = rep(0, 98)
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, Auto.test$mpg01)

TeER.glm = mean(glm.pred != Auto.test$mpg01)

#g) KNN
#k = 1
train.X = Auto.train[, -c(1,8,9,10)]
test.X= Auto.test[, -c(1,8,9,10)]
train.mpg = Auto.train$mpg01


knn.pred1 = knn(train.X, test.X, train.mpg, k = 1)

table(knn.pred1, Auto.test$mpg01)
TeER.knn1 = mean(knn.pred1 != Auto.test$mpg01)

#k = 3
knn.pred2 = knn(train.X, test.X, train.mpg, k = 3)

table(knn.pred2, Auto.test$mpg01)
TeER.knn2 = mean(knn.pred2 != Auto.test$mpg01)

#k = 8
knn.pred3 = knn(train.X, test.X, train.mpg, k = 8)

table(knn.pred3, Auto.test$mpg01)
TeER.knn3 = mean(knn.pred3 != Auto.test$mpg01)

#Alternative
set.seed(2)
TeER.knn = rep(0,20)
for(i in 1:20){
  knn.pred = knn(train.X, test.X, train.mpg, k = i)
  TeER.knn[i] = mean(knn.pred != Auto.test$mpg01)
}
ggplot(data = data.frame(TeER.knn), aes(x = seq_along(TeER.knn), y = TeER.knn)) + geom_line(color = "dodgerblue3") + labs(list(x =  "k", y = "Test Error Rate")) + theme_bw() + scale_x_continuous(breaks = pretty(seq_along(TeER.knn), n = 20)) + scale_y_continuous(breaks = TeER.knn)

