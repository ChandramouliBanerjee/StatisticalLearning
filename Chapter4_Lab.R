library("ISLR")
library("ggplot2")
library("MASS")
library("class")

data(Smarket)
View(Smarket)

cor(Smarket[, -9])

#Plotting Volume Data
p1 = ggplot(data = Smarket, aes(x = seq_along(Smarket$Volume), y = Smarket$Volume)) + geom_line(color = "dodgerblue3") + labs(list(x =  "Index", y = "Volume")) + theme_bw()

attach(Smarket)
#Running Logit
glm.model1 = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial(link = 'logit'))
summary(glm.model1)

#Predictions and Confusion Matrix
glm.probs = predict(glm.model1, type = "response")
contrasts(Direction)
glm.pred = rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)

#Training Error Rate
TrER.glm = mean(glm.pred != Direction)

#Alternative: Training and Testing by Holding Data Out. 
train = (Year < 2005)
Smarket.test = Smarket[!train, ]

glm.model2 = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial(link = 'logit'), subset = train)
summary(glm.model2)

#Now predicting using the held out data
glm.probs = predict(glm.model2, newdata = Smarket.test, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Smarket.test$Direction)

#Test Error Rate 
TeER.glm = mean(glm.pred != Smarket.test$Direction)

#LDA
lda.model1 = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.model1
lda.pred = predict(lda.model1, newdata = Smarket.test)
table(lda.pred$class, Smarket.test$Direction)

TeER.lda = mean(lda.pred$class != Smarket.test$Direction)

ggplot(data = Smarket.test, aes(x = Smarket.test$Lag1, y = Smarket.test$Lag2)) + geom_point(aes(color = Smarket.test$Direction, shape = Smarket.test$Direction)) + labs(list(x =  "Lag1", y = "Lag2")) + theme_bw()
#Can't figure out how to plot decision boundary!

#QDA
qda.model1 = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.model1
qda.pred = predict(qda.model1, newdata = Smarket.test)
table(qda.pred$class, Smarket.test$Direction)

TeER.qda = mean(qda.pred$class != Smarket.test$Direction)

#KNN
#Must generate data in format for KNN first
train.X = cbind(Smarket$Lag1, Smarket$Lag2)[train, ]
test.X = cbind(Smarket$Lag1, Smarket$Lag2)[!train, ]
train.Direction = Smarket$Direction[train]

set.seed(1)

#k = 1
knn.pred1 = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred1, Smarket.test$Direction)

TeER.knn1 = mean(knn.pred != Smarket.test$Direction)


#k = 3
knn.pred2 = knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred2, Smarket.test$Direction)

TeER.knn2 = mean(knn.pred2 != Smarket.test$Direction)

#Alternative
TeER.knn = rep(0,20)
for(i in 1:20){
  knn.pred = knn(train.X, test.X, train.Direction, k = i)
  TeER.knn[i] = mean(knn.pred != Smarket.test$Direction)
}
ggplot(data = data.frame(TeER.knn), aes(x = seq_along(TeER.knn), y = TeER.knn)) + geom_line(color = "dodgerblue3") + labs(list(x =  "k", y = "Test Error Rate")) + theme_bw() + scale_x_continuous(breaks = seq_along(TeER.knn)) + scale_y_continuous(breaks = TeER.knn)
