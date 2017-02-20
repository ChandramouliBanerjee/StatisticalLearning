library("ISLR")
library("ggplot2")
library("MASS")
library("class")
library("gridExtra")
library("boot")
library("parallel")
library("pROC")

bwght = read.csv("/home/ron/Downloads/excelfiles/bwght.csv", header = TRUE)

#Recoding missing values as NA
bwght[bwght == "."] = NA

#Redefining Education as numeric
bwght$fatheduc = as.numeric(bwght$fatheduc)
bwght$motheduc = as.numeric(bwght$motheduc)

#Redefining Male and White as factors
bwght$male = as.factor(bwght$male)
bwght$white = as.factor(bwght$white)


#Removing columns with missing values
bwght = bwght[complete.cases(bwght),]

#Defining a binary variable for smoking while pregnant. 
bwght$smokes = ifelse(bwght$cigs > 0, 1, 0)
bwght$smokes = as.factor(bwght$smokes)

#Splitting the data into training and test sets
smpsize = floor(0.75 * nrow(bwght))
set.seed(123)
train_ind = sample(seq_len(nrow(bwght)), size = smpsize)

bwght.train = bwght[train_ind, ]
bwght.test = bwght[-train_ind, ]

#1) Test two different specifications for logit model. Calculate cross validated classfication error for each:

#1.1: Baseline, LOOCV and 10-fold CV. 
glm.model1 = glm(smokes ~ motheduc + white + lfaminc, data = bwght, family = binomial(link = 'logit'))
summary(glm.model1)$coefficients

loocv.err1 = cv.glm(bwght, glm.model1)$delta[1]
loocv.err1.cost = cv.glm(bwght, cost = function(r, pi = 0) mean(abs(r-pi) > 0.5), glm.model1)$delta[1]
kfcv.err1 = cv.glm(bwght, glm.model1, K = 10)$delta[1]

#1.2: Baseline + fatheduc + cigprice, LOOCV, 10-fold, No Cost Function
glm.model2 = glm(smokes ~ motheduc + white + lfaminc + fatheduc + cigprice, data = bwght, family = binomial(link = 'logit'))
summary(glm.model2)$coefficients

loocv.err2 = cv.glm(bwght, glm.model2)$delta[1]
kfcv.err2 = cv.glm(bwght, glm.model2, K = 10)$delta[1]

#The second model seems to be overfitting. Test error rate of first model:
glm.model1.train = glm(smokes ~ motheduc + white + lfaminc, data = bwght.train, family = binomial(link = 'logit'))
glm.probs = predict(glm.model1.train, newdata = bwght.test, type = "response")
glm.pred = rep(0, nrow(bwght.test))
glm.pred[glm.probs > 0.5] = 1
table(glm.pred, bwght.test$smokes)

TeER.glm = mean(glm.pred != bwght.test$smokes)

bwght.test$probs = glm.probs

#ROC Curves
g = roc(smokes ~ glm.probs, data = bwght.test)
plot(g)

#2) Bootstrap the first model:
coef.fn = function(data, index){
  return(coef(glm(smokes ~ motheduc + white + lfaminc, data = bwght, family = binomial(link = 'logit'), subset = index)))
}

coef.fn(bwght.train, 1:893)

set.seed(373)

cl = makeCluster(4)
clusterExport(cl, c('coef.fn', 'bwght'))

coef.boot = boot(data = bwght, coef.fn, 1000, parallel = 'snow', ncpus = 4, cl = cl)

stopCluster(cl)

coef.boot

#3) kNN: Cross Validated using the caret package
library("doMC")
registerDoMC(cores = 4)

set.seed(107)
train_ind = createDataPartition(y = bwght$smokes, p = 0.75, list = FALSE)
bwght.train = bwght[train_ind, ]
bwght.test = bwght[-train_ind, ]

set.seed(400)
ctrl = trainControl(method = "repeatedcv", repeats = 3)
knnfit = train(smokes ~  motheduc + white + lfaminc, data = bwght.train, method = "knn", trControl = ctrl, preProcess = c("center", "scale"), tuneLength = 20)

knnfit
plot(knnfit)

knnpred = predict(knnfit, newdata = bwght.test)
confusionMatrix(knnpred, bwght.test$smokes)
TeER.knn = mean(knnpred != bwght.test$smokes)

#ROC Curve
probs = predict(knnfit, newdata = bwght.test, type = "prob")
h = roc(bwght.test$smokes ~ probs[, "0"] , data = bwght.test)
plot(h)

#Choose no of neighbors manually
set.seed(500)
knngrid = expand.grid(k = 1:25)
ctrl = trainControl(method = "repeatedcv", repeats = 3)
knnfit = train(smokes ~  motheduc + white + lfaminc, data = bwght.train, method = "knn", trControl = ctrl, preProcess = c("center", "scale"), tuneGrid= knngrid)
knnfit
plot(knnfit)