### R code from vignette source 'svmdoc.Rnw'

###################################################
### code chunk number 1: svmdoc.Rnw:140-149
###################################################
library(e1071)
library(rpart)
data(Glass, package="mlbench")

## split data into a train and test set
index     <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset   <- Glass[testindex,]
trainset  <- Glass[-testindex,]


###################################################
### code chunk number 2: svmdoc.Rnw:154-157
###################################################
## svm
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred  <- predict(svm.model, testset[,-10])


###################################################
### code chunk number 3: svmdoc.Rnw:162-165
###################################################
## rpart
rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred  <- predict(rpart.model, testset[,-10], type = "class")


###################################################
### code chunk number 4: svmdoc.Rnw:168-173
###################################################
## compute svm confusion matrix
table(pred = svm.pred, true = testset[,10])

## compute rpart confusion matrix 
table(pred = rpart.pred, true = testset[,10])


###################################################
### code chunk number 5: svmdoc.Rnw:178-213
###################################################
library(xtable)
rp.acc <- c()
sv.acc <- c()
rp.kap <- c()
sv.kap <- c()
reps <- 10
for (i in 1:reps) {
  ## split data into a train and test set
  index     <- 1:nrow(Glass)
  testindex <- sample(index, trunc(length(index)/3))
  testset   <- na.omit(Glass[testindex,])
  trainset  <- na.omit(Glass[-testindex,])
  
  ## svm
  svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
  svm.pred  <- predict(svm.model, testset[,-10])
  tab <- classAgreement(table(svm.pred, testset[,10]))
  sv.acc[i] <- tab$diag
  sv.kap[i] <- tab$kappa
  
  ## rpart
  rpart.model <- rpart(Type ~ ., data = trainset)
  rpart.pred  <- predict(rpart.model, testset[,-10], type = "class")
  tab <- classAgreement(table(rpart.pred, testset[,10]))
  rp.acc[i] <- tab$diag
  rp.kap[i] <- tab$kappa

}
x <- rbind(summary(sv.acc), summary(sv.kap), summary(rp.acc), summary(rp.kap))
rownames <- c()
tab <- cbind(rep(c("svm","rpart"),2), round(x,2))
colnames(tab)[1] <- "method"
rownames(tab) <- c("Accuracy","","Kappa"," ")
xtable(tab, label = "tab:class", caption = "Performance of \\texttt{svm()} and\
       \\texttt{rpart()} for classification (10 replications)")


###################################################
### code chunk number 6: svmdoc.Rnw:226-245
###################################################
library(e1071)
library(rpart)
data(Ozone, package="mlbench")

## split data into a train and test set
index     <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset   <- na.omit(Ozone[testindex,-3])
trainset  <- na.omit(Ozone[-testindex,-3])

## svm
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
svm.pred  <- predict(svm.model, testset[,-3])
crossprod(svm.pred - testset[,3]) / length(testindex)

## rpart
rpart.model <- rpart(V4 ~ ., data = trainset)
rpart.pred  <- predict(rpart.model, testset[,-3])
crossprod(rpart.pred - testset[,3]) / length(testindex)


###################################################
### code chunk number 7: svmdoc.Rnw:248-271
###################################################
rp.res <- c()
sv.res <- c()
reps <- 10
for (i in 1:reps) {
  ## split data into a train and test set
  index     <- 1:nrow(Ozone)
  testindex <- sample(index, trunc(length(index)/3))
  testset   <- na.omit(Ozone[testindex,-3])
  trainset  <- na.omit(Ozone[-testindex,-3])
  
  ## svm
  svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
  svm.pred  <- predict(svm.model, testset[,-3])
  sv.res[i] <- crossprod(svm.pred - testset[,3]) / length(testindex)
  
  ## rpart
  rpart.model <- rpart(V4 ~ ., data = trainset)
  rpart.pred  <- predict(rpart.model, testset[,-3])
  rp.res[i] <- crossprod(rpart.pred - testset[,3]) / length(testindex)
}
xtable(rbind(svm = summary(sv.res), rpart = summary(rp.res)), 
       label = "tab:reg", caption = "Performance of \\texttt{svm()} and\
       \\texttt{rpart()} for regression (Mean Squared Error, 10 replications)")


