rm(list=ls())
require(gbm)

#setup data
setwd("C:\\Projects\\R")
data <- read.table(file="TrainingDataset.csv",header=TRUE, sep=",")
testData <- read.table(file="TestDataset.csv",header=TRUE, sep=",")

X <- data[,14:29]
Xtest <- testData[,3:18]
Ytestrows <- nrow(Xtest)
#last column is the total sales for the year
Ytest <- matrix(nrow = Ytestrows , ncol = 14)
YmonthlyFractions <- matrix(nrow = 1, ncol = 12)

#Y - labels a single column from data
Y <- as.numeric(data[,1])
Y <- log(Y)
Y[is.na(Y)] <- 0.0

#Y - labels as sum of all month sales
for(i in 1:nrow(data)){
  Y[i] <- log(sum(data[i,1:12],na.rm=TRUE))
}

nc1 <- ncol(X)
d1 <- dimnames(X)[[2]]

#cleanup data - factor variables are still problematic on prediction
# idxCat <- c(1,18)
# for(i in 1:length(idxCat)) {
#   v <- as.factor(X[,idxCat[i]])
#   X[,idxCat[i]] <- v
# 
#   v <- as.factor(Xtest[,idxCat[i]])
#   Xtest[,idxCat[i]] <- v
# }

for(i in 1:16){
  v <- is.nan(X[,i])
  if(sum(v)>0)
  {
    meanx <- mean(X[!v,i])
    X[v,i] <- meanx
    X <- cbind(X,as.factor(v))
  }
  else
  {
    X <- cbind(X, as.factor(FALSE))      
  }
}

for(i in 1:16){
  v <- is.nan(Xtest[,i])
  if(sum(v)>0)
  {
    meanxtest <- mean(Xtest[!v,i])
    Xtest[v,i] <- meanxtest
    Xtest <- cbind(Xtest,as.factor(v))
  }
  else
  {
    Xtest <- cbind(Xtest, as.factor(FALSE))      
  }
}

newCols <- paste("V",1:(ncol(X)-nc1),sep="")
dimnames(X)[[2]] <- c(d1,newCols)

newCols <- paste("V",1:(ncol(Xtest)-nc1),sep="")
dimnames(Xtest)[[2]] <- c(d1,newCols)

#estimate and predict total sales for data in the training set
gdata <- cbind(Y,X)
ntrees <- 4000
depth <- 5
minObs <- 10
shrink <- 0.001

folds <- 10
mo1gbm <- gbm(Y~. ,data=gdata,
              distribution = "gaussian",
              n.trees = ntrees,
              shrinkage = shrink,
              cv.folds = folds)

gbm.perf(mo1gbm,method="cv")
sqrt(min(mo1gbm$cv.error))
which.min(mo1gbm$cv.error)

Ytest[,14] <- exp(predict.gbm(mo1gbm, newdata=Xtest, n.trees = ntrees))

#estimate and predict the fraction of total sales that each month will have
for( i in 2:13 ) {
  Ymonth <- data[,i-1]/exp(Y)
  Ymonth[is.na(Ymonth)] <- 0.0
  
  gdata <- cbind(Ymonth,X)
  
  mo1gbm <- gbm(Ymonth~. ,
                data=gdata,
                distribution = "gaussian",
                n.trees = ntrees,
                shrinkage = shrink,
                cv.folds = folds)
  
  gbm.perf(mo1gbm,method="cv")
  
  sqrt(min(mo1gbm$cv.error))
  which.min(mo1gbm$cv.error)
  
  monthlySalesOfTotal <- predict.gbm(mo1gbm, newdata=Xtest, n.trees = ntrees)
  #save monthly fractions predictions
  YmonthlyFractions[i-1] <- monthlySalesOfTotal  
  Ytest[,i] <- monthlySalesOfTotal * Ytest[, 14]
}

#
ntestrows = nrow(Xtest)
Ytest[,1] = seq(1,ntestrows,1)
write.csv(Ytest[,1:14], "campaign_4_jag_gbm.csv", row.names=FALSE)


