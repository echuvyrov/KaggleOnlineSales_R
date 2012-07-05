rm(list=ls())
require(gbm)

cleanInputDataForGBM <- function(X) {
  names(X);
  for(i in 1:length(X)) {
    
    name = names(X)[i]
    print (name)
    col = X[,i]  
    
    index = which(is.na(col))
    
    if ( substr(name,1,3) == 'Cat'  ) {
      col[index] = "Unknown"
      X[,i] <- as.factor(col)
    }
    
    if ( substr(name,1,4) == 'Quan' ) {
      column_mean = mean(col, na.rm = TRUE)
      col[index] = column_mean
      X[,i] <- as.numeric(col)
    }
    
    if ( substr(name,1,4) == 'Date' ) {    
      column_mean = mean(col, na.rm = TRUE)
      col[index] = column_mean
      X[,i] <- as.numeric(col)
    }

    result = is.factor(X[,i])
    print(result);
  }
  return (X)
}


computeRMSLE <- function(Ysimulated, Yreal) {
  
  #pick every 3rd row
  #Ysimulated <- Ysimulated[seq(1, nrow(Ysimulated), 3), ]
  #Yreal <- Yreal[seq(1, nrow(Yreal), 3), ]
    
  #zero out negative elements  
  Ysimulated <- ifelse(Ysimulated<0,0,Ysimulated)
  Yreal <- ifelse(Yreal<0,0,Yreal)
  
  #initialize values
  rmsle <- 0.0
  n <- 0
  
  #perform calculations
  Ysimulated <- log(Ysimulated + 1)
  Yreal <- log(Yreal + 1)
  
  #n <- nrow(Yreal) * ncol(Yreal)
    
  #for vectors, n is the length of the vector
  n <- length(Yreal)
  rmsle <- sqrt(sum((Ysimulated - Yreal)^2)/n)
  
  return (rmsle)
}

#setup data
setwd("C:\\Projects\\R")
data <- read.table(file="TrainingDataset.csv",header=TRUE, sep=",")
testData <- read.table(file="TestDataset.csv",header=TRUE, sep=",")

X <- data[,13:29]
#add the difference between two dates as another variable
daysbetweencampaigns <- as.numeric(data[,14]-data[,19])
X <- cbind(X, daysbetweencampaigns)

Xtest <- testData[,2:18]
daysbetweencampaigns <- as.numeric(testData[,3]-testData[,8])
Xtest <- cbind(Xtest, daysbetweencampaigns)

YMonthlyFractions <- matrix(nrow = 1, ncol = 12)

YMonthlySales <- as.matrix(data[,1:12])
YMonthlySales <- log(YMonthlySales)
YMonthlySales[is.na(YMonthlySales)] <- 0.0

YTotalSales <- as.numeric(data[,1])
#Y - labels as sum of all month sales
for(i in 1:nrow(data)){
  YTotalSales[i] <- log(sum(data[i,1:12],na.rm=TRUE))
}

#cleanup data - factor variables are still problematic on prediction
X <- cleanInputDataForGBM(X)
Xtest <- cleanInputDataForGBM(Xtest)
ntestRows <- nrow(Xtest)
Ytest <- matrix(nrow = ntestRows, ncol = 12)

#train and cross-validate the model using 10-fold cv
numberOfRows <- nrow(X)
numberOfXColumns <- ncol(X)
rmsles <- rep(0, 10)

#estimate and predict total sales for data in the training set
gdata <- cbind(YTotalSales,X)
ntrees <- 4000
depth <- 5
minObs <- 10
shrink <- 0.001
folds <- 10

mo1gbm <- gbm(YTotalSales~. ,data=gdata,
              distribution = "gaussian",
              n.trees = ntrees,
              shrinkage = shrink,
              cv.folds = folds)

YPredictedAnnual <- predict.gbm(mo1gbm, newdata=Xtest, n.trees = ntrees)

#now estimate and predict individual months' sales
for( i in 1:12 ) {
  index500s <- as.numeric(which(YMonthlySales[,i]==log(500)))
  
  #get the fraction of total sales for a given month
  YThisMonthFraction <- exp(YMonthlySales[,i])/exp(YTotalSales)  
  YThisMonthFraction[is.na(YThisMonthFraction)] <- 0.0
  
  gdata <- cbind(YThisMonthFraction, X)
  
  #fit the model on training data
  mo2gbm <- gbm(YThisMonthFraction~. ,
                data=gdata,
                distribution = "gaussian",
                n.trees = ntrees,
                shrinkage = shrink,
                cv.folds = folds)
      
  #apply the model to test data
  monthlySalesOfTotal <- predict.gbm(mo2gbm, newdata=Xtest, n.trees = ntrees)
  #save monthly sales predictions
  Ytest[,i] <- monthlySalesOfTotal * exp(YPredictedAnnual)
  
  #now, classify which sales will be $500, but only if the training subset has any 500s
  if(length(index500s)>0)
  {
    YThisMonthFraction[index500s] <- 1
    YThisMonthFraction[-index500s] <- 0
    
    #run classification prediction on 500 yes/no
    gdata <- cbind(YThisMonthFraction, X)
    ntrees <- 4000
    depth <- 2
    minObs <- 10
    shrink <- 0.001
    folds <- 10
    
    gbmClassify500s <- gbm(YThisMonthFraction~. ,data=gdata,
                           distribution = "bernoulli",
                           n.trees = ntrees,
                           shrinkage = shrink,
                           cv.folds = folds)
    
    best.iter=gbm.perf(gbmClassify500s, method="cv")  
    YTestPredicted500s <- predict.gbm(gbmClassify500s, newdata=Xtest, n.trees=best.iter, type="response")
    index500s <- which(YTestPredicted500s > .50)
    YTestPredicted500s[index500s] <- 500
    
    #merge the results of monthly predictions with results of 500 prediction
    indexTest500s <- which(YTestPredicted500s == 500)
    Ytest[indexTest500s,i] <- 500
  }
}

indices = seq(1,ntestRows,1)
Ytest <- cbind(indices,Ytest)
write.csv(Ytest, "campaign_6_classified_500s_gbm.csv", row.names=FALSE)


