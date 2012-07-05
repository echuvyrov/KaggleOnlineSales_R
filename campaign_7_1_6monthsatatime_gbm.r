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
    
  Ysimulated <- exp(YCVTestPredictedMonthly)
  Yreal <- exp(YCVTestExpectedMonthlySales)

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

ntestRows <- nrow(Xtest)
Ytest <- matrix(nrow = ntestRows, ncol = 12)

YMonthlyFractions <- matrix(nrow = 1, ncol = 12)

YMonthlySales <- as.matrix(data[,1:12])
YMonthlySales <- log(YMonthlySales)
YMonthlySales[is.na(YMonthlySales)] <- 0.0

YTotalSales6Months <- as.numeric(data[,1])
#Y - labels as sum of all month sales
for(i in 1:nrow(data)){
  YTotalSales6Months[i] <- log(sum(data[i,1:6],na.rm=TRUE))
}

#cleanup data - factor variables are still problematic on prediction
X <- cleanInputDataForGBM(X)

#train and cross-validate the model using 10-fold cv
numberOfRows <- nrow(X)
numberOfXColumns <- ncol(X)
rmsles <- rep(0, 10)

#estimate and predict total sales for data in the training set
gdata <- cbind(YTotalSales6Months, X)
ntrees <- 4000
depth <- 5
minObs <- 10
shrink <- 0.001
folds <- 10

mo1gbm <- gbm(YTotalSales6Months~. ,data=gdata,
              distribution = "gaussian",
              n.trees = ntrees,
              shrinkage = shrink,
              cv.folds = folds)

YPredicted6Months <- predict.gbm(mo1gbm, newdata=Xtest, n.trees = ntrees)

#now estimate and predict individual months' sales  
# first 6 months of sales will be estimated on the prediction of fractions of monthly sales to total 6 months' sales
for( i in 1:6 ) {
  #get the fraction of total sales for a given month
  i<-2
  YSalesThisMonth <- exp(YMonthlySales[,i])/exp(YTotalSales6Months)
  YSalesThisMonth[is.na(YSalesThisMonth)] <- 0.0
  
  gdata <- cbind(YSalesThisMonth, X)
  
  #fit the model on training data
  mo2gbm <- gbm(YSalesThisMonth~. ,
                data=gdata,
                distribution = "gaussian",
                n.trees = ntrees,
                shrinkage = shrink,
                cv.folds = folds)
      
  #apply the model to test data
  monthlySalesOfTotal <- predict.gbm(mo2gbm, newdata=Xtest, n.trees = ntrees)
  #save monthly sales predictions
  Ytest[,i] <- monthlySalesOfTotal * exp(YPredicted6Months)
}
  
# second 6 months of sales will be pure sales prediction
for( i in 7:12 ) {
  YSalesThisMonth <- YMonthlySales[,i]
  gdata <- cbind(YSalesThisMonth, X)
  
  #fit the model on training data
  gbmMonthly <- gbm(YSalesThisMonth~. ,
                    data=gdata,
                    distribution = "gaussian",
                    n.trees = ntrees,
                    shrinkage = shrink,
                    cv.folds = folds)
  
  monthlySales <- predict.gbm(gbmMonthly, newdata=Xtest, n.trees = ntrees)
  #save monthly sales predictions
  Ytest[,i] <- exp(monthlySales)
}


indices = seq(1,ntestRows,1)
Ytest <- cbind(indices,Ytest)
write.csv(Ytest, "campaign_7_6monthssplit_gbm.csv", row.names=FALSE)
