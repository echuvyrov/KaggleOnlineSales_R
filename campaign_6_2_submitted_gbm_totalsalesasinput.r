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
Xtest <- testData[,2:18]
#colnames(Xcats) <- c("Quan_500", "Quan_501", "Quan_502")

#add the difference between two dates as another variable
daysbetweencampaigns <- as.numeric(data[,14]-data[,19])
X <- cbind(X, daysbetweencampaigns)

daysbetweencampaigns <- as.numeric(testData[,3]-testData[,8])
Xtest <- cbind(Xtest, daysbetweencampaigns)

#YMonthlyFractions <- matrix(nrow = 1, ncol = 12)

YMonthlySales <- as.matrix(data[,1:12])
YMonthlySales <- log(YMonthlySales)
YMonthlySales[is.na(YMonthlySales)] <- 0.0

YTotalSales <- as.numeric(data[,1])
for(i in 1:nrow(data)){
  YTotalSales[i] <- log(sum(data[i,1:12],na.rm=TRUE))
}

#feed total sales as an input variable
X <- cbind(X, YTotalSales)

#cleanup data - factor variables are still problematic on prediction
X <- cleanInputDataForGBM(X)
Xtest <- cleanInputDataForGBM(Xtest)
ntestRows <- nrow(Xtest)
Ytest <- matrix(nrow = ntestRows, ncol = 12)

#train and cross-validate the model using 10-fold cv
numberOfRows <- nrow(X)
numberOfXColumns <- ncol(X)
rmsles <- rep(0, 10)

ntrees <- 4000
depth <- 5
minObs <- 10
shrink <- 0.001
folds <- 10
gdata <- cbind(YTotalSales,X)

gbmAnnual <- gbm(YTotalSales~. ,data=gdata,
              distribution = "gaussian",
              n.trees = ntrees,
              shrinkage = shrink,
              cv.folds = folds)

YTotalSales <- predict.gbm(gbmAnnual, newdata=Xtest, n.trees = ntrees)
Xtest <- cbind(Xtest, YTotalSales)

#estimate and predict individual months' sales
for( i in 1:12 ) {
  YThisMonth <- YMonthlySales[,i]
  gdata <- cbind(YThisMonth, X)
  
  #fit the model on training data
  gbmMonthly <- gbm(YThisMonth~. ,
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
write.csv(Ytest, "campaign_6_totalsales_asinput_gbm.csv", row.names=FALSE)
