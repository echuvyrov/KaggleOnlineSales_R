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

YMonthlyFractions <- matrix(nrow = 1, ncol = 12)

YMonthlySales <- as.matrix(data[,1:12])
#YMonthlySales <- log(YMonthlySales)
YMonthlySales <- YMonthlySales/500
YMonthlySales[is.na(YMonthlySales)] <- 0.0

YTotalSales <- as.numeric(data[,1])
#Y - labels as sum of all month sales
for(i in 1:nrow(data)){
  YTotalSales[i] <- sum(data[i,1:12],na.rm=TRUE)
}

#scale all sales figures down by a factor of 500
YTotalSales <- YTotalSales/500
YTotalSales[is.na(YTotalSales)] <- 0.0

#cleanup data - factor variables are still problematic on prediction
X <- cleanInputDataForGBM(X)

#train and cross-validate the model using 10-fold cv
numberOfRows <- nrow(X)
numberOfXColumns <- ncol(X)
rmsles <- rep(0, 10)

for(cvFold in 1:10){
  #split the input space into training and test data
  testRows <- seq(from=cvFold, to=numberOfRows, by=10)
  XCVTest <- X[testRows, 1:numberOfXColumns]
  XCVTrain <- X[-testRows, 1:numberOfXColumns]

  YCVTestExpected <- YTotalSales[testRows]
  YCVTrain <- YTotalSales[-testRows]
  
  #estimate and predict total sales for data in the training set
  gdata <- cbind(YCVTrain,XCVTrain)
  ntrees <- 4000
  depth <- 5
  minObs <- 10
  shrink <- 0.001
  folds <- 10
  
  mo1gbm <- gbm(YCVTrain~. ,data=gdata,
                distribution = "gaussian",
                n.trees = ntrees,
                shrinkage = shrink,
                cv.folds = folds)
  
  YCVTestPredictedAnnual <- predict.gbm(mo1gbm, newdata=XCVTest, n.trees = ntrees)
  
  numberOfRowsToTest <- length(YCVTestPredictedAnnual)
  YCVTestPredictedMonthly <- matrix(nrow = numberOfRowsToTest, ncol = 12)
  YCVTrainMonthly <- YMonthlySales[-testRows,]  

  #now estimate and predict individual months' sales  
  for( i in 1:12 ) {
    #get the fraction of total sales for a given month
    YCVTrainMonthly[,i] <- YCVTrainMonthly[,i]/YCVTrain
    YCVTrainMonthly[is.na(YCVTrainMonthly)] <- 0.0
    #store that fraction in YCVTrainThisMonth
    YCVTrainThisMonth <- YCVTrainMonthly[,i]
    
    gdata <- cbind(YCVTrainThisMonth, XCVTrain)
    
    #fit the model on training data
    mo2gbm <- gbm(YCVTrainThisMonth~. ,
                  data=gdata,
                  distribution = "gaussian",
                  n.trees = ntrees,
                  shrinkage = shrink,
                  cv.folds = folds)
        
    #apply the model to test data
    monthlySalesOfTotal <- predict.gbm(mo2gbm, newdata=XCVTest, n.trees = ntrees)
    #save monthly sales predictions
    YCVTestPredictedMonthly[,i] <- monthlySalesOfTotal * YCVTestPredictedAnnual
  }
  
  YCVTestExpectedMonthlySales <- YMonthlySales[testRows,]
  YCVTestExpectedMonthlySales[is.na(YCVTestExpectedMonthlySales)] <- 0.0
  rmsles[cvFold] <- computeRMSLE(YCVTestPredictedMonthly * 500, YCVTestExpectedMonthlySales * 500)
  
}

cvError <- sum(rmsles)/10
