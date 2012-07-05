rm(list=ls())
require(gbm)

cleanInputDataForGBM <- function(X) {
  names(X);
  for(i in 1:length(X)) {
    
    name = names(X)[i]
    print (name)
    col = X[,i]  
    
    index = which(is.na(col))
    
    if ( substr(name,1,3) == 'Cat') {
      column_mean = mean(col, na.rm = TRUE)
      #col[index] = column_mean
      col[index] <- 0
      X[,i] <- as.numeric(col)
    }
        
    if ( substr(name,1,4) == 'Quan' ) {
      column_mean = mean(col, na.rm = TRUE)
      #col[index] = column_mean
      col[index] <- 0
      X[,i] <- as.numeric(col)
    }
    
    if ( substr(name,1,4) == 'data' ) {
      column_mean = mean(col, na.rm = TRUE)
      #col[index] = column_mean
      col[index] <- 0
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

#setup data
setwd("C:\\Projects\\R")
data <- read.table(file="TrainingDataset.csv",header=TRUE, sep=",")
testData <- read.table(file="TestDataset.csv",header=TRUE, sep=",")

#partition data into 2 sets: one with Quan_5 as NaN, the other with the opposite
#pick the set without NaNs

#data <- data[which(is.na(data$Quan_5)),]
#data <- data[which(is.na(data$Quan_4)),]

#testData <- testData[which(!is.na(testData$Quan_5)),]
#dataTest2 <- data[which(!is.na(data$Quan_5)),]

fValues <- factor(c(1, 2,3,4,5))
X <- data[,13:29]
#X[,3] <- log(X[,3])
#X[,5] <- log(X[,5])

# X <- cbind(X, data[,19:19])
# X <- cbind(X, data[,22:29])
# 
# nRows = nrow(X)
# XCat <- matrix(nrow = nRows, ncol = 1)
# 
# for(i in 1:nRows) {
#   v1 <- data[i,]$Quan_5
#   v2 <- data[i,]$Quan_4
#   
#   if(is.na(v1) & is.na(v2)){
#     XCat[i] <- as.factor(fValues[2])
#   }
#   if(is.na(v1) & !is.na(v2)){
#     XCat[i] <- as.factor(fValues[3])
#   }
#   if(!is.na(v1) & is.na(v2)){
#     XCat[i] <- as.factor(fValues[4])
#   }
#   if(!is.na(v1) & !is.na(v2)){
#     XCat[i] <- as.factor(fValues[5])
#   }
# }
# 
# X <- cbind(X, XCat)

Xtest <- testData[,2:18]
#add the difference between two dates as another variable
#daysbetweencampaigns <- as.numeric(data[,14]-data[,19])
#X <- cbind(X, daysbetweencampaigns)

YMonthlySales <- as.matrix(data[,5])
#YMonthlySales <- rbind(as.matrix(data[,1]), as.matrix(data[,2]), as.matrix(data[,3]), as.matrix(data[,4]), as.matrix(data[,5]), as.matrix(data[,6]), as.matrix(data[,7]), as.matrix(data[,8]), as.matrix(data[,9]), as.matrix(data[,10]), as.matrix(data[,11]), as.matrix(data[,12]))
YMonthlySales[is.na(YMonthlySales)] <- 1
YMonthlySales <- log(YMonthlySales)

#X <- rbind(X,X,X,X,X,X,X,X,X,X,X,X)

#YMonthlySales <- ifelse(YMonthlySales == 500, 1, 0)
#YMonthlySales <- ifelse(YMonthlySales == 2000, fValues[2], YMonthlySales)
#YMonthlySales <- ifelse(YMonthlySales == 3000, fValues[3], YMonthlySales)
#YMonthlySales <- ifelse(YMonthlySales > 3000, 0, YMonthlySales)
#YMonthlySales[is.na(YMonthlySales)] <- 0

#YMonthlySales[is.na(YMonthlySales)] <- 1
#YMonthlySales <- log(YMonthlySales)

YTotalSales <- as.numeric(data[,5])
for(i in 1:nrow(data)){
  YTotalSales[i] <- sum(data[i,1:12],na.rm=TRUE)
}

#cleanup data - factor variables are still problematic on prediction
X <- cleanInputDataForGBM(X)

#estimate and predict total sales for data in the training set
gdata <- cbind(YMonthlySales,X)
ntrees <- 10000
depth <- 2
minObs <- 10
shrink <- 0.001
folds <- 10

mo1gbm <- gbm(YMonthlySales~. ,data=gdata,
              distribution = "gaussian",
              n.trees = ntrees,
              shrinkage = shrink,
              cv.folds = folds)

min(mo1gbm$cv.error)
which.min(mo1gbm$cv.error)
summary(mo1gbm)

#YPredictedAnnual <- predict.gbm(mo1gbm, newdata=XTest, n.trees = ntrees)

#numberOfRowsToTest <- length(YPredictedAnnual)
#YPredictedMonthly <- matrix(nrow = numberOfRowsToTest, ncol = 4)
