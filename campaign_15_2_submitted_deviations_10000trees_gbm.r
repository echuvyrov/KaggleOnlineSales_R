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

#setup data
setwd("C:\\Projects\\R")
data <- read.table(file="TrainingDataset.csv",header=TRUE, sep=",")
testData <- read.table(file="TestDataset.csv",header=TRUE, sep=",")

X <- data[,13:29]
Xtest <- testData[,2:18]
#add the difference between two dates as another variable
daysbetweencampaigns <- as.numeric(data[,14]-data[,19])
X <- cbind(X, daysbetweencampaigns)
X2 <- cbind(data[,44:44], data[,138:138])
colnames(X2) <- c("Quan_500", "Quan_501")
X <- cbind(X,X2)

X2 <- cbind(testData[,33:33], testData[,127:127])
colnames(X2) <- c("Quan_500", "Quan_501")
Xtest <- cbind(Xtest,X2)

daysbetweencampaigns <- as.numeric(testData[,3]-testData[,8])
Xtest <- cbind(Xtest, daysbetweencampaigns)

YMonthlySales <- as.matrix(data[,1:12])
YMonthlySales <- log(YMonthlySales)
YMonthlySales[is.na(YMonthlySales)] <- 0.0

#temporary, TotalSales will be first month's sales
YTotalSales <- as.numeric(data[,1])
YTotalSales[is.na(YTotalSales)] <- 1
YTotalSales <- log(YTotalSales)

#Y - labels as sum of all month sales
for(i in 1:nrow(data)){
  YTotalSales[i] <- log(sum(data[i,1:12],na.rm=TRUE))
}
YTotalSales[is.na(YTotalSales)] <- 0.0

#cleanup data - factor variables are still problematic on prediction
X <- cleanInputDataForGBM(X)
Xtest <- cleanInputDataForGBM(Xtest)
ntestRows <- nrow(Xtest)
Ytest <- matrix(nrow = ntestRows, ncol = 12)

numberOfRows <- nrow(X)
frac <- rep(0.0, 12)

#calculate sum of all sales for the current fold
sumAll <- sum(exp(YTotalSales))

#find "average" time series behavior
#1.  volume weighted  
#frac is total by month / grand total
for(i in 1:12){
  frac [i] <- sum(exp(YMonthlySales[,i]),na.rm=TRUE)/sumAll
}

#some checks
#see if frac sums to 1
sum(frac)
#plot to see if it's reasonable
plot(frac)

#estimate and predict total sales
gdata <- cbind(YTotalSales,X)
ntrees <- 10000
depth <- 5
minObs <- 10
shrink <- 0.001
folds <- 10

mo1gbm <- gbm(YTotalSales~. ,data=gdata,
              distribution = "gaussian",
              n.trees = ntrees,
              shrinkage = shrink,
              cv.folds = folds)

min(mo1gbm$cv.error)
which.min(mo1gbm$cv.error)
summary(mo1gbm)

YPredictedAnnual <- predict.gbm(mo1gbm, newdata=Xtest, n.trees = ntrees)

#now estimate and predict deviations from the "average" monthly portions of the total
for( i in 1:12 ) {
  #get the deviation from the average for a given month's sales
  YDeviationThisMonth <- exp(YMonthlySales[,i])/exp(YTotalSales) - frac[i]
  YDeviationThisMonth[is.na(YDeviationThisMonth)] <- 0.0
  
  gdata <- cbind(YDeviationThisMonth, X)
  
  #fit the model
  mo2gbm <- gbm(YDeviationThisMonth~. ,
                data=gdata,
                distribution = "laplace",
                n.trees = ntrees,
                shrinkage = shrink,
                cv.folds = folds)
  
  #apply the model
  monthlySalesDeviationFromAverage <- predict.gbm(mo2gbm, newdata=Xtest, n.trees = ntrees)
  #save monthly sales prediction
  Ytest[,i] <- monthlySalesDeviationFromAverage * exp(YPredictedAnnual) + frac[i] * exp(YPredictedAnnual)
}

indices = seq(1,ntestRows,1)
Ytest <- cbind(indices,Ytest)
write.csv(Ytest, "campaign_15_deviations_from_predicted_fraction_10000trees_gbm.csv", row.names=FALSE)
