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
  Ysimulated <- Ysimulated[seq(1, nrow(Ysimulated), 3), ]
  Yreal <- Yreal[seq(1, nrow(Yreal), 3), ]
  
  #zero out negative elements
  Ysimulated <- ifelse(Ysimulated<0,0,Ysimulated)
  Yreal <- ifelse(Yreal<0,0,Yreal)
  
  #initialize values
  rmsle <- 0.0
  n <- 0
  
  #perform calculations
  Ysimulated <- log(Ysimulated + 1)
  Yreal <- log(Yreal + 1)
  
  n <- nrow(Yreal) * ncol(Yreal)
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

#the line below to perform prediction on the test data
Xtest <- testData[,2:18]

#the line below for rough estimate of RMSLE on train data
#Xtest <- data[,13:29]

daysbetweencampaigns <- as.numeric(Xtest[,2]-Xtest[,7])
Xtest <- cbind(Xtest, daysbetweencampaigns)
Ytestrows <- nrow(Xtest)

#last column is the total sales for the year
Ytest <- matrix(nrow = Ytestrows , ncol = 14)
YmonthlyFractions <- matrix(nrow = 1, ncol = 12)

#Y - first month's sales only
Y <- as.numeric(data[,1])
Y <- log(Y)
Y[is.na(Y)] <- 0.0

#Y - labels as sum of all month sales
#for(i in 1:nrow(data)){
#  Y[i] <- log(sum(data[i,1:12],na.rm=TRUE))
#}


#cleanup data - factor variables are still problematic on prediction
X <- cleanInputDataForGBM(X)
Xtest <- cleanInputDataForGBM(Xtest)

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
summary(mo1gbm)

#save total sales figure as the last (14th) column in Ytest matrix
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

Yreal <- as.matrix(data[,1:12])
Yreal[is.na(Yreal)] <- 0.0
Ysim <- Ytest[,2:13]

validate <- computeRMSLE(Ysim, Yreal)

#
ntestrows = nrow(Xtest)
Ytest[,1] = seq(1,ntestrows,1)
write.csv(Ytest[,1:13], "campaign_4_1_gbm.csv", row.names=FALSE)
