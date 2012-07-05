install.packages("forecast")

require(gbm)
require(tseries)
require(forecast)

rm(list=ls())

crossvalidate <- function(Ysimulated, Yreal) {
  
  #pick every 10th row
  i <- 0
  rmsle <- 0  
  
  while(i < nrow(Ysimulated)){
    for(j in 1:ncol(Ysimulated)){
        rmsle <- rmsle + log(Ysimulated[i,j] + 1) - log(Yreal[i,j] + 1)
        i <- i + 10
      }
    }
  n = nrow(Ysimulated) * ncol(Ysimulated)
  rmsle <- sqrt(rmsle)/n
    
  return (rmsle)
}

#setup data
setwd("C:\\Projects\\R")
data <- read.table(file="TrainingDataset.csv",header=TRUE, sep=",")
testData <- read.table(file="TestDataset.csv",header=TRUE, sep=",")

X <- data[,13:30]
Y <- data[,1:12]
Y <- log(Y)
Y[is.na(Y)] <- 0.0

Xtest <- testData[,2:19]
Ytestrows <- nrow(Xtest)
Ytest <- matrix(nrow = Ytestrows , ncol = 13)
Ytest[,1] <- 2.0

Ymonthlysales <- rep(NA, 10)

#Y - labels as sum of all month sales
for(i in 1:ncol(Y))
{
  Ymonthlysales[i] <- log(sum(data[,i],na.rm=TRUE))
}

Box.test(ts(Ymonthlysales))
fit <- Arima(Ymonthlysales, order=c(3,0,1), seasonal=list(order=c(0,1,1), period=12), 
              include.drift=TRUE, lambda=0, method="ML")

m <- forecast(auto.arima(Ymonthlysales), h=12)
plot(m)
m$model
m$residuals

fit <- ets(Ymonthlysales)
plot(Ymonthlysales)
lines(simulate(fit, 12),col="red")

predict(arima(Ymonthlysales, order=c(0,2,0)))


y.acf <- acf(Ymonthlysales, lag.max = NULL,
    type = "correlation",
    plot = TRUE, na.action = na.fail, demean = TRUE)

y.pacf <- pacf(ts(Ymonthlysales))


#estimate time series from totals
Ycorr <- ar(Ymonthlysales)

r.arima <- arima(ts(Ymonthlysales), order=c(1,1,1), method="ML")
r.arima

ts.plot(arima.sim(n=12,model=r.arima))
Ypr <- predict(r.arima, n.ahead = 12)

Ymonthlyts <- ar(Ymonthlysales)
Ymonthlyts$ar

Ypr <- predict(Ymonthlyts, n.ahead = 11)

nc1 <- ncol(X)
d1 <- dimnames(X)[[2]]

#Y - labels as sum of all month sales
for(i in 1:nrow(data))
{
  Y[i] <- log(sum(data[i,1:12],na.rm=TRUE))
}

#cleanup data - factor variables are still problematic on prediction
idxCat <- c(1,18)
for(i in 1:length(idxCat)) {
 v <- as.factor(X[,idxCat[i]])
 X[,idxCat[i]] <- v

 v <- as.factor(Xtest[,idxCat[i]])
 Xtest[,idxCat[i]] <- v
}

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

#fix up the labels of the new columns
newCols <- paste("V",1:(ncol(X)-nc1),sep="")
dimnames(X)[[2]] <- c(d1,newCols)

