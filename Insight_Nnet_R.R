library(neuralnet)
library(caret)

Full <- data.frame(read.csv('Desktop/Data_Challenges/Insight_DC/Full_Data.csv'))
Full <- data.frame(Full[complete.cases(Full),])

NormFunc <- function(x){ 
  a <- min(x) 
  b <- max(x) 
  (x - a)/(b - a) 
} 

Full <- data.frame(apply(Full,2,NormFunc))
trainIndex <- createDataPartition(Full$NSP, p = .8,list = FALSE, times = 1)

TempTrain <- Full[trainIndex,]
Final_Test <- Full[-trainIndex,]

trainIndex <- createDataPartition(TempTrain$NSP, p = .8,list = FALSE, times = 1)

Final_Train <- Full[trainIndex,]
Final_Valid <- Full[-trainIndex,]

nnet <- neuralnet(NSP~ LB+AC+FM+UC+DL+DS+DP+ASTV+MSTV+ALTV+MLTV+Width+
                  Min+Max+Nmax+Nzeros+Mode+Mean+Median+Variance+Tendency, 
                  Final_Train,hidden=c(4,5), lifesign = "minimal",linear.output = FALSE, threshold = 0.1)

nnet.results <- data.frame(compute(nnet, Final_Valid[-22]))

Results <- data.frame(cbind(Final_Valid$NSP,nnet.results$net.result))

roundStuff <- function(number){
  if (number >= 0 & number <= .15){
    return(0)
  }else if(
    number > .35 & number <= .65
  ){
    return (0.5)
  }else if(
    number >0.85
  ){
    return(1)
  }else {
    return (2)
  }
}

Results$RdPred <- sapply(Results$X2, roundStuff )

Results[Results==2] <- NA
Results <- na.omit(Results)

confusionMatrix(Results$RdPred,Results$X1)
#model <- train(as.factor(NSP) ~ LB+AC+FM+UC+DL+DS+DP+ASTV+MSTV+ALTV+MLTV+Width+Min+Max+Nmax+Nzeros+Mode+Mean+Median+Variance+Tendency, 
#  Final_Train, method='nnet', linout=FALSE, trace = FALSE,hidden=(20,15,7),tuneGrid=expand.grid(.size=c(1,5,10),.decay=c(0,0.001,0.1)))

