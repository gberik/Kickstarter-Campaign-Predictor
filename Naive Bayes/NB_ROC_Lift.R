#Galip Sina Berik
#ROC Curve on Naive Mayes Model 4

#turn scientific notation off
options(scipen=999)

#install R package for Naive Bayes

if (!require("e1071")) {
  install.packages("e1071")
  library("e1071")
}

if (!require("caret")) {
  install.packages("caret")
  library("caret")
}

if (!require("DMwR")) {
  install.packages("DMwR")
  library("DMwR")
}

#Import data file
setwd("C:/Users/gberik1/Desktop/QTM Final/Kickstarter_Data/FINAL")
myData <- read.csv("fulldatagrouped_01.csv")

#Mark the variables that won't be used as NULL
myData$ID <- NULL 
myData$Name <- NULL 
myData$Launched <- NULL 
myData$Deadline <- NULL 
myData$Backers <- NULL 
myData$Pledged..USD. <- NULL 
myData$X <- NULL

myData$StatusBinary <- as.factor(myData$StatusBinary)

#Split data into a random 70% training set and 30% test set
trainSetSize <- floor(0.7 * nrow(myData))   
set.seed(12345)                       
trainInd <- sample(seq_len(nrow(myData)), size = trainSetSize) 
myDataTrain <- myData[trainInd, ]               
myDataTest <- myData[-trainInd, ] 


#Build Naive Bayes model 4
nbModelCatNOCurrency <- naiveBayes(StatusBinary ~ Category + GoalGroup + Country, myDataTrain)

#Diplay model
nbModelCatNOCurrency

#Predict outcome for new observation(s) stored in test data set
predTestProbNaive <- predict(nbModelCatNOCurrency, myDataTest, type = "raw")[,2] 
predTestClassNaiveCatNOCurrency <- predict(nbModelCatNOCurrency, myDataTest) #default cutoff is 0.5

predTestProbNaive
predTestClassNaiveCatNOCurrency
#Compute confusion matrix for prediction model
################################################
actualTestClassCatNOCurrency <- myDataTest$StatusBinary

#Use confustionMatrix from the caret package
confusionMatrix(as.factor(predTestClassNaiveCatNOCurrency), as.factor(actualTestClassCatNOCurrency), positive = "1")

predTestClassNaiveCatNOCurrency <- as.numeric(as.character(predTestClassNaiveCatNOCurrency))
actualTestClassCatNOCurrency <- as.numeric(as.character(actualTestClassCatNOCurrency))


#Thanks to Bob Horton, Microsoft
####################
simpleROC <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

rocData <- simpleROC(actualTestClassCatNOCurrency,predTestProbNaive)
rocData
plot(rocData$FPR,rocData$TPR, xlab = "1 - Specificity", ylab = "Sensitivity")

#Use library pROC to plot ROC and 
#calculate area under the curve (AUC)
pROCData <- pROC::roc(myDataTest$StatusBinary,predTestProbNaive) 
plot(pROCData) # Gets a smoother version of the curve for calculating AUC; axes labels a bit confusing
pROCData[9] # Prints the AUC (Area Under Curve)


####################
#Create a lift chart
####################

#Simple lift chart; from scratch
dfForLiftChart <- data.frame(predTestProbNaive,actualTestClassCatNOCurrency) 
sortedData <- dfForLiftChart[order(-dfForLiftChart$predTestProbNaive),] 
cumulCases <- cumsum(sortedData[,2]) 
cumulCases
##Plot the lift chart
plot(cumulCases, xlab = "Number of Cases", ylab = "Number of 1s Identified by Algorithm So Far", type="l", col = "blue") 
##Plot the 45 degree line
X <- c(0, length(predTestProbNaive))
Y <- c(0, cumulCases[length(predTestProbNaive)])
lines(X, Y, col = "red", type = "l", lty = 2)

############################################
#Decile Lift Chart using the "gains" library
############################################
if (!require("gains")) {
  install.packages("gains")
  library("gains")
}
gain <- gains(actualTestClassCatNOCurrency,predTestProbNaive)
barplot(gain$mean.resp/mean(actualTestClassCatNOCurrency), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile Lift Chart")


