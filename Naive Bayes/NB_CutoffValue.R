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

#Predict Probability
predTestScores <- predict(nbModelCatNOCurrency, myDataTest, type = "raw")[,2] 

#Classify based on Naive Bayes output
##Set cutoff value 
cutoff <- 0.65
##Initially, set all predicted class assignments to 0
predTestClass <- rep(0, length(predTestScores))
##Then, replace with only those entries that are greater than the cutoff value 
predTestClass[predTestScores > cutoff] <- 1


#Create a confusion matrix
actualTestClass <- myDataTest$StatusBinary
confMx <- confusionMatrix(as.factor(actualTestClass), as.factor(predTestClass), positive = "1") 
confMx

