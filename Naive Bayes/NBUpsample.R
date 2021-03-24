#Galip Sina Berik
#All Naive Bayes Models with Upsampling

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
myData <- read.csv("fulldatagrouped.csv")

#Mark the variables that won't be used as NULL
myData$ID <- NULL 
myData$Name <- NULL 
myData$Launched <- NULL 
myData$Deadline <- NULL 
myData$Backers <- NULL 
myData$Pledged..USD. <- NULL 
myData$X <- NULL
myData$GoalGroup <- as.factor(myData$GoalGroup)


#Split data into a random 70% training set and 30% test set
trainSetSize <- floor(0.7 * nrow(myData))   
set.seed(12345)                       
trainInd <- sample(seq_len(nrow(myData)), size = trainSetSize) 
myDataTrain <- myData[trainInd, ]     
#uptrain the training set
myDataTrain <- upSample(x = myDataTrain,
                          y = myDataTrain$Status)
myDataTest <- myData[-trainInd, ] 


#Naive

#Build Naive Bayes model with Main.Category and without Country
nbModelWOCountry <- naiveBayes(Status ~ Main.Category + GoalGroup + Currency, myDataTrain)

#Diplay model
nbModelWOCountry

#Predict outcome for new observation(s) stored in test data set
predTestProbNaive <- predict(nbModelWOCountry, myDataTest, type = "raw") 
predTestClassNaiveWOCountry <- predict(nbModelWOCountry, myDataTest) #default cutoff is 0.5


#Compute confusion matrix for prediction model
################################################
actualTestClassWOCountry <- myDataTest$Status

#Calculate all relevant statistics for confusion matrix
#############################################
confMxNaiveWOCountry <- confusionMatrix(predTestClassNaiveWOCountry, actualTestClassWOCountry, positive = "successful")
#print confusion matrix
confMxNaiveWOCountry

#calculate total accuracy and print it
totAccWOCountry <- confMxNaiveWOCountry$overall[1]
totAccWOCountry

#Build Naive Bayes model with Main.Category and without Currency
nbModelNOCurrency <- naiveBayes(Status ~ Main.Category + GoalGroup + Country, myDataTrain)

#Diplay model
nbModelNOCurrency

#Predict outcome for new observation(s) stored in test data set
predTestProbNaive <- predict(nbModelNOCurrency, myDataTest, type = "raw") 
predTestClassNaiveNOCurrency <- predict(nbModelNOCurrency, myDataTest) #default cutoff is 0.5


#Compute confusion matrix for prediction model
################################################
actualTestClassNOCurrency <- myDataTest$Status

#Calculate all relevant statistics for confusion matrix
#############################################
confMxNaiveNOCurrency <- confusionMatrix(predTestClassNaiveNOCurrency, actualTestClassNOCurrency, positive = "successful")
#print confusion matrix
confMxNaiveNOCurrency

#calculate total accuracy and print it
totAccNOCurrency <- confMxNaiveNOCurrency$overall[1]

#Compare Total Accuracies
totAccNOCurrency
totAccWOCountry




#Naive
#This part uses Category instead of Main.Category

#Build Naive Bayes model with Category and without Country
nbModelCatWOCountry <- naiveBayes(Status ~ Category + GoalGroup + Currency, myDataTrain)

#Diplay model
nbModelCatWOCountry

#Predict outcome for new observation(s) stored in test data set
predTestProbNaive <- predict(nbModelCatWOCountry, myDataTest, type = "raw") 
predTestClassNaiveCatWOCountry <- predict(nbModelCatWOCountry, myDataTest) #default cutoff is 0.5


#Compute confusion matrix for prediction model
################################################
actualTestClassCatWOCountry <- myDataTest$Status

#Calculate all relevant statistics for confusion matrix
#############################################
confMxNaiveCatWOCountry <- confusionMatrix(predTestClassNaiveCatWOCountry, actualTestClassCatWOCountry, positive = "successful")
#print confusion matrix
confMxNaiveCatWOCountry

#calculate total accuracy and print it
totAccCatWOCountry <- confMxNaiveCatWOCountry$overall[1]
totAccCatWOCountry



#Build Naive Bayes model with Category and without Currency
nbModelCatNOCurrency <- naiveBayes(Status ~ Category + GoalGroup + Country, myDataTrain)

#Diplay model
nbModelCatNOCurrency

#Predict outcome for new observation(s) stored in test data set
predTestProbNaive <- predict(nbModelCatNOCurrency, myDataTest, type = "raw") 
predTestClassNaiveCatNOCurrency <- predict(nbModelCatNOCurrency, myDataTest) #default cutoff is 0.5


#Compute confusion matrix for prediction model
################################################
actualTestClassCatNOCurrency <- myDataTest$Status

#Calculate all relevant statistics for confusion matrix
#############################################
confMxNaiveCatNOCurrency <- confusionMatrix(predTestClassNaiveCatNOCurrency, actualTestClassCatNOCurrency, positive = "successful")
#print confusion matrix
confMxNaiveCatNOCurrency

#calculate total accuracy and print it
totAccCatNOCurrency <- confMxNaiveCatNOCurrency$overall[1]

#Compare Total Accuracies

#Category, Country, GoalGroup
totAccCatNOCurrency
#Cateogory, Currency, GoalGroup
totAccCatWOCountry
#Main.Category, Country, GoalGroup
totAccNOCurrency
#Main.Category, Currency, GoalGroup
totAccWOCountry

#Print all ConfMx
confMxNaiveCatNOCurrency
confMxNaiveCatWOCountry
confMxNaiveNOCurrency
confMxNaiveWOCountry