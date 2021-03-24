#Logistic regression for Kickstarter Data Set
#QTM2000, Javier Guerrero

#Install required packages
if (!require("caret")) {
  install.packages("caret")
  library("caret")
}

#Import kickstart.csv file
#setwd
kickstart <- read.csv("fulldata.csv")


#Split data into 70% training set and 30% test set
trainSetSize <- floor(0.7 * nrow(kickstart))   
set.seed(12345)                       
trainInd <- sample(seq_len(nrow(kickstart)), size = trainSetSize) 
TrainSet <- kickstart[trainInd, ]               
TestSet <- kickstart[-trainInd, ] 

kickstart$Status.Binary <- as.factor(kickstart$Status.Binary)

#Create a logistic regression model from training data
LogModel <- glm(Status.Binary ~ Main.Category + Country + Goal..USD., 
                    data =TrainSet,
                    family ="binomial")
#Even though we are using all explanatory variables in the data set, we cannot abbreviate
#the set of predictors with "." because this data set contains an ID column, which 
#we would not want to use as a predictor in the logistic regression model
#However, you could do it if you first remove the column loans$ID with the following command:

kickstart$ID <- NULL
kickstart$Name <- NULL 
TestSet$ID <- NULL
TestSet$Name <- NULL
TrainSet$ID <- NULL
TrainSet$Name <- NULL


#Summarize logistic regression output 
summary(LogModel)
coef(LogModel)
exp(coef(LogModel)) 

#Score the logistic regression model on the test data set
predTestScores <- predict(LogModel, type = "response", newdata=TestSet) 

#Create an extra column with predictions to loansTestSet, store in dfToExport 
#Export to file predictedLoans.csv

dfToExport <- data.frame(TestSet,predTestScores)
write.csv(dfToExport, file = "../ROutput/predictedKickstartLog.csv")

#Classify based on logistic regression output
##Set cutoff value 
cutoff <- 0.5
##Initially, set all predicted class assignments to 0
predTestClass <- rep(0, length(predTestScores))
##Then, replace with only those entries that are greater than the cutoff value 
predTestClass[predTestScores > cutoff] <- 1
##Output to file
dfToExport <- data.frame(TestSet,predTestScores,predTestClass)
write.csv(dfToExport, file = "../ROutput/predictedKickstart1.csv")

#Create a confusion matrix
actualTestClass <- TestSet$Status.Binary
actualTestClass <- as.factor(actualTestClass)
confMx <- confusionMatrix(as.factor(actualTestClass), as.factor(predTestClass), positive = "1") 
confMx

levels(kickstart$Status.Binary)

#-------------------------------------
#LogModel 2 with extra variables

kickstart2 <- kickstart

kickstart2$Year <- as.factor(kickstart$Year)

LogModel2 <- glm(Status.Binary ~ Main.Category + Year + Backers + Country + Goal..USD., 
                  data =TrainSet,
                  family ="binomial")
#Even though we are using all explanatory variables in the data set, we cannot abbreviate
#the set of predictors with "." because this data set contains an ID column, which 
#we would not want to use as a predictor in the logistic regression model
#However, you could do it if you first remove the column loans$ID with the following command:




#Summarize logistic regression output 
summary(LogModel2)
coef(LogModel2)
exp(coef(LogModel2)) 

#Score the logistic regression model on the test data set
predTestScores2 <- predict(LogModel2, type = "response", newdata=TestSet) 

#Create an extra column with predictions to loansTestSet, store in dfToExport 
#Export to file predictedLoans.csv

dfToExport <- data.frame(TestSet,predTestScores2)
write.csv(dfToExport, file = "../ROutput/predictedKickstartLog.csv")

#Classify based on logistic regression output
##Set cutoff value 
cutoff <- 0.5
##Initially, set all predicted class assignments to 0
predTestClass2 <- rep(0, length(predTestScores2))
##Then, replace with only those entries that are greater than the cutoff value 
predTestClass2[predTestScores2 > cutoff] <- 1
##Output to file
dfToExport <- data.frame(TestSet,predTestScores,predTestClass)
write.csv(dfToExport, file = "../ROutput/predictedKickstart1.csv")

#Create a confusion matrix
  actualTestClass2 <- TestSet$Status.Binary
  actualTestClass2 <- as.factor(actualTestClass2)
  confMx <- confusionMatrix(as.factor(actualTestClass2), as.factor(predTestClass2), positive = "1") 
  confMx


newObs = data.frame(Main.Category = "Games", Country = "USA", Goal..USD. = 8000) 
predNewObsScore <- predict(LogModel,newObs,type="response")  
predNewObsScore

