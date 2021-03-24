#Install required packages
if (!require("caret")) {
  install.packages("caret")
  library("caret")
}

#Import kickstart.csv file
#setwd
kickstart <- read.csv("fulldata.csv")

#Convert Status Binary to Factor
kickstart$Status.Binary <- as.factor(kickstart$Status.Binary)

#Split data into a random 70% training set and 30% test set
trainSetSize <- floor(0.7 * nrow(kickstart))  
set.seed(12345)                      
trainInd <- sample(seq_len(nrow(kickstart)), size = trainSetSize)
TrainSet <- kickstart[trainInd, ]    
#uptrain the training set
TrainSet <- upSample(x = TrainSet,
                        y = TrainSet$Status.Binary)
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