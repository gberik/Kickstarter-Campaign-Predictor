#Galip Sina Berik & Javier Guerrero
#Plot ROC Curve
#Install required packages

if (!require("caret")) {
  install.packages("caret")
  library("caret")
}

#Import kickstart.csv file
#setwd
kickstart <- read.csv("fulldatagrouped_01.csv")


#Split data into 70% training set and 30% test set
trainSetSize <- floor(0.7 * nrow(kickstart))   
set.seed(12345)                       
trainInd <- sample(seq_len(nrow(kickstart)), size = trainSetSize) 
TrainSet <- kickstart[trainInd, ]               
TestSet <- kickstart[-trainInd, ] 

kickstart$StatusBinary <- as.factor(kickstart$StatusBinary)

#Create a logistic regression model from training data
LogModel <- glm(StatusBinary ~ Main.Category + Country + Goal..USD., 
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
actualTestClass <- TestSet$StatusBinary
confMx <- confusionMatrix(as.factor(actualTestClass), as.factor(predTestClass), positive = "1") 
confMx



#Thanks to Bob Horton, Microsoft
####################
####################
simpleROC <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels), FPR=cumsum(!labels)/sum(!labels), labels)
}

rocData <- simpleROC(actualTestClass,predTestScores)
rocData
plot(rocData$FPR,rocData$TPR, xlab = "1 - Specificity", ylab = "Sensitivity")

#Use library pROC to plot ROC and 
#calculate area under the curve (AUC)
pROCData <- pROC::roc(TestSet$StatusBinary,predTestScores) 
plot(pROCData) # Gets a smoother version of the curve for calculating AUC; axes labels a bit confusing
pROCData[9] # Prints the AUC (Area Under Curve)

####################
#Create a lift chart
####################

#Simple lift chart; from scratch
dfForLiftChart <- data.frame(predTestScores, actualTestClass) 
sortedData <- dfForLiftChart[order(-dfForLiftChart$predTestScores),] 
cumulCases <- cumsum(sortedData[,2]) 
cumulCases
##Plot the lift chart
plot(cumulCases, xlab = "Number of Cases", ylab = "Number of 1s Identified by Algorithm So Far", type="l", col = "blue") 
##Plot the 45 degree line
X <- c(0, length(predTestScores))
Y <- c(0, cumulCases[length(predTestScores)])
lines(X, Y, col = "red", type = "l", lty = 2)

#Lift chart using the "caret" library
li <-lift(relevel(as.factor(actualTestClass), ref="1") ~ predTestScores)
xyplot(li, plot = "gain")

############################################
#Decile Lift Chart using the "gains" library
############################################
if (!require("gains")) {
  install.packages("gains")
  library("gains")
}
gain <- gains(actualTestClass, predTestScores)
barplot(gain$mean.resp/mean(actualTestClass), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile Lift Chart")

