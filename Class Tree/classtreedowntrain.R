#QTM2000 Case Studies in Business Analytics
#FInal Project Classification Tree

#import necessary libraries
if (!require("caret")) {
  install.packages("caret")
  library("caret")
}

if (!require("rpart")) {
  install.packages("rpart")
  library("rpart")
}
if (!require("rpart.plot")) {
  install.packages("rpart.plot")
  library("rpart.plot")
}


#Read the Data 
setwd("C:/Users/rbarco1/Desktop/QTM2000/R/Data")
kickstart <- read.csv("Sample_Optimum_ToPredict.csv", header=TRUE, sep=",")
table(kickstart$Status)

RNGkind(sample.kind = "Rejection")
set.seed(12345)    


#Split data into 60% training set and 40% test set
trainSetSize <- floor(0.7 * nrow(kickstart))   
trainInd <- sample(seq_len(nrow(kickstart)), size = trainSetSize) 
trainSet <- kickstart[trainInd, ]

trainSet <- downSample(x = trainSet,
                     y = trainSet$Status)
table(trainSet$Status)

testSet <- kickstart[-trainInd, ] 



#Create the full classification tree (ClassTree1)
Fulltree <- rpart(Status ~ Category + Main.Category + Currency +  + Country + Goal..USD.,
                  data=trainSet,
                  method = "class",
                  control = rpart.control(minsplit = 1,cp = 0))
printcp(Fulltree)
prp(Fulltree, extra=1)

#Created Pruned Classification Tree (ClassTree2)

#Prune the tree
Prunedtree <- prune(Fulltree, cp=
                      Fulltree$cptable[which.min(Fulltree$cptable[,"xerror"]), "CP"])
printcp(Prunedtree)
prp(Prunedtree, extra=1,cex = 0.35)

#Confusion Matrix
predTestClass1 <- predict(Fulltree, newdata = testSet, type="class")
confMx <- confusionMatrix(predTestClass1, testSet$Status, positive = "successful")
confMx

#Predict the prices of houses in the test set for ClassTree2
predTestClass2 <- predict(Prunedtree, newdata = testSet, type="class")
confMx <- confusionMatrix(predTestClass2, testSet$Status, positive = "successful")
confMx

#Finding The Second most important Variables 

Fulltree2 <- rpart(Status ~ Main.Category + Currency + Country + Goal..USD.,
                   data=trainSet,
                   method = "class",
                   control = rpart.control(minsplit = 1,cp = 0))
printcp(Fulltree2)
prp(Fulltree2, extra=1)

#Created Pruned Classification Tree (ClassTree2)

#Prune the tree
Prunedtree2 <- prune(Fulltree2, cp=
                       Fulltree2$cptable[which.min(Fulltree2$cptable[,"xerror"]), "CP"])
printcp(Prunedtree2)
prp(Prunedtree2, extra=1)

#Confusion Matrix
predTestClass3 <- predict(Fulltree2, newdata = testSet, type="class")
confMx <- confusionMatrix(predTestClass3, testSet$Status, positive = "successful")
confMx

#Predict the prices of houses in the test set for ClassTree2
predTestClass4 <- predict(Prunedtree2, newdata = testSet, type="class")
confMx <- confusionMatrix(predTestClass4, testSet$Status, positive = "successful")
confMx

#Finding The Third most important Variable

Fulltree3 <- rpart(Status ~ Main.Category + Currency + Country,
                   data=trainSet,
                   method = "class",
                   control = rpart.control(minsplit = 1,cp = 0))
printcp(Fulltree3)
prp(Fulltree2, extra=1)

#Created Pruned Classification Tree (ClassTree2)

#Prune the tree
Prunedtree3 <- prune(Fulltree3, cp=
                       Fulltree3$cptable[which.min(Fulltree3$cptable[,"xerror"]), "CP"])
printcp(Prunedtree3)
prp(Prunedtree3, extra=1)

#Confusion Matrix
predTestClass5 <- predict(Fulltree3, newdata = testSet, type="class")
confMx <- confusionMatrix(predTestClass5, testSet$Status, positive = "successful")
confMx

#Predict the prices of houses in the test set for ClassTree2
predTestClass6 <- predict(Prunedtree3, newdata = testSet, type="class")
confMx <- confusionMatrix(predTestClass5, testSet$Status, positive = "successful")
confMx

