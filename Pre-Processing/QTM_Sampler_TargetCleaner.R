#Galip Sina Berik
#Samples Data & Filters by Target Var

options(scipen=999)

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

if (!require("Metrics")) {
  install.packages("Metrics")
  library("Metrics")
}

#Import CSV file
setwd("C:/Users/gberik1/Desktop/QTM Final/Kickstarter_Data/Merged")
myData <- read.csv("Merged_Full_Data.csv")


#Split data into 60% training set and 40% test set
trainSetSize <- 10000
RNGkind(sample.kind = "Rejection")
set.seed(12345)    

trainInd <- sample(seq_len(nrow(myData)), size = trainSetSize) 
sampleSet <- myData[trainInd, ]       

#inspired from: https://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r
cleansampleSet<-sampleSet[!(sampleSet$Status=="canceled" | sampleSet$Status=="suspended" | sampleSet$Status=="live"),]


summary(cleansampleSet)


#export
dfToExport <- data.frame(cleansampleSet)
write.csv(dfToExport, file = "marged_sample_cleantarget.csv")
