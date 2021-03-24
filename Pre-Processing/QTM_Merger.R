#Galip Sina Berik
#join two datasets

#set working directory
setwd("C:/Users/gberik1/Desktop/QTM Final/Kickstarter_Data/Cleaning")

#define two data frames
data1 <- read.csv("KickStarter_Dataset1.csv")
data2 <- read.csv("KickStarter_Dataset2.csv")

#check for NA
anyNA(data1)
anyNA(data2)

#merge datasets with inner join
innerJoin <- merge(data1,data2, by="ID")

#Remove inconsistent duplicate data
cleanInnerjoin<-innerJoin[!(innerJoin$ID=="1000004038"),]

#save data file
write.csv(cleanInnerjoin, "../Merged_Full_Data.csv")

summary(innerJoin)
