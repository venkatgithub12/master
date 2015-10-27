library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)



predictActivity<-function(){  
trainingDataUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testDataUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#Cleaning NA values
trainingData <- read.csv(url(trainingDataUrl), na.strings=c("NA","#DIV/0!",""))
testingData <- read.csv(url(testDataUrl), na.strings=c("NA","#DIV/0!",""))

#Creating Partition 70:30 
trainingParition <- createDataPartition(trainingData$classe, p=0.7, list=FALSE)  
trainingParitionData <- trainingData[trainingParition, ]
testingParitionData <- trainingData[-trainingParition, ]
head(trainingParition)

# Removing the data that are not unique. 
nonUniqTrainingData<-nearZeroVar(trainingParitionData,saveMetrics=TRUE)
trainingParitionData<-trainingParitionData[,nonUniqTrainingData$nzv==FALSE]

nonUniqTestingData<-nearZeroVar(testingParitionData,saveMetrics=TRUE)
testingParitionData<-testingParitionData[,nonUniqTestingData$nzv==FALSE]

trainingParitionData<-trainingParitionData[c(-1)]

trainingParitionDataNew <- trainingParitionData
#Exlclude NAS
for(i in 1:length(trainingParitionData)) {
  if( sum( is.na( trainingParitionData[, i] ) ) /nrow(trainingParitionData) >= .7) {
    for(j in 1:length(trainingParitionDataNew)) {
      if( length( grep(names(trainingParitionData[i]), names(trainingParitionDataNew)[j]) ) == 1)  {
        trainingParitionDataNew <- trainingParitionDataNew[ , -j]
      }   
    } 
  }
}

# Set back to the original variable name
trainingParitionData <- trainingParitionDataNew
cleanCols <- colnames(trainingParitionData)
cleanColExcClasse <- colnames(trainingParitionData[, -58])  
testingParitionData <- testingParitionData[cleanCols]       
testingData <- testingData[cleanColExcClasse]             


for (i in 1:length(testingData) ) {
  for(j in 1:length(trainingParitionData)) {
    if( length( grep(names(testingParitionData[i]), names(testingData)[j]) ) == 1)  {
      class(testingData[j]) <- class(trainingParitionData[i])
    }      
  }      
}

testingData <- rbind(trainingParitionData[2, -58] , testingData)
testingData <- testingData[-1,]

set.seed(11111)
modelFitRadomForest <- randomForest(classe ~ ., data=trainingParitionData)
predict <- predict(modelFitRadomForest, testingParitionData, type = "class")
confMatrix <- confusionMatrix(predict, testingParitionData$classe)
print(confMatrix)
plot(modelFitRadomForest)
}
