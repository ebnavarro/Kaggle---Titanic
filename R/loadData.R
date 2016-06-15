loadCleanData <- function(){

  # READ DATA
  trainPath = "data/train.csv" 
  testPath = "data/test.csv" 
  train <- read.csv(trainPath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  test <- read.csv(testPath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  
  # Joining datasets to analyse better the whole data
  train$SampleOrigin <- 'TRAIN'
  test$SampleOrigin <- 'TEST'
  test$Survived <- NA
  
  fullData <- rbind(train, test)
  # anyDuplicated(fullData$PassengerId) # As it is 0, we can consider this field as 
  
  # Prepare data by type
  fullData$SampleOrigin = factor(fullData$SampleOrigin)
  fullData$Survived <- factor(fullData$Survived)
  fullData$Pclass <- factor(fullData$Pclass)
  fullData$Sex <- factor(fullData$Sex)
  fullData$Embarked <- factor(fullData$Embarked)

  return(fullData)
}

handleMissings <- function(fullData){
  ids <- which(is.na(fullData$Fare)) # returns 1044
  #hist(fullData$Fare)
  #boxplot(fullData$Fare)
  fullData[ids,]$Fare <- mean(fullData$Fare[!is.na(fullData$Fare)])
  
  #boxplot(Fare~Embarked, data=fullData)
  fullData[which(fullData$Embarked==''),]$Embarked <- 'C'
  fullData$Embarked <- factor(fullData$Embarked)
  
  return(fullData)
}

