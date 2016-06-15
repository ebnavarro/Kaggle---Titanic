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

normal.charts <- function(x, main.label, x.label, normalized = FALSE){
  # Normalization
  if (normalized) {
    x = normalize(x)
  }
  
  h<-hist(x, breaks=10, col="red", xlab=x.label,
          main=main.label) 
  xfit<-seq(min(x),max(x),length=40)
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)
}

poisson.charts <- function(x, main.label, x.label, normalized = FALSE) {
  if (normalized) {
    x = normalize(x)
  }
  
  h<-hist(x, breaks=10, col="red", xlab=x.label,
          main=main.label)
  xfit<-seq(min(x),max(x),length=max(x)-min(x)+1)
  yfit<-dpois(xfit,lambda=mean(x))
  yfit <- yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit, col="blue", lwd=2)
}

normalize <- function(x){
  x <- (x-min(x))/(max(x)-min(x))
  return(x)
}

outliers.remove <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
  return(y[!is.na(y)])
}

handle.missings <- function(fullData){
  ids <- which(is.na(fullData$Fare)) # returns 1044
  #hist(fullData$Fare)
  #boxplot(fullData$Fare)
  fullData[ids,]$Fare <- mean(fullData$Fare[!is.na(fullData$Fare)])
  
  #boxplot(Fare~Embarked, data=fullData)
  fullData[which(fullData$Embarked==''),]$Embarked <- 'C'
  fullData$Embarked <- factor(fullData$Embarked)
  
  return(fullData)
}

