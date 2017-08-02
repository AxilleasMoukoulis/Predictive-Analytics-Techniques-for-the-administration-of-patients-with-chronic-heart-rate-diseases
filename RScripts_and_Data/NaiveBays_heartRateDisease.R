#rm(list = ls())
#Required libraries
require(e1071)
require(caret)

#Read data from csv file
df <- read.csv("cleveland.csv", sep = ",", na.strings = "?")
head(df)
dim(df)

#Transform to Binomial attribute
df$num[df$num >0] <- 1

#Data manipulation for the algorithm to process
df$age <- factor(df$age)
df$cp <- factor(df$cp)
df$sex <- factor(df$sex)
df$fbs <- factor(df$fbs)
df$restecg <- factor(df$restecg)
df$exang <- factor(df$exang)
df$slope <- factor(df$slope)
df$num <- factor(df$num)
df$trestbps <- factor(df$trestbps)
df$chol <- factor(df$chol)
df$cp <- factor(df$cp)
df$thal <- factor(df$thal)
df$ca <- factor(df$ca)
df$thal <- factor(df$thal)
df$oldpeak <- factor(df$oldpeak)
df$thalach <- factor(df$thalach)

#Remove the rows with empty cells
s <- sum(is.na(df))
df <- na.omit(df)
dim(df)

#Split data - 90% training set - 10% test set
set.seed(10)
inTrainRows <- createDataPartition(df$num, p = 0.9, list = FALSE)
trainData <- df[inTrainRows, ]
testData <- df[-inTrainRows, ]
nrow(trainData) / (nrow(testData) + nrow(trainData))

#Train the naive Bayes model
model <- naiveBayes(num ~ ., data = trainData) 

#Prediction based on the model
pred <- predict(model, testData[,-14]) 

#Confusion matrix of the results
tab <- table(pred, testData$num)
tab

sum(tab[row(tab)==col(tab)])/sum(tab)
summary(testData$num)
