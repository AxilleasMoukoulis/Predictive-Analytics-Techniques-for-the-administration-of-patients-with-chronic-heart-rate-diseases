#rm(list = ls())
require(caret)
require(pROC)
require(randomForest)
require(gbm)

#Read data from csv file
df <- read.csv("cleveland.csv", sep = ",", na.strings = "?")
head(df)
dim(df)

#Transform to Binomial attribute
df$num[df$num >0] <- 1

#Data manipulation for the algorithm to process
df$cp <- factor(df$cp)
df$sex <- factor(df$sex)
#df$fbs <- factor(df$fbs)
#df$restecg <- factor(df$restecg)
df$exang <- factor(df$exang)
df$slope <- factor(df$slope)
df$num <- factor(df$num)

levels(df$num) <- c("No disease", "Disease") 
levels(df$sex) <- c("female", "male", "")

#Some visualization of the data
barplot(table(df$num), main = "Fate", col = "black")
mosaicplot(df$sex ~ df$num, main = "Fate by Gender", shade = FALSE, color = TRUE, xlab="Gender", ylab = "Heart disease")
boxplot(df$age ~ df$num, main = "Fate by Age", ylab = "Age", xlab = "Heart disease")

#Remove the rows with empty cells
s <- sum(is.na(df))
df <- na.omit(df)
dim(df)

#Split the data to 70% training set and 30% test set
set.seed(10)
inTrainRows <- createDataPartition(df$num, p = 0.7, list = FALSE)
trainData <- df[inTrainRows, ]
testData <- df[-inTrainRows, ]
nrow(trainData) / (nrow(testData) + nrow(trainData))

#List for the Area Under the Curve of the tested algorithms
AUC = list()

#List for the Accurancy of the tested algorithms
Accuracy = list()

#####################
#Logistic Regression#
#####################
set.seed(10)

#Train the model with the training set
logRegModel <- train(num ~., data = trainData, method = "glm", family = "binomial")
#Test the model with the testing set
logRegPrediction <- predict(logRegModel, testData)
#Calculate the probability percentage of the prediction
logRegPredictionprob <- predict(logRegModel, testData, type = "prob")[2]

#Store the confusion matrix of the algorithm
logRegConfMat <- confusionMatrix(logRegPrediction, testData[,"num"])

#Store results
AUC$logReg <- roc(as.numeric(testData$num),as.numeric(as.matrix((logRegPredictionprob))))$auc
Accuracy$logReg <- logRegConfMat$overall['Accuracy']

#################
#     C4.5      #
#################
library(RWeka)

#Train the model with the training set
fit <- J48(num~., data=trainData)
#Test the model with the testing set
predictions <- predict(fit, testData)
#Calculate the probability percentage of the prediction
c45Predictionprob <- predict(fit, testData, type = "prob")[2]

#Store the confusion matrix of the algorithm
C45ConfMat <- confusionMatrix(predictions, testData[,"num"])

#AUC$C45 <- roc(as.numeric(testData$num),as.numeric(as.matrix((c45Predictionprob))))$auc
#Accuracy$C45 <- C45ConfMat$overall['Accuracy']


##############
#RandomForest#
##############
set.seed(10)

#Train the model with the training set
RFModel <- randomForest(num ~ ., data=trainData, importance=TRUE, ntree=2000)
#Test the model with the testing set
RFPrediction <- predict(RFModel, testData)
#Calculate the probability percentage of the prediction
RFPredictionprob = predict(RFModel,testData,type="prob")[, 2]

#Store the confusion matrix of the algorithm
RFConfMat <- confusionMatrix(RFPrediction, testData[,"num"])

#Store the confusion matrix of the algorithm
AUC$RF <- roc(as.numeric(testData$num),as.numeric(as.matrix((RFPredictionprob))))$auc
Accuracy$RF <- RFConfMat$overall['Accuracy']  

#############
#BoostedTree#
#############
set.seed(10)
objControl <- trainControl(method='cv', number=10,  repeats = 10)
gbmGrid <-  expand.grid(interaction.depth =  c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode =10)
boostModel <- train(num ~ .,data=trainData, method='gbm',
                    trControl=objControl, tuneGrid = gbmGrid, verbose=F)

#plot(boostModel)
boostPrediction <- predict(boostModel, testData)
boostPredictionprob <- predict(boostModel, testData, type='prob')[2]
boostConfMat <- confusionMatrix(boostPrediction, testData[,"num"])

AUC$boost <- roc(as.numeric(testData$num),as.numeric(as.matrix((boostPredictionprob))))$auc
Accuracy$boost <- boostConfMat$overall['Accuracy']  

####################
#StochasticGradient#
####################

feature.names <- names(df)

for (f in feature.names) {
  if (class(df[[f]])=="factor") {
    levels <- unique(c(df[[f]]))
    df[[f]] <- factor(df[[f]], labels=make.names(levels))
  }
}

set.seed(10)
inTrainRows <- createDataPartition(df$num,p=0.7,list=FALSE)
trainData2 <- df[inTrainRows,]
testData2 <-  df[-inTrainRows,]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

set.seed(10)
gbmModel <- train(num ~ ., data = trainData2, method = "gbm", trControl = fitControl, verbose = FALSE, tuneGrid = gbmGrid, metric = "ROC")

gbmPrediction <- predict(gbmModel, testData2)
gbmPredictionprob <- predict(gbmModel, testData2, type='prob')[2]
gbmConfMat <- confusionMatrix(gbmPrediction, testData2[,"num"])

AUC$gbm <- roc(as.numeric(testData2$num),as.numeric(as.matrix((gbmPredictionprob))))$auc
Accuracy$gbm <- gbmConfMat$overall['Accuracy']


######################
#SupportVectorMachine#
######################
set.seed(10)
svmModel <- train(num ~ ., data = trainData2,
                  method = "svmRadial",
                  trControl = fitControl,
                  preProcess = c("center", "scale"),
                  tuneLength = 8,
                  metric = "ROC")

svmPrediction <- predict(svmModel, testData2)
svmPredictionprob <- predict(svmModel, testData2, type='prob')[2]
svmConfMat <- confusionMatrix(svmPrediction, testData2[,"num"])

AUC$svm <- roc(as.numeric(testData2$num),as.numeric(as.matrix((svmPredictionprob))))$auc
Accuracy$svm <- svmConfMat$overall['Accuracy']  


#########
#Results#
#########
row.names <- names(Accuracy)
col.names <- c("AUC", "Accuracy")
cbind(as.data.frame(matrix(c(AUC,Accuracy),nrow = 5, ncol = 2,
                           dimnames = list(row.names, col.names))))


summary(logRegModel)$coeff

boostImp =varImp(boostModel, scale = FALSE)
row = rownames(varImp(boostModel, scale = FALSE)$importance)
#row = convert.names(row)
rownames(boostImp$importance)=row
plot(boostImp,main = 'Variable importance for heart failure prediction with boosted tree')

logRegConfMat
RFConfMat
boostConfMat
gbmConfMat
svmConfMat
C45ConfMat

