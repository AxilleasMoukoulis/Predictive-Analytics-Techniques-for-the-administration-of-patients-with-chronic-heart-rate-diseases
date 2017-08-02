#rm(list = ls())
require(rpart)
require(partykit)
require(Rgraphviz)
require(caret)

#Read data from csv file (this dataset is a trimmed version of the original, without the 2, 6 and 7 columns)
df <- read.csv("clevelandCopy3.csv", sep = ",", na.strings = "?")

#A basic view of the dataset
head(df)
dim(df)

###########################
#    Data Manipulation    #
###########################

#Transform to Binomial attribute
df$num[df$num >0] <- 1
#Barplot the shows the number of people with/without Heart rate disease in the dataset
barplot(table(df$num), main = "Fate", col = "black")

#Data manipulation for the algorithm to process
df$cp <- factor(df$cp)
df$exang <- factor(df$exang)
df$slope <- factor(df$slope)
df$num <- factor(df$num)

levels(df$num) <- c("No", "Yes") 

#Remove the rows with empty cells
s <- sum(is.na(df))
df <- na.omit(df)
dim(df)

#Split data - 80% training set - 20% test set
set.seed(10)
inTrainRows <- createDataPartition(df$num, p = 0.8, list = FALSE)
df_train <- df[inTrainRows, ]
df_test <- df[-inTrainRows, ]
nrow(df_train) / (nrow(df_test) + nrow(df_train))

###########################
#          CART           #
###########################

#Fit the data to the model using the proper formula
fit_rpart <- rpart(num~thal+oldpeak+cp+ca+thalach+slope, df_train)
summary(fit_rpart)


#Predictions with the model and the test dataset
predictions_rpart <- predict(fit_rpart, df_test, type = "class")
table(predictions_rpart, df_test$num)

#Ploting the CART tree model
rpart.plot::rpart.plot(fit_rpart)
summary(df_test$num)

###########################
#         C4.5            #
###########################

#rm(list = ls())
library(RWeka)

#Fit the data to the model using the proper formula
fit_J48 <- J48(num~thal+oldpeak+cp+ca+thalach+slope, df_train)
summary(fit_J48)

#Ploting the C4.5 tree
write_to_dot(fit_J48)
ff <- tempfile()
write_to_dot(fit_J48, ff)
plot(agread(ff))

#Evaluation of the model, this library provides evaluation options
e <- evaluate_Weka_classifier(fit_J48,numFolds = 10, complexity = TRUE,seed = 123, class = TRUE)
#A summary of the model
summary(e)

#Predictions with the model and the test dataset
predictions_J48 <- predict(fit_J48, df_test, type = "class")
table(predictions_J48, df_test$num)
summary(df_test$num)

###########################
#      Random Forest      #
###########################

#rm(list = ls())
library(randomForest)
library(reprtree)

#Fit the data to the model using the proper formula
fit_randomForest <- randomForest(num~thal+oldpeak+cp+ca+thalach+slope, df_train, importance=TRUE, ntree=2000)
summary(fit_randomForest)

#Ploting the Random Forest tree model
reprtree:::plot.getTree(fit_randomForest)

#Predictions with the model and the test dataset
predictions_randomForest <- predict(fit_randomForest, df_test, type = "class")
table(predictions_randomForest, df_test$num)
summary(df_test$num)
