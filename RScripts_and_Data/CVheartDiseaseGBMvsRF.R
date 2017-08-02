#rm(list = ls())
require(gbm)
require(dplyr)
require(caret)
require(verification)
require(randomForest)

#Read data from csv file
df <- read.csv("cleveland.csv", sep = ",", na.strings = "?")
s <- sum(is.na(df))
df <- na.omit(df)
dim(df)

#Transform to Binomial attribute
df$num[df$num >0] <- 1

############# Load and transform data #################
df$cp <- factor(df$cp)
df$sex <- factor(df$sex)
df$thal <- factor(df$thal)

levels(df$sex) <- c("female", "male", "")
levels(df$cp) <- c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")
levels(df$thal) <- c("normal", "fixed defected", "reversable defect") 

#Split data to training set (70%) and test set (30%)
set.seed(10)
inTrainRows <- createDataPartition(df$num, p = 0.7, list = FALSE)
train <- df[inTrainRows, ]
test <- df[-inTrainRows, ]
nrow(train) / (nrow(test) + nrow(train))

head(train)
summary(train)


#################### partition the data #####################
#there's a function in plyr that will do this, but it's easy to do your own
#for k-fold CV, you create k different partitions in the data
#my data are already in a random order

k = 10
n = floor(nrow(train)/k) #n is the size of each fold
#I rounded down to avoid going out of bounds on the last fold
err.vect = rep(NA,k) #store the error in this vector

#how to partition the first fold
i = 1
s1 = ((i - 1) * n+1) #the start of the subset
s2 = (i * n)       #the end of the subset
subset = s1:s2   #the range of the subset 
#because of rounding, the end of the subset may be slighly out of range

cv.train = train[-subset,] #train the model using this data    
cv.test = train[subset,] #test the model's performance on this data

#to do "standard" CV, we could just run the model on the cv.train data
#and test it on the cv.test data
#k-fold CV allows us to use all of the data for the final model
#but still have realistic model performance estimates 

#next, move to the second fold:
i = 2
#...
##############################################################

########################### CV for random forest ############################
#need to loop over each of the folds
for(i in 1:k){
  s1 = ((i - 1) * n+1) #the start of the subset
  s2 = (i * n)       #the end of the subset
  subset = s1:s2   #the range of the subset 
  
  cv.train = train[-subset,] #train the model using this data    
  cv.test = train[subset,] #test the model's performance on this data
  
  #run the random forest on the train set
  fit = randomForest(x = cv.train[,-14], y = as.factor(cv.train[,14]))
  #make predictions on the test set
  prediction = predict(fit, newdata = cv.test[,-14], type = "prob")[,2]
  
  #calculate the model's accuracy for the ith fold
  err.vect[i] = roc.area(cv.test[,14], prediction)$A 
  print(paste("AUC for fold", i, ":", err.vect[i]))
}
print(paste("Average AUC:", mean(err.vect)))

#each fold has a different error rate,
#and that's why we do k-fold CV!

##############################################################################

########################### CV for gbm ############################
ntrees = 5000 #the default is only 100
for(i in 1:k){
  s1 = ((i - 1) * n+1) #the start of the subset
  s2 = (i * n)       #the end of the subset
  subset = s1:s2   #the range of the subset 
  
  cv.train = train[-subset,]    
  cv.test = train[subset,] #test the model's performance on this data
  
  #estimate the gbm on the cv.train set
  fit = gbm.fit(x = cv.train[,-14], y = cv.train[,14], 
                n.trees = ntrees, verbose = FALSE, shrinkage = 0.01, 
                interaction.depth = 6, n.minobsinnode = 10, distribution = "bernoulli") 
  #use bernoulli or adaboost for classification problems
  #make predictions on the test set
  prediction = predict(fit, newdata = cv.test[,-14], n.trees = ntrees)
  err.vect[i] = roc.area(cv.test[,14], prediction)$A 
  print(paste("AUC for fold", i, ":", err.vect[i]))
}
print(paste("Average AUC:", mean(err.vect)))

