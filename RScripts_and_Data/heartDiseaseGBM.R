#rm(list = ls())
require(gbm)
require(dplyr)
require(caret)

#Read data from csv file
df <- read.csv("cleveland.csv", sep = ",", na.strings = "?")
head(df)
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
#you can estimate gbm and make predictions on observations with missing values 
#in the feature space (independent variables)

####### Basic data manipulation ########
disease = train$num
train = select(train, -num)
test = select(test, -num)
end_trn = nrow(train)

#combine the two into one data set
all = rbind(train,test)
#Why? So if we manipulate variables (create new ones, cap and floor), 
#we do the same operation for the training and testing data
end = nrow(all)

#select variables to use in modeling (select is a dplyr function)
#gbm does a good job of filtering out noise variables, but will still
#get a better fit when you get rid of junk 
#(especially factor variables with lots of levels)
all = select(all
             , thal
             , oldpeak
             , cp
             , ca
             , thalach
             , age
             ,chol
) 
#not many variables to choose from
#perform variable selection later thal+oldpeak+cp+ca+thalach+slope

head(all)
########################################################

########## The model #############

#a high guess of how many trees we'll need
ntrees = 5000

#how to tune parameters? 
#we'll tune the number of trees and 
#use reasonable values of other parameters
#test different parameters with Cross Validation 

Model = gbm.fit( 
  x = all[1:end_trn,] #dataframe of features
  , y = disease #dependent variable
  #two ways to fit the model
  #use gbm.fit if you are going to specify x = and y = 
  #instead of using a formula
  #if there are lots of features, I think it's easier to specify 
  #x and y instead of using a formula
  
  
  , distribution = "bernoulli"
  #use bernoulli for binary outcomes
  #other values are "gaussian" for GBM regression 
  #or "adaboost"
  
  
  , n.trees = ntrees
  #Choose this value to be large, then we will prune the
  #tree after running the model
  
  
  , shrinkage = 0.01 
  #smaller values of shrinkage typically give slightly better performance
  #the cost is that the model takes longer to run for smaller values
  
  
  , interaction.depth = 6
  #use cross validation to choose interaction depth!!
  
  
  , n.minobsinnode = 10
  #n.minobsinnode has an important effect on overfitting!
  #decreasing this parameter increases the in-sample fit, 
  #but can result in overfitting
  
  , nTrain = round(end_trn * 0.8)
  #use this so that you can select the number of trees at the end
  
  # , var.monotone = c(3) 
  #can help with overfitting, will smooth bumpy curves
  
  , verbose = FALSE #print the preliminary output
)  

#look at the last model built
#Relative influence among the variables can be used in variable selection
summary(Model)
#If you see one variable that's much more important than all of the rest,
#that could be evidence of overfitting.

#optimal number of trees based upon CV
gbm.perf(Model)

#look at the effects of each variable
for(i in 1:length(Model$var.names)){
  plot(Model, i.var = i
       , ntrees = gbm.perf(Model, plot.it = FALSE) #optimal number of trees
       , type = "response" #to get fitted probabilities
  )
}

################ Make predictions ##################
#test set predictions
TestPredictions = predict(object = Model,newdata =all[(end_trn+1):end,]
                          , n.trees = gbm.perf(Model, plot.it = FALSE)
                          , type = "response") #to output a probability
#training set predictions
TrainPredictions = predict(object = Model,newdata =all[1:end_trn,]
                           , n.trees = gbm.perf(Model, plot.it = FALSE)
                           , type = "response")

#round the predictions to zero or one
TestPredictions = round(TestPredictions)
TrainPredictions = round(TrainPredictions)

head(TrainPredictions, n = 20)
head(disease, n = 20)



#in sample classification accuracy
1 - sum(abs(disease - TrainPredictions)) / length(TrainPredictions) 
