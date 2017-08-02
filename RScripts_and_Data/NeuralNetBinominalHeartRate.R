#rm(list = ls())
require(ISLR)
require(caTools)
require(neuralnet)

df <- read.csv("cleveland.csv", sep = ",", na.strings = "?")
df$num[df$num >0] <- 1
s <- sum(is.na(df))
df <- na.omit(df)

#Maximum value for every column
maxs <- apply(df[,1:13], 2, max)

#Minimum value for every column
mins <- apply(df[,1:13], 2, min)

#Data scaling based on mins and maxs
scaled.data <- as.data.frame(scale(df[,1:13],center = mins, scale = maxs - mins))

Num = as.numeric(df$num)
#The full dataset with the independent value and the scaled data
data = cbind(Num,scaled.data)

set.seed(101)
#Split data - 80% training set - 20% test set
split = sample.split(data$Num, SplitRatio = 0.80)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

#The features of every column
feats <- names(scaled.data)

#Build the formula for the algorithm: "Num ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal"
f <- paste(feats,collapse=' + ')
f <- paste('Num ~',f)
f <- as.formula(f)

#The neural network with 2 hidden layers
nn <- neuralnet(f,train,hidden= c(2,2),linear.output=FALSE)

#Prediction based on the model
predicted.nn.values <- compute(nn,test[2:14])
print(head(predicted.nn.values$net.result))

#Confusion matrix of the results
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(test$Num,predicted.nn.values$net.result)

#View Neural Network
plot(nn)
