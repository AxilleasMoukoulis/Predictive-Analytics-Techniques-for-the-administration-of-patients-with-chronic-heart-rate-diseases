#rm(list = ls())

#Required libraries
require(class)
require(gmodels)

#Read data from csv file (this dataset is a trimmed version of the original, without the 2, 6 and 7 columns)
df <- read.csv("clevelandCopy3.csv")

#Normalize each data so that all the values can have a value between 0 and 1, helps for better performance of the algorithm
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
df_normalize <- as.data.frame(lapply(df[1:11], normalize))

#Split data to train set (84%) and test set (16%)
df_train <- df_normalize[1:250, ]
df_test <- df_normalize[251:297, ]

df_train_labels <- df[1:250, 11]
df_test_labels <- df[251:297, 11]

#KNN algorithm with k = 1
df_test_pred <- knn(train = df_train, test = df_test, cl = df_train_labels, k = 1,prob = TRUE)

#Confusion matrix of the results
CrossTable(x = df_test_labels, y = df_test_pred, prop.chisq = FALSE)

#The labels of the testing set so we can compare the overall values with the predicted form the algoritm
table(df_test_labels)
