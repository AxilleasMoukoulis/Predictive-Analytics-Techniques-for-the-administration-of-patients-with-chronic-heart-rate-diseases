#rm(list = ls())
library(OneR)

df <- read.csv("cleveland.csv", sep = ",", na.strings = "?")
head(df)
dim(df)

df$num[df$num >0] <- 1

s <- sum(is.na(df))
df <- na.omit(df)
dim(df)

set.seed(12) # for reproducibility
random <- sample(1:nrow(df), 0.8 * nrow(df))
data_train <- optbin(df[random, ], method = "infogain")

data_test <- df[-random, ]

model_train <- OneR(data_train, verbose = TRUE)
summary(model_train)
plot(model_train)

prediction <- predict(model_train, data_test)
eval_model(prediction, data_test)

