#Different library for the multinomial calssiffication for better performance
require(nnet)
require(RCurl)

#Download a custom library that i've found to plot the neural network
root.url<-'https://gist.githubusercontent.com/fawda123'
raw.fun<-paste(
  root.url,
  '5086859/raw/cc1544804d5027d82b70e74b83b3941cd2184354/nnet_plot_fun.r',
  sep='/'
)
script<-getURL(raw.fun, ssl.verifypeer = FALSE)
eval(parse(text = script))
rm('script','raw.fun')


#Read data from .csv file (this dataset is a trimmed version of the original, without columns 2, 6 and 7)
#df <-read.csv("clevelandCopy3.csv")
df <- read.csv("cleveland.csv", sep = ",", na.strings = "?")
tmp <- NULL
s <- sum(is.na(df))
df <- na.omit(df)

df_train <- sample(1:297, 208)
df_test <- setdiff(1:297, df_train)

#Run 10 times the algorithm to calculate the mean of the performance of every iteration the algorithm runs
for(i in 1:10){
  
  ideal <- nnet::class.ind(df$num)
  
  #Train the neural network
  df_ANN <- nnet::nnet(df[df_train, -14], ideal[df_train, ], size = 50,softmax = TRUE, maxit = 2000, Hess = FALSE)
  #predict(df_ANN, df[df_train, -14], type = "class")
  
  #Gather the predicted results of every iteration
  tmp[i] <- sum(diag(table(predict(df_ANN, df[df_test, -14], type = "class"), df[df_test, ]$num)))/89
  
}

#Calculate the mean performance of the aglorithm
print(mean(tmp) * 100)

#Plot the neuraln network
par(mar=numeric(4),family='serif')
plot.nnet(df_ANN,pos.col='darkgreen',neg.col='darkblue',alpha.val=0.7,rel.rsc=15,
          circle.cex=10,cex=1.4,
          circle.col='brown')

