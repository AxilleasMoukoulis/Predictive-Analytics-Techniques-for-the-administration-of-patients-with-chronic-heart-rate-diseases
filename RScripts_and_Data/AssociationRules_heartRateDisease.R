#rm(list = ls())
#Required libraries
require(arules)
require(arulesViz)

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

levels(df$num) <- c("No disease", "Disease")
levels(df$sex) <- c("female", "male", "")
levels(df$cp) <- c("typical angina", "atypical angina", "non-anginal pain", "asymptomatic")
levels(df$restecg) <- c("normal", "wave abnormality", "probable or definite left ventricular hypertrophy")
levels(df$exang) <- c("No", "Yes")
levels(df$slope) <- c("upsloping", "flat", "downsloping") 
levels(df$thal) <- c("normal", "fixed defected", "reversable defect") 
levels(df$fbs) <- c("< 120 mg/dl", "> 120 mg/dl")

#Remove the rows with empty cells
s <- sum(is.na(df))
df <- na.omit(df)
dim(df)

#A priori algorithm
rules <- apriori(df,parameter = list(minlen=4,maxlen=13, supp=0.15, conf=0.8),appearance = list(rhs=c("num=No disease", "num=Disease"),default="lhs"))
rules.sorted <- sort(rules, by="lift")

#View the 15 first rules
inspect(head(rules.sorted,15))


