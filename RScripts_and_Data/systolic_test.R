#rm(list = ls())
library(fields)
library(stringi)

df <- read.csv("SystolicWithout.csv", sep = ",", na.strings = "", stringsAsFactors=FALSE);

#Delete N/A values, the dataset gets smaller
#s <- sum(is.na(df))
#df <- na.omit(df)

#Avarage for Glucose
test.minimum <-NULL
test.maximum <- NULL
df$glucoseAvg = NA
df$glucoseDng = NA
threshold <- 50
min_glucose_value <- 20
for(i in seq_along(df$glucose)){
  
  if( !is.na(df$glucose[i])   ){
  
    num <- stri_extract_all_regex(df$glucose[i], "[0-9]+")
    num <- as.integer(unlist(num))
    
    #print(num[i])
    
    avg <- 0
    minimum <- num[1]
    maximum <- num[1]
    
    for(j in seq_along(num)){
      
      if( num[j] > min_glucose_value ){
      
        if( num[j] < minimum ){
          minimum <- num[j]
        }
        
        if( num[j] > maximum ){
          maximum <- num[j]
        }
        
        avg <- avg + num[j]
      } 
    }
  
    avg <- avg/length(num)
    test.minimum[[i]] <- minimum
    test.maximum[[i]] <- maximum
    
    if( (maximum - minimum) > threshold ){
      df$glucoseDng[[i]] <- "Danger"
    } else {
      df$glucoseDng[[i]] <- "Normal"
    }
    
    df$glucoseAvg[[i]] <- avg
  }
} 

#Avarage for Systolic
df$systolicAvg = NA
df$systolicNormalValue = NA
for(i in seq_along(df$systolic)){
  
  if(!is.na(df$systolic[i]) ){
  
    num <- stri_extract_all_regex(df$systolic[i], "[0-9]+")
    num <- as.integer(unlist(num))
    
    avg <- 0
    for(j in seq_along(num)){
      avg <- avg + num[j]
    }
    avg <- avg/length(num)
    df$systolicAvg[[i]] <- avg
    
    if( avg > 120 ){
      df$systolicNormalValue[[i]] <- "Above Normal"
    } else {
      df$systolicNormalValue[[i]] <- "Normal"
    }
  }
} 

#Avarage for Diastolic
df$diastolicAvg = NA
df$diastolicNormalValue = NA
for(i in seq_along(df$diastolic)){
  
  if(!is.na(df$diastolic[i])){
  
    num <- stri_extract_all_regex(df$diastolic[i], "[0-9]+")
    num <- as.integer(unlist(num))
    
    avg <- 0
    for(j in seq_along(num)){
      avg <- avg + num[j]
    }
    avg <- avg/length(num)
    df$diastolicAvg[[i]] <- avg
    
    if( avg > 80 ){
      df$diastolicNormalValue[[i]] <- "Above Normal"
    } else {
      df$diastolicNormalValue[[i]] <- "Normal"
    }
  }
} 

#Avarage for HR
df$hrAvg = NA
df$hrNormalValue = NA
for(i in seq_along(df$hr)){
  
  if(!is.na(df$hr[i])){
  
    num <- stri_extract_all_regex(df$hr[i], "[0-9]+")
    num <- as.integer(unlist(num))
    
    avg <- 0
    for(j in seq_along(num)){
      avg <- avg + num[j]
    }
    avg <- avg/length(num)
    df$hrAvg[[i]] <- avg
    
    if( avg > 100 ){
      df$hrNormalValue[[i]] <- "Above Normal"
    }else if(avg < 60) {
      df$hrNormalValue[[i]] <- "Below Normal"
    }else {
      df$hrNormalValue[[i]] <- "Normal"
    }
  }
} 

#Avarage for Cholesterol
df$cholesterolAvg = NA
df$cholesterolcNormalValue = NA
for(i in seq_along(df$cholesterol)){
  
  if(!is.na(df$cholesterol[i])){
  
    num <- stri_extract_all_regex(df$cholesterol[i], "[0-9]+")
    num <- as.integer(unlist(num))
    
    avg <- 0
    for(j in seq_along(num)){
      avg <- avg + num[j]
    }
    avg <- avg/length(num)
    df$cholesterolAvg[[i]] <- avg
    
    if( avg > 200 ){
      df$cholesterolcNormalValue[[i]] <- "Above Normal"
    } else {
      df$cholesterolcNormalValue[[i]] <- "Normal"
    }
  }
} 

#df$gender <- as.integer(df$gender)
#df$gender[df$gender == "male"] <- 0
#df$gender[df$gender == "female"] <- 1
#df$gender[df$gender == 2] <- 0

head(df)
dim(df)

#df$systolic <- as.integer(df$systolic)
#df$diastolic <- as.integer(df$diastolic)
#df$hr <- as.integer(df$hr)
#df$cholesterol <- as.integer(df$cholesterol)
#df$glucose <- as.integer(df$glucose)

#write.csv(df, file = "Systolic2.csv", row.names = FALSE)

fields::stats(df$systolicAvg, by=df$gender)
fields::stats(df$diastolic, by=df$gender)
fields::stats(df$hr, by=df$gender)
fields::stats(df$cholesterol, by=df$gender)
fields::stats(df$glucose, by=df$gender)

fields::stats(df$glucose, by=df$systolic)
fields::stats(df$glucose, by=df$diastolic)
fields::stats(df$diastolic, by=df$hr)
fields::stats(df$systolic, by=df$hr)
fields::stats(df$systolic, by=df$diastolic)

#https://oup.silverchair-cdn.com/oup/backfile/Content_public/Journal/aje/159/12/10.1093/aje/kwh160/2/kwh160.pdf?Expires=1497008068&Signature=fqTlEUW4nITCths418dakdZZpQxvxKQlTLemM7e~PuDp0UjGG8R39fitg-cxdBSwKQgcR7PPITf8NBHNPJTOf8~TI-HUUqhi03PWWpJokn~9QyrwYevAh61dKCtKdr1kyTHQ4Ge66b18VOiZtUclfhfaMVihRAAZYEAN0D4Vd7JJRJjd5YHHuIOzVgwEVOdS4uxs6LPH-bflT~uEb7PXyy~WqKGdtmFiUkJ015cqQKnL63iFb6Ze~azMq0hUZgRsXvEKXSQIn5K7RspwVsr-g9awUJiLzgItzpkvOrT3N~THnHogAAZ0wzSexKT1fN7h8ooDk9EZdyHZB-gqCTE2oA__&Key-Pair-Id=APKAIUCZBIA4LVPAVW3Q