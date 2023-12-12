rm(list=ls()) ; gc()

library(tidyverse)
library(readr)
library(corrplot)
library(scales)
library(dplyr)

library(plyr)
library(caret) 
library(e1071) 
library(randomForest) 
library(ggplot2)
library(VIM) # for missing values
library(corrplot)


df <- read.csv("UCI_Credit_Card.csv")
df<- df%>% select(-1)

head(df)


str(df)
# There are no Object collumns or NA colums thus we can move on to Cleansing the Data
#It also helps that these values are all numerical/integer as we can issue a correlation heatmap

##Checking the missing value

aggr(df, numbers = TRUE, prop = c(TRUE, FALSE))


colnames(df)[colnames((df)) == "default.payment.next.month"] = "DEFAULT"


df$DEFAULT = as.numeric(df$DEFAULT)
r = cor(df[-c(3, 4, 5, 26, 27)])
corrplot(r, method = "circle")


#We can change our column types to ensure that our algorithm does not mistake our 
# Ordinal values for our nominal and Scale our values to better help our algorithm

df$SEX<- as.factor(df$SEX)
df$EDUCATION<- as.factor(df$EDUCATION)
df$MARRIAGE <- as.factor(df$MARRIAGE)


df <- df%>%
  mutate(across(where(is.numeric), scale))%>%
  mutate(DEFAULT = as.factor(DEFAULT))

df$GENDER = ifelse(df$SEX == 1, "Male", "Female")

ggplot(data = df, mapping = aes(x = GENDER, fill = DEFAULT)) +
  geom_bar() +
  ggtitle("Gender") +
  stat_count(aes(label = ..count..), geom = "label")

ggplot(data = df, mapping = aes(x = EDUCATION, fill = DEFAULT)) +
  geom_bar() +
  ggtitle("EDUCATION") +
  stat_count(aes(label = ..count..), geom = "label")


ggplot(data = df, mapping = aes(x = MARRIAGE , fill = DEFAULT)) +
  geom_bar() +
  xlab("Marital status") +
  ggtitle(" Defaulters on Marital Status") +
  stat_count(aes(label = ..count..), geom = "label")

df$PAY_0 = as.factor(df$PAY_0)

calDiffBillAndPayPer = function(x, bill_amt_ind, pay_amt_ind){
  TOTAL_PAYABLE_PERC = c()
  
  for (i in 1:nrow(x)) {
    sum_bill_amt = sum(x[i, bill_amt_ind])
    sum_pay_amt = sum(x[i, pay_amt_ind])
    
    payable_amt =  sum_bill_amt - sum_pay_amt
    if(sum_bill_amt != 0){
      payable_perc = round(payable_amt / sum_bill_amt * 100, digits = 2)
    }
    else{
      payable_perc = 0.00
    }
    
    TOTAL_PAYABLE_PERC = c(TOTAL_PAYABLE_PERC, payable_perc)
  }
  return(TOTAL_PAYABLE_PERC)
}

#REMAINING_AMT_PER is the percentage of difference of total Bill_Amt of 6 months </br> and total Pay_Amt of 6 months.

check = calDiffBillAndPayPer(x = df, bill_amt_ind = c(11:16), pay_amt_ind = c(17:22))
df$REMAINING_AMT_PER = check

modified.df <- df[, -25]

str(modified.df)
  
train_portion = .7


train_index <- createDataPartition(modified.df$DEFAULT,p = train_portion, list = FALSE,times = 1)
trainset <- modified.df[train_index,]
testset <- modified.df[-train_index,]


#Checking the distribution of our DEFAUT variable in our data sets
# our full data set
prop.table(table(modified.df$DEFAULT))

# our training set
prop.table(table(trainset$DEFAULT))

# our testing set
prop.table(table(testset$DEFAULT))

#Fitting a Random Forest Classifier to our training set
classifier.rf = randomForest(formula = DEFAULT ~., data = trainset, ntree = 50)

varImpPlot(classifier.rf)
summary(classifier.rf)


our.predict.rf = predict(classifier.rf, newdata = testset, type = "class")

confusionMatrix(our.predict.rf, testset$DEFAULT)


#boosting algorithm

library(adabag)


set.seed(1)
boost <- boosting(DEFAULT ~ ., data = trainset)
pred <- predict(boost, testset)
confusionMatrix(as.factor(pred$class), testset$DEFAULT)

