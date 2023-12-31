#### Figure 9.9
library(rpart)
library(rpart.plot)
library(caret)

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.

# partition
set.seed(1)  
train.index <- sam?sampleple(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# classification tree
default.ct <- rpart(Personal.Loan ~ ., data = train.df ,method = "class")

# plot tree
prp(default.ct, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
# count number of leaves
length(default.ct$frame$var[default.ct$frame$var == "<leaf>"])


default.info.ct <- rpart(Personal.Loan ~ ., data = train.df, parms = list(split = 'information'), method = "class")
prp(default.info.ct, type = 1, extra = 2, under = TRUE, split.font = 1, varlen = -10)
length(default.info.ct$frame$var[default.info.ct$frame$var == "<leaf>"])

#### Figure 9.10

deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = -1, minsplit = 1)
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  




#### Table 9.3

# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.
default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$Personal.Loan))

### repeat the code for the validation set, and the deeper tree

default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$Personal.Loan))

deeper.ct.point.pred.train <- predict(deeper.ct,train.df,type = "class")
confusionMatrix(deeper.ct.point.pred.train, as.factor(train.df$Personal.Loan))
deeper.ct.point.pred.valid <- predict(deeper.ct,valid.df,type = "class")
confusionMatrix(deeper.ct.point.pred.valid, as.factor(valid.df$Personal.Loan))

#### Figure 9.13

# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.

set.seed(1)
cv.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0.00001, minsplit = 1, xval = 5)  # minsplit is the minimum number of observations in a node for a split to be attempted. xval is number K of folds in a K-fold cross-validation.
printcp(cv.ct)  # Print out the cp table of cross-validation errors. The R-squared for a regression tree is 1 minus rel error. xerror (or relative cross-validation error where "x" stands for "cross") is a scaled version of overall average of the 5 out-of-sample errors across the 5 folds.
pruned.ct <- prune(cv.ct, cp = 0.0068729)

printcp(pruned.ct)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 


####Figure 9.15

library(randomForest)
## random forest
rf <- randomForest(as.factor(Personal.Loan) ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)


## confusion matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, as.factor(valid.df$Personal.Loan))




#### Table 9.5

library(adabag)

train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)

set.seed(1)
boost <- boosting(Personal.Loan ~ ., data = train.df)
pred <- predict(boost, valid.df)
confusionMatrix(as.factor(pred$class), as.factor(valid.df$Personal.Loan))
