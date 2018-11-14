# setting work dir

setwd("~/R/kaggle project/New folder (2)/house")

# load train dataset

train <- read.csv(file = "train.csv",stringsAsFactors = FALSE, header = TRUE)

# load test dataser

test <- read.csv(file = "test.csv",stringsAsFactors = FALSE, header=TRUE)

# Assigning name to field names as variables
variables <- names(train)

# removing SalesPrice field

variables <- variables[variables != "SalePrice"]

print(variables)

# to treat NA values in character fields and replace NA with "Not Available"

for(variable in variables)
{
  if(any(is.na(train[[variable]])))
  {
    if(is.character(train[[variable]]))
    {
      train[[variable]][is.na(train[[variable]])] <- "Not Available"
    }
    else
    {
      train[[variable]][is.na(train[[variable]])] <- mean(train[[variable]],na.rm=TRUE)
    }
  }
  if(any(is.na(test[[variable]])))
  {
    if(is.character(test[[variable]]))
    {
      test[[variable]][is.na(test[[variable]])] <- "Not Available"
    }
    else
    {
      test[[variable]][is.na(test[[variable]])] <- mean(test[[variable]],na.rm=TRUE)
    }
  }
}

# to treat NA values in factor fields and replace NA with "Not Available"
for(variable in variables)
{
  if(is.character(train[[variable]]))
  {
    levels <- sort(unique(c(train[[variable]],test[[variable]])))
    train[[variable]] <- factor(train[[variable]],levels=levels)
    test[[variable]] <- factor(test[[variable]],levels=levels)
  }
}
library(randomForest)

library(mlbench)

library(caret)

#random forest

trainforest <- randomForest(SalePrice~.,data = train)

trainforest

#calculate best m try


bestmtry <- tuneRF(train,train$SalePrice,stepFactor = 1.2,improve = 0.01,trace = T,plot = T)

#gini imporatnce of trainforest to find the significant variables and plot for easy visualization

importance(trainforest)

varImpPlot(trainforest)

#to predict i.e to check accuracy

pred <- predict(trainforest,newdata = test,type = "class")

pred

test$SalePrice <- pred

library(ggplot2)

library(lattice)

library(caret)

confusionMatrix(table(pred,test$SalePrice))

table(pred,test$SalePrice)

test$SalePrice
