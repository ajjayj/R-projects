#setting working project directory

setwd("~/R/kaggle project/New folder/final")

#reading data into dataframe df

df<- read.csv(file= "HR_comma_sep.csv", stringsAsFactors = FALSE ,header = TRUE)

summary(df)

View(df)

library(ggplot2)

ggplot(df,aes(x=salary))+
  geom_bar()

table(df$salary)
# high 1237 0.824 low 7316 0.487 medium 6446 0.429
prop.table(table(df$salary))

ggplot(df,aes(x=left))+
  geom_bar()

#to see how many left
table(df$left)
# total=14999 0-11428 1-3571

#to see how many left in percentages
prop.table(table(df$left))
#0-0.7619175  1-0.2380825

ggplot(df,aes(x=left),fill=satisfaction_level)+
  theme_bw()+
  geom_histogram()

plot(df$left,df$satisfaction_level)

#converting into factors

df$sales <-as.factor(df$sales)

df$salary <- as.factor(df$salary)

df$left <- as.factor(df$left)

#to split the data

library(caTools)

split <- sample.split(df,SplitRatio = 0.7)

split       

train <- subset(df,split == "TRUE")

test <- subset(df,split =="FALSE")

library(mlbench)

library(caret)

library(randomForest)

#random forest

trainforest <- randomForest(left~.,data = train)

#trainforest <- randomForest(left~ satisfaction_level+time_spend_company+number_project,data = train)

trainforest

#calculate best m try


bestmtry <- tuneRF(train,train$left,stepFactor = 1.2,improve = 0.01,trace = T,plot = T)

#gini imporatnce of trainforest to find the significant variables and plot for easy visualization

importance(trainforest)

varImpPlot(trainforest)

#to predict i.e to check accuracy

pred <- predict(trainforest,newdata = test,type = "class")

pred

library(ggplot2)

library(lattice)

library(caret)

confusionMatrix(table(pred,test$left))

