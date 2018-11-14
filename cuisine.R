setwd("~/R/kaggle project/cuisine/cuisine/aj")

#install required libraries

library(jsonlite)

library(tm)

library(SnowballC)

library(randomForest)

train <- fromJSON("train.json")

test <- fromJSON("test.json")

library(tm)

#create corpus train

train <- Corpus(VectorSource(train$ingredients))

#convert text to lower case

train <- tm_map(train, tolower)

train[[1]]

#remove punctuation

train <- tm_map(train, removePunctuation)

train[[1]]

#remove stopwords

train <- tm_map(train, removeWords, c(stopwords('english')))

train[[1]]

#remove whitespaces

train <- tm_map(train, stripWhitespace)

train[[1]]

#stemming

train <- tm_map(train, stemDocument)

train[[1]]

#create corpus test

test <- Corpus(VectorSource(test$ingredients))

#convert text to lower case

test <- tm_map(test, tolower)

test[[1]]

#remove punctuation

test <- tm_map(test, removePunctuation)

test[[1]]

#remove stopwords

test <- tm_map(test, removeWords, c(stopwords('english')))

test[[1]]

#remove whitespaces

test <- tm_map(test, stripWhitespace)

test[[1]]

#stemming

test <- tm_map(test, stemDocument)

test[[1]]

dtmtrain <- DocumentTermMatrix(train) 

dtmtest <- DocumentTermMatrix(test)

sparse <- removeSparseTerms(dtmtrain, 0.98) 

sparse2 <- removeSparseTerms(dtmtest, 0.98) 

trainingredients <- as.data.frame(as.matrix(sparse)) 

testingredients <- as.data.frame(as.matrix(sparse2))

trainColumns<-names(trainingredients) 

testColumns<-names(testingredients)

intersect<-intersect(trainColumns,testColumns)
ingredientsDTM<- ingredientsDTM[,c(intersect)] 
ingredientsDTM2<- ingredientsDTM2[,c(intersect)] 
ingredientsDTM$cuisine <- as.factor(train$cuisine)
names(ingredientsDTM) <- gsub("-", "", names(ingredientsDTM)) 
names(ingredientsDTM2) <- gsub("-", "", names(ingredientsDTM2))


library(randomForest)
forestmodel <- randomForest(cuisine ~., data=ingredientsDTM, importance=TRUE, ntree=50) 
forestPredict<-predict(forestmodel, newdata = ingredientsDTM2, type = "class")
submission <- data.frame(id = test$id, cuisine = forestPredict)
write.csv(submission, "submission.csv", quote = FALSE, row.names = FALSE)