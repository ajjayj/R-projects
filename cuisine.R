# setting working directory

setwd("~/Ajjay Project")

install.packages('jsonlite')

library(jsonlite)

# Importing train and test data 

train <- fromJSON("train.json")

test <- fromJSON("test.json")

#add dependent variable

test$cuisine <- NA

#combine data set

combi <- rbind(train, test)

#install package

install.packages('tm')

library(tm)

#create corpus

corpus <- Corpus(VectorSource(combi$ingredients))

#convert text to lower case

corpus <- tm_map(corpus, tolower)

corpus[[1]]

#remove punctuation

corpus <- tm_map(corpus, removePunctuation)

corpus[[1]]

#remove stopwords

corpus <- tm_map(corpus, removeWords, c(stopwords('english')))

corpus[[1]]

#remove whitespaces

corpus <- tm_map(corpus, stripWhitespace)

corpus[[1]]

#

install.packages('SnowballC')
install.packages("SnowballC")

library(SnowballC)

#stemming

corpus <- tm_map(corpus, stemDocument)

corpus[[1]]

corpus <- tm_map(corpus, PlainTextDocument)

View(corpus)

#View(Corpus())

#document matrix

frequencies <- DocumentTermMatrix(corpus) 

frequencies

#organizing frequency of terms

freq <- colSums(as.matrix(frequencies))

length(freq)

ord <- order(freq)

ord

m <- as.matrix(frequencies)
dim(m) write.csv(m, file = 'matrix.csv')


freq[head(ord)]
freq[tail(ord)]

head(table(freq),20)
tail(table(freq),20)

#remove sparse terms
sparse <- removeSparseTerms(frequencies, 1 - 3/nrow(frequencies))
dim(sparse)

#create a data frame for visualization
wf <- data.frame(word = names(freq), freq = freq)
head(wf)

#plot terms which appear atleast 10,000 times
library(ggplot2)

chart <- ggplot(subset(wf, freq >10000), aes(x = word, y = freq))
chart <- chart + geom_bar(stat = 'identity', color = 'black', fill = 'white')
chart <- chart + theme(axis.text.x=element_text(angle=45, hjust=1))
chart

#find associated terms
findAssocs(frequencies, c('salt','oil'), corlimit=0.30)

#create wordcloud
install.packages('wordcloud')
library(wordcloud)
set.seed(142)

#plot word cloud
wordcloud(names(freq), freq, min.freq = 2500, scale = c(6, .1), colors = brewer.pal(4, "BuPu"))

#plot 5000 most used words
wordcloud(names(freq), freq, max.words = 5000, scale = c(6, .1), colors = brewer.pal(6, 'Dark2'))
