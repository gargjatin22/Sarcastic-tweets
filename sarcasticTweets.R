
rm(list=ls()) # to remove from global Environment
getwd()
setwd("C:/Users/jatin/Documents/Rcode") # to set working directory


dataset <- read.csv("TweetsDataSet.csv",stringsAsFactors = F) # reading TweetsDataSet.csv file into dataset
datarows<- sample(1:nrow(dataset),20000)# taking 20,000 rows out of total rows
dataset<- dataset[datarows,] # taking sample from dataset

barplot(table(dataset$label),col=c("green","blue"),width = .5,xlab="labels",ylab="frequency")
# installing packages

#install.packages("tm")
#install.packages("SnowballC")
#install.packages("caTools")
#install.packages("wordcloud")

#loading packages 
library(tm)
library(SnowballC)
library(caTools)
library(wordcloud)

#pre-processing
Corpus <- VCorpus(VectorSource(dataset$tweet)) # convert into corpus only tweets

Corpus<- tm_map(Corpus,content_transformer(tolower)) # convert uppercase to lower case

Corpus<- tm_map(Corpus,removeNumbers) # remove numbers

Corpus<- tm_map(Corpus,removePunctuation) # remove punctuation

Corpus<- tm_map(Corpus,removeWords,stopwords()) # remove stopwords

Corpus<- tm_map(Corpus,stemDocument) # stemming

Corpus <- tm_map(Corpus,stripWhitespace) # remove white spaces


# word Cloud
wordcloud(Corpus, max.words = 200, scale=c(3, .1), colors=brewer.pal(6, "Dark2")) # world cloud

#dtm
dtm <- DocumentTermMatrix(Corpus) # convert corpus into document term matrix
dtm<- removeSparseTerms(dtm,0.999) # Reducing sparsity by 0.999%  

#Data frame
dataset1<- as.data.frame(as.matrix(dtm)) # convert Document term matrix to data frame
dataset$label <- factor(dataset$label,labels = c(0,1)) # convert target veriable into factor
dataset1$label <- dataset$label# add target variable to our dataset
dataset<- dataset1 # changing name to avoid confustion 

#training and testing data
set.seed(123) # set seed
split = sample.split(dataset$label,SplitRatio = 0.7) # split into training and testing data
training_set = subset(dataset,split == T) # training data
test_set = subset(dataset,split==F) # test data



#Naive Bayes  classification model
#install.packages("e1071")
library(e1071) # loading library for naivebayes
classifier = naiveBayes(x= training_set[,-ncol(training_set)],y = training_set$label) # building classification model using naive bayes
save(classifier,file = "naiveBayes_classifier.rda") # to save the model
load(file = "naiveBayes_classifier.rda") # to load the model

y_pred = predict(classifier,newdata = test_set[,-ncol(training_set)]) # making prediction on test data using our model

cm = table(test_set[,ncol(training_set)],y_pred)# Confusion matrix
cm #print confusion matrix

accuracy <- (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
accuracy

#Random FOrest
#install.packages("randomForest")
library(randomForest) # loading random forest library
classifier_rm = randomForest(x= training_set[,-ncol(dataset)],y = training_set$label,ntree = 15) # building classification model using random forest
 

y_pred_rm = predict(classifier_rm,newdata = test_set[,-ncol(dataset)]) # making prediction on test data using our model

cm_rm = table(test_set[,ncol(dataset)],y_pred_rm) # Confusion matrix
cm_rm # print confusion matrix

accuracy_rm <- (cm_rm[1,1]+cm_rm[2,2])/(cm_rm[1,1]+cm_rm[2,2]+cm_rm[1,2]+cm_rm[2,1])
accuracy_rm


