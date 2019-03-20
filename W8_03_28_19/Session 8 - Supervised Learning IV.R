# TA: Leslie Huang
# Course: Text as Data
# Date: 3/22/2018
# Recitation 8: Supervised Learning IV

# Setup
rm(list = ls())
set.seed(100)
setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/W8_03_22_18/")


# Questions from last time ...

# 1 Tuning hyperparameters for SVM? ** Beyond the scope of this class **

# install.packages("e1071")
library(e1071)

# Simple example here: https://rischanlab.github.io/SVM.html

# Many other machine learning tools are available in that library as well.

# NB: e1071 uses libsvm(), a Python library. And RTextTools, the package from last time, uses e1071.
# However, if you're using Python, sklearn has a great SVM implementation!


# 2 More SVM

library(RTextTools)
library(tm)
library(quanteda)
library(quanteda.corpora)

# 2.1 

data("data_corpus_ukmanifestos")
manifesto_corpus <- corpus(data_corpus_ukmanifestos)

# SVM is very difficult with more than 2 classes, so let's subset to Labour and Conservative only
manifesto_corpus <- corpus_subset(manifesto_corpus, Party == "Lab" | Party == "Con")

# Class labels are the party labels
manifesto_labels <- docvars(manifesto_corpus, "Party")

manifesto_dfm <- dfm(manifesto_corpus)

manifesto_mat <- quanteda::convert(manifesto_dfm, "matrix") # convert to matrix format

# Now we're going to do 1-fold cross-validation... "by hand" (without using the cross_validate function)

# Create containers
training_break <- floor( 0.9 * length(manifesto_labels) )

training_manifesto_dtm <- manifesto_mat[1:training_break, ]
test_manifesto_dtm <- manifesto_mat[(training_break + 1 ): length(manifesto_labels), ]

train_manifesto_container <- create_container(training_manifesto_dtm, 
                                              manifesto_labels[1:training_break], 
                                              trainSize = 1:nrow(training_manifesto_dtm), 
                                              virgin = FALSE
                                              )

test_manifesto_container <- create_container(test_manifesto_dtm, 
                                             manifesto_labels[training_break + 1 : length(manifesto_labels)], 
                                             trainSize = 1:nrow(test_manifesto_dtm), 
                                             virgin = FALSE
                                             )

# Train a model on the training data

manifesto_train_svm <- train_model(train_manifesto_container, "SVM", kernel = "linear")

# Predict the test data
classify_model(test_manifesto_container, manifesto_train_svm)

# Accuracy?

manifesto_labels[(training_break+1):length(manifesto_labels)]

# 2/4 correct... Not great!



# 3 SVM with tf-idf or raw term frequencies: which has better performance?

# We might expect that tf-idf is better because it upweights terms that discriminate more between documents 

# 3.1 Compare radial/linear SVM over tf/tf-idf inputs

# Modified example from https://rpubs.com/bmcole/reuters-text-categorization

# Another great library!
# install.packages("caret")

library(caret)


# 3.2 Wrangle in the data

r8train <- read.table("r8-train-all-terms.txt", header=FALSE, sep='\t')
r8test <- read.table("r8-test-all-terms.txt", header=FALSE, sep='\t')
# Data credit: https://www.cs.umb.edu/~smimarog/textmining/datasets/


# rename variables
names(r8train) <- c("Class", "docText")
names(r8test) <- c("Class", "docText")

# convert the document text variable to character type
r8train$docText <- as.character(r8train$docText)
r8test$docText <- as.character(r8test$docText)

# create varible to denote if observation is train or test
r8train$train_test <- c("train")
r8test$train_test <- c("test")

# merge the train/test data
merged <- rbind(r8train, r8test)

# remove objects that are no longer needed 
remove(r8train, r8test)

# subset to 3 document classes only for sake of computational expense/memory
merged <- merged[which(merged$Class %in% c("grain", "ship", "trade")),]

# drop unused levels in the response variable
merged$Class <- droplevels(merged$Class) 

# counts of each class in the train/test sets
table(merged$Class, merged$train_test) 

rownames(merged) <- NULL # reset rownames to numbers


# 3.3 Create corpus, preprocess, DTM
sourceData <- VectorSource(merged$docText)

# create the corpus
corpus <- Corpus(sourceData)

# preprocess/clean the training corpus
corpus <- tm_map(corpus, content_transformer(tolower)) # convert to lowercase
corpus <- tm_map(corpus, removeNumbers) # remove digits
corpus <- tm_map(corpus, removePunctuation) # remove punctuation
corpus <- tm_map(corpus, stripWhitespace) # strip extra whitespace
corpus <- tm_map(corpus, removeWords, stopwords('english')) # remove stopwords

# create term document matrix (tdm)
tdm <- DocumentTermMatrix(corpus)

# create tf-idf weighted version of term document matrix
weightedtdm <- weightTfIdf(tdm)


# 3.4 TDM --> DF for test/training 

# convert tdm's into data frames 
tdm_df <- as.data.frame(as.matrix(tdm))
weightedtdm_df <- as.data.frame(as.matrix(weightedtdm))

# split back into train and test sets
tdmTrain <- tdm_df[which(merged$train_test == "train"), ]
weightedTDMtrain <- weightedtdm_df[which(merged$train_test == "train"), ]

tdmTest <-  tdm_df[which(merged$train_test == "test"), ]
weightedTDMtest <- weightedtdm_df[which(merged$train_test == "test"), ]

# append document labels as last column
tdmTrain$doc.class <- merged$Class[which(merged$train_test == "train")]
tdmTest$doc.class <- merged$Class[which(merged$train_test == "test")]
weightedTDMtrain$doc.class <- merged$Class[which(merged$train_test == "train")]
weightedTDMtest$doc.class  <- merged$Class[which(merged$train_test == "test")]


# 3.5 Linear SVM + tf-idf

# set resampling scheme: 10-fold cross-validation, 3 times
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# SVM using the weighted (td-idf) term document matrix
# kernel: linear 
# tuning parameters: C 

load("svm_knn_workspace.RData") # this will take too long to run here!

svm.tfidf.linear  <- train(doc.class ~ . , data = weightedTDMtrain, trControl = ctrl, method = "svmLinear")

# 3.6 Radial SVM + tf-idf

# tuning parameters: sigma, C 
svm.tfidf.radial  <- train(doc.class ~ . , data=weightedTDMtrain, trControl = ctrl, method = "svmRadial")

# predict on test data
svm.tfidf.linear.predict <- predict(svm.tfidf.linear,newdata = weightedTDMtest)
svm.tfidf.radial.predict <- predict(svm.tfidf.radial,newdata = weightedTDMtest)


# 3.7 Linear SVM + unweighted dfm
 
# tuning parameters: C 
svm.linear  <- train(doc.class ~ . , data=tdmTrain, trControl = ctrl, method = "svmLinear")

# 3.8 Radial SVM + unweighted

# tuning parameters: sigma, C 
set.seed(100)
svm.radial  <- train(doc.class ~ . , data=tdmTrain, trControl = ctrl, method = "svmRadial")

# predict on test data
svm.linear.predict <- predict(svm.linear,newdata = tdmTest)
svm.radial.predict <- predict(svm.radial,newdata = tdmTest)


# 3.9 Performance

# Weighted:

svm.tfidf.linear # linear kernel
svm.tfidf.radial # radial basis function kernel

plot(svm.tfidf.radial)

# Unweighted:

svm.linear
svm.radial

plot(svm.radial)


# confusion matrices allow you to evaluate accuracy and other metrics
confusionMatrix(svm.linear.predict, tdmTest$doc.class)  # linear kernel, unweighted TDM

confusionMatrix(svm.radial.predict, tdmTest$doc.class)  # radial kernel, unweighted TDM ** Best performance!

confusionMatrix(svm.tfidf.linear.predict, weightedTDMtest$doc.class) # linear kernel, weighted TDM

confusionMatrix(svm.tfidf.radial.predict, weightedTDMtest$doc.class) # radial kernel, weighted TDM

# print various info about parameters, etc. used in the model with highest accuracy
svm.radial$results # error rate and values of tuning parameter

svm.linear$bestTune # final tuning parameter

svm.linear$metric # metric used to select optimal model


# 4 KNN -- also from https://rpubs.com/bmcole/reuters-text-categorization

# set resampling scheme
ctrl_knn <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# 4.1 fit a kNN model using the weighted (td-idf) term document matrix

# tuning parameter: K
knn.tfidf <- train(doc.class ~ ., data = weightedTDMtrain, method = "knn", trControl = ctrl_knn)

# predict on test data
knn.tfidf.predict <- predict(knn.tfidf, newdata = weightedTDMtest)

# 4.2 fit a kNN model using the unweighted TDM
# tuning parameter: K

knn <- train(doc.class ~ ., data = tdmTrain, method = "knn", trControl = ctrl_knn)

# predict on test data
knn.predict <- predict(knn, newdata = tdmTest)

# 4.3 Performance

knn.tfidf

knn

# plot accuracy vs. number of neighbors

plot(knn.tfidf)

# confusion matrices allow you to evaluate accuracy and other metrics
confusionMatrix(knn.predict, tdmTest$doc.class)  # unweighted TDM

confusionMatrix(knn.tfidf.predict, weightedTDMtest$doc.class)  # weighted TDM
plot(knn)

# print info about parameters, etc. used in the model with highest accuracy
knn$results # error rate and values of tuning parameter

knn$bestTune # final tuning parameter

save.image("svm_knn_workspace.RData")
