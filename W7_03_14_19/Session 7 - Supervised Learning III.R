# TA: Pedro L. Rodríguez
# Course: Text as Data
# Date: 03/14/2019
# Lab adapted from: Kevin Munger, Patrick Chester and Leslie Huang.

#----------------------------------------
# Set up environment                     ---
#----------------------------------------
# clear global environment
rm(list = ls())

# load required libraries
library(dplyr)
library(RTextTools)

# set working directory
setwd("~/Drobox/GitHub/Text-as-Data-Lab-Spring-2019/W6_03_07_19/")

#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------
news_data <- readRDS("news_data.rds")
table(news_data$category)

# let's work with 2 categories
news_samp <- news_data %>% filter(category %in% c("WEIRD NEWS", "GOOD NEWS")) %>% select(headline, category) %>% setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "WEIRD NEWS"])
head(news_samp$text[news_samp$class == "GOOD NEWS"])

# some pre-processing (the rest will let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
news_samp$class <- recode(news_samp$class,  "WEIRD NEWS" = "weird", "GOOD NEWS" = "good")

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
set.seed(1984)
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL

#----------------------------------------
# 2. Support Vector Machine (SVM) using RTextTools ---
#----------------------------------------

# A. create document feature matrix (RTextTools has its own function)
news_dfm <- create_matrix(news_samp$text, language = "english", removePunctuation = TRUE, removeNumbers = TRUE, stemWords = TRUE)

# B. create a "container" (an RTextTools object) with training and test sets
training_break <- as.integer(0.9 * nrow(news_samp))  # define training set break
container <- create_container(matrix = news_dfm, 
                              labels = news_samp$class, 
                              trainSize = 1:training_break, 
                              testSize = (training_break + 1):nrow(news_dfm), 
                              virgin = FALSE)

# C. now we can train any number of models
print_algorithms()

# SVM - linear
svm.linear <- train_model(container, "SVM", kernel = "linear")

# SVM - radial
svm.radial <- train_model(container, "SVM", kernel = "radial")

# D. out of sample evaluation

# predict test set classes
svm.linear.classify <- classify_model(container, svm.linear)
svm.radial.classify <- classify_model(container, svm.radial)

# get confusion matrix
cmat <- table(news_samp$class[(training_break + 1):nrow(news_dfm)], svm.linear.classify$SVM_LABEL)
linear_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)

cmat <- table(news_samp$class[(training_break + 1):nrow(news_dfm)], svm.radial.classify$SVM_LABEL)
radial_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)

# baseline
baseline_acc <- max(prop.table(table(news_samp$class[(training_break + 1):nrow(news_dfm)])))

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  linear_acc, "\n",
  "SVM-Radial Accuracy:",  radial_acc
)

# each model has its own set of tuning parameters
?train_model

## Comments:
# linear vs radial kernels 
# radial more flexible BUT/HENCE can overfit
# linear kernel is faster
# nfold is the number of times you have a different test set

#----------------------------------------
# 3. Support Vector Machine (SVM) using Caret ---
#----------------------------------------
library(caret)
library(quanteda)

# create document feature matrix
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% convert("matrix")

# A. the caret package has it's own partitioning function
set.seed(1984)
ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "none")
#trctrl <- trainControl(method = "LOOCV", p = 0.8)

# C. train model (caret gives us access to even more options)
# see: https://topepo.github.io/caret/available-models.html

# svm - linear
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)

# svm - radial
svm_mod_radial <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trctrl)

svm_radial_pred <- predict(svm_mod_radial, newdata = test_x)
svm_radial_cmat <- confusionMatrix(svm_radial_pred, test_y)

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]], "\n",
  "SVM-Radial Accuracy:",  svm_radial_cmat$overall[["Accuracy"]]
)

# why may result differ from above?

