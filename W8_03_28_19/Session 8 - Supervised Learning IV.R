# TA: Pedro L. Rodríguez
# Course: Text as Data
# Date: 03/28/2019

# discuss HW2 typos

#----------------------------------------
# go over concepts                       ---
#----------------------------------------
# Ensemble methods 
# Ensemble = a group of predictors
# intuition: wisdom of the crowds (best if predictions are independent)
# 1. Bagging: use different random subsets of the training set w. replacement
# 2. Pasting: use different random subsets of the training set w/o replacement
# 3. Boosting: train predictors sequentially, each trying to correct its predecessor
# 3a. AdaBoost: increase weight of instances that predecessor underfitted/misclassified
# 3b. Gradient Boosting: fit new predictor to the residual erors made by the previous predictor
# 4. Stacking: train a model to aggregate predictions of all predictors in an ensemble (can have several layers)
# Random Forest is an ensemble of decision trees (generally use bagging)
# Random Forest introduces additional source of randomness when growing trees: 
# non RF-decision trees search for the best feature among ALL features when splitting a node
# decision-trees in a RF algorithm search for the best feature among a random subset of features

#----------------------------------------
# Set up environment                     ---
#----------------------------------------
# clear global environment
rm(list = ls())

# load required libraries
library(dplyr)
library(randomForest)
library(mlbench)
library(caret)

# set working directory
setwd("~/Drobox/GitHub/Text-as-Data-Lab-Spring-2019/W6_03_07_19/")

#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------
news_data <- readRDS("news_data.rds")
table(news_data$category)

# let's work with 2 categories
set.seed(1984)
news_samp <- news_data %>% 
  filter(category %in% c("MONEY", "LATINO VOICES")) %>% 
  group_by(category) %>%
  sample_n(500) %>%  # sample 250 of each to reduce computation time (for lab purposes)
  ungroup() %>%
  select(headline, category) %>% 
  setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "MONEY"])
head(news_samp$text[news_samp$class == "LATINO VOICES"])

# some pre-processing (the rest we'll let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
news_samp$class <- recode(news_samp$class,  "MONEY" = "money", "LATINO VOICES" = "latino")

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
set.seed(1984)
news_samp <- news_samp %>% sample_n(nrow(news_samp))
rownames(news_samp) <- NULL

#----------------------------------------
# 2. Prepare Data                        ---
#----------------------------------------
library(caret)
library(quanteda)

# create document feature matrix
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% convert("matrix")

# keep tokens that appear in at least 5 headlines
presen_absent <- news_dfm 
presen_absent[presen_absent > 0] <- 1
feature_count <- apply(presen_absent, 2, sum)
features <- names(which(feature_count > 5))
news_dfm <- news_dfm[,features]

# caret package has it's own partitioning function
set.seed(1984)
ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

#----------------------------------------
# 3. Using RandomForest                  ---
#----------------------------------------
library(randomForest)
mtry = sqrt(ncol(train_x))  # number of features to sample at each split
ntree = 51  # numbre of trees to grow
# more trees generally improve accuracy but at the cost of computation time
# odd numbers avoid ties (recall default aggregation is "majority voting")
set.seed(1984)
system.time(rf.base <- randomForest(x = train_x, y = train_y, ntree = ntree, mtry = mtry))
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)])

# print results
print(rf.base)

# plot importance
# gini impurity = how "pure" is given node ~ class distribution
# = 0 if all instances the node applies to are of the same class
# upper bound depends on number of instances
varImpPlot(rf.base, n.var = 10, main = "Variable Importance")

#----------------------------------------
# 4. RandomForest Using Caret            ---
#----------------------------------------
trainControl <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
mtry <- sqrt(ncol(train_x))
ntree = 51  
tunegrid <- expand.grid(.mtry = mtry)
set.seed(1984)
system.time(rf.caret <- train(x = train_x, y = train_y, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = trainControl,
                              ntree = ntree))

# print results
print(rf.caret)

# plot importance
varImpPlot(rf.caret$finalModel, n.var = 10, main = "Variable Importance")

#----------------------------------------
# 5. RandomForest Using Caret + tuning   ---
#----------------------------------------
# note: the package RandomForest also has its own tuning function: tuneRF
trainControl <- trainControl(method = "cv", number = 5)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry = c(0.5*mtry, mtry, 1.5*mtry))  # at the moment caret only allows tuning of mtry (partly b/c ntree is just a matter of computational constratints)
set.seed(1984)
system.time(rf.grid <- train(x = train_x, y = train_y, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = ntree))
# print grid search results
print(rf.grid)

# plot grid search results
plot(rf.grid)

#----------------------------------------
# 6. RandomForest Using Caret + manual tuning ---
#----------------------------------------
mtry <- sqrt(ncol(train_x))
tunegrid <- expand.grid(.mtry = mtry)
# ntree = 1
set.seed(1984)
system.time(rf.man1 <- train(x = train_x, y = train_y, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = trainControl, ntree = 1))

# ntree = 5
set.seed(1984)
system.time(rf.man2 <- train(x = train_x, y = train_y, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = trainControl, ntree = 5))

# ntree = 101
set.seed(1984)
system.time(rf.man3 <- train(x = train_x, y = train_y, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = trainControl, ntree = 51))

# collect results & summarize
results <- resamples(list(rf1 = rf.man1, rf5 = rf.man2, rf51 = rf.man3))
summary(results)

# test set accuracy
confusionMatrix(predict(rf.man1, newdata = test_x), test_y)
confusionMatrix(predict(rf.man2, newdata = test_x), test_y)
confusionMatrix(predict(rf.man3, newdata = test_x), test_y)

# box and whisker plots to compare models
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
bwplot(results, scales = scales)