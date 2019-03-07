# TA: Pedro L. Rodríguez
# Course: Text as Data
# Date: 03/07/2019
# Lab adapted from: Kevin Munger, Patrick Chester and Leslie Huang.

#----------------------------------------
# Set up environment                   ---
#----------------------------------------
# clear global environment
rm(list = ls())

setwd("~/Dropbox/NYU/Teaching/Text as Data/TaD-2018/W6_02_27_18/")

# load required libraries
library(quanteda)
library(quanteda.corpora)
library(readtext)
library(dplyr)

#----------------------------------------
# 1 Supervised Learning: Naive Bayes     ---
#----------------------------------------
#source of data: https://www.kaggle.com/rmisra/news-category-dataset#News_Category_Dataset_v2.json
#library(rjson)
#json_file <- "/Users/pedrorodriguez/Downloads/News_Category_Dataset_v2.json"
#con = file(json_file, "r") 
#input <- readLines(con, -1L) 
#news_data <- lapply(X=input,fromJSON)
#news_data <- lapply(news_data, function(x) as_tibble(t(unlist(x))))
#news_data <- do.call(rbind, news_data)
#saveRDS(news_data, "~/Dropbox/NYU/Teaching/Text as Data/TaD-2018/W6_02_27_18/news_data.rds")

# load data
news_data <- readRDS("news_data.rds")

# subset data and keep relevant variables
news_samp <- news_data %>% filter(category %in% c("CRIME", "SPORTS")) %>% select(headline, category) %>% setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "CRIME"])
head(news_samp$text[news_samp$class == "SPORTS"])

# some pre-processing (the rest will let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
head(news_samp$text[news_samp$class == "SPORTS"])

# what's the distribution of classes?
prop.table(table(news_samp$class))

# split sample into training & test sets
set.seed(1984L)
prop_train <- 0.8
ids <- 1:nrow(news_samp)
ids_train <- sample(ids, ceiling(prop_train*length(ids)), replace = FALSE)
ids_test <- ids[-ids_train]
train_set <- news_samp[ids_train,]
test_set <- news_samp[ids_test,]

# get dfm for each set
train_dfm <- dfm(train_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm(test_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))

# how does this look?
as.matrix(train_dfm)[1:5,1:5]

# match test set dfm to train set dfm features
test_dfm <- dfm_match(test_dfm, features = featnames(train_dfm))

# w/o smoothing ----------------

# train model on the training set
nb_model <- textmodel_nb(train_dfm, train_set$class, smooth = 0, prior = "uniform")

# evaluate on test set
predicted_class <- predict(nb_model, newdata = test_dfm)

# baseline
baseline_acc <- max(prop.table(table(test_set$class)))

# get confusion matrix
cmat <- table(test_set$class, predicted_class)
nb_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- cmat[2,2]/sum(cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)

# w smoothing ----------------

# train model on the training set using Laplace smoothing
nb_model_sm <- textmodel_nb(train_dfm, train_set$class, smooth = 1, prior = "uniform")

# evaluate on test set
predicted_class_sm <- predict(nb_model_sm, newdata = test_dfm)

# get confusion matrix
cmat_sm <- table(test_set$class, predicted_class_sm)
nb_acc_sm <- sum(diag(cmat_sm))/sum(cmat_sm) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall_sm <- cmat_sm[2,2]/sum(cmat_sm[2,]) # recall = TP / (TP + FN)
nb_precision_sm <- cmat_sm[2,2]/sum(cmat_sm[,2]) # precision = TP / (TP + FP)
nb_f1_sm <- 2*(nb_recall_sm*nb_precision_sm)/(nb_recall_sm + nb_precision_sm)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc_sm, "\n",
  "Recall:",  nb_recall_sm, "\n",
  "Precision:",  nb_precision_sm, "\n",
  "F1-score:", nb_f1_sm
)

# take a look at the most discriminant features (get some face validity)
posterior <- tibble(feature = rownames(t(nb_model_sm$PcGw)), 
                    post_CRIME = t(nb_model_sm$PcGw)[,1],
                    post_SPORTS = t(nb_model_sm$PcGw)[,2])

posterior %>% arrange(-post_SPORTS) %>% head(10)
posterior %>% arrange(-post_CRIME) %>% head(10)

# what does smoothing do? Reduces the "weight" place on new information (the likelihood) vis-a-vis the prior. 
plot(nb_model$PwGc[1,], nb_model_sm$PwGc[1,], xlim = c(0,0.02), ylim = c(0,0.02), xlab="No Smooth", ylab="Smooth") + abline(a = 0, b = 1, col = "red")

#----------------------------------------
# 2 Classification using Word Scores     ---
#----------------------------------------
# Read in conservative and labour manifestos
filenames <- list.files(path = "cons_labour_manifestos")

# Party name and year are in the filename -- we can use regex to extract these to use as our docvars
party <- unlist(regmatches(unlist(filenames), gregexpr("^[[:alpha:]]{3}", unlist(filenames))))
year <- unlist(regmatches(unlist(filenames), gregexpr("[[:digit:]]+", unlist(filenames))))

# This is how you would make a corpus with docvars from this data
cons_labour_manifestos <- corpus(readtext("cons_labour_manifestos/*.txt"))
docvars(cons_labour_manifestos, field = c("party", "year") ) <- data.frame(cbind(party, year))

# But we're going to use a dataframe
cons_labour_df <- tibble(text = texts(cons_labour_manifestos),
                         party = party,
                         year = as.integer(year))
colnames(cons_labour_df)

# keep vars of interest
cons_labour_df <- cons_labour_df %>% select(text, party) %>% setNames(c("text", "class"))

# what's the class distribution?
prop.table(table(cons_labour_df$class))

# randomly sample a test speech
set.seed(1984L)
ids <- 1:nrow(cons_labour_df)
ids_test <- sample(ids, 1, replace = FALSE)
ids_train <- ids[-ids_test]
train_set <- cons_labour_df[ids_train,]
test_set <- cons_labour_df[ids_test,]

# create DFMs
train_dfm <- dfm(train_set$text, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm(test_set$text, remove_punct = TRUE, remove = stopwords("english"))

# Word Score model w/o smoothing ----------------
ws_base <- textmodel_wordscores(train_dfm, 
                                y = (2 * as.numeric(train_set$class == "Lab")) - 1 # Y variable must be coded on a binary x in {-1,1} scale, so -1 = Conservative and 1 = Labour
)

# Look at strongest features
lab_features <- sort(ws_base$wordscores, decreasing = TRUE)  # for labor
lab_features[1:10]

con_features <- sort(ws_base$wordscores, decreasing = FALSE)  # for conservative
con_features[1:10]

# Can also check the score for specific features
ws_base$wordscores[c("drugs", "minorities", "unemployment")]

# predict that last speech
test_set$class
predict(ws_base, newdata = test_dfm,
        rescaling = "none", level = 0.95) 

# Word Score model w smoothing ----------------
?textmodel_wordscores
ws_sm <- textmodel_wordscores(train_dfm, 
                              y = (2 * as.numeric(train_set$class == "Lab")) - 1, # Y variable must be coded on a binary x in {-1,1} scale, so -1 = Conservative and 1 = Labour
                              smooth = 1
)

# Look at strongest features
lab_features_sm <- sort(ws_sm$wordscores, decreasing = TRUE)  # for labor
lab_features_sm[1:10]

con_features_sm <- sort(ws_sm$wordscores, decreasing = FALSE)  # for conservative
con_features_sm[1:10]

# predict that last speech
test_set$class
predict(ws_base, newdata = test_dfm,
        rescaling = "none", level = 0.95) 

# Smoothing  
plot(ws_base$wordscores, ws_sm$wordscores, xlim=c(-1, 1), ylim=c(-1, 1),
     xlab="No Smooth", ylab="Smooth")

#----------------------------------------
# 3 Applying Naive Bayes and Word Scores to Amicus texts from Evans et al ---
#----------------------------------------
# Loading data
data("data_corpus_amicus")

# create dfm
amicus_dfm <- dfm(data_corpus_amicus)

# naive bayes model ----

# train NB model
amNBmodel <- textmodel_nb(amicus_dfm, docvars(data_corpus_amicus, "trainclass")) 

# predict class label of test set
amNBpredict <- predict(amNBmodel)

# "confusion matrix": Naive Bayes
nb_cmat <- table(docvars(data_corpus_amicus, "testclass"), amNBpredict)

# baseline accuracy
baseline_acc <- max(prop.table(table(docvars(data_corpus_amicus, "testclass"))))
baseline_acc

# get scores
nb_acc <- sum(diag(nb_cmat))/sum(nb_cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- nb_cmat[2,2]/sum(nb_cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- nb_cmat[2,2]/sum(nb_cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)

# wordscore model ----

# create reference texts
reference <- c(1, 1, -1, -1, rep(NA, 98)) # class labels

# train ws model
amWSmodel <- textmodel_wordscores(amicus_dfm, reference, smooth = 1)

# plot nb and ws scores
plot(amWSmodel$wordscores, c(1, -1) %*% amNBmodel$PcGw, xlab="Wordscore", ylab = "Linear Posterior Class Pr. Diff")

# let's look at predictions from our wordscores model
amWSpredict <- predict(amWSmodel)
amWSresults <- ifelse(amWSpredict > 0, "P", "R")

# "confusion matrix": wordscores
ws_cmat <- table(docvars(data_corpus_amicus, "testclass"), amWSresults)

# get scores
ws_acc <- sum(diag(ws_cmat))/sum(ws_cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
ws_recall <- ws_cmat[2,2]/sum(ws_cmat[2,]) # recall = TP / (TP + FN)
ws_precision <- ws_cmat[2,2]/sum(ws_cmat[,2]) # precision = TP / (TP + FP)
ws_f1 <- 2*(ws_recall*ws_precision)/(ws_recall + ws_precision)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  ws_acc, "\n",
  "Recall:",  ws_recall, "\n",
  "Precision:",  ws_precision, "\n",
  "F1-score:", ws_f1
)
