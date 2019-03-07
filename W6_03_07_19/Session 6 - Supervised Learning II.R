# TA: Leslie Huang
# Course: Text as Data
# Date: 2/27/2018, part 2
# Supervised Learning II
# Credit: materials adapted from Patrick Chester, with some examples taken from Ken Benoit's NYU Dept. of Politics short course Fall 2014

## 1 Setting up 

# Clear Global Environment
rm(list = ls())

setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/W6_02_27_18")

# Libraries
library(quanteda)
library(quanteda.corpora)
library(readtext)

## 1 Supervised Learning: Naive Bayes

# Example: Replication of 13.1 from IIR textbook

trainingset <- matrix(0,ncol=6,nrow=5)
trainingset[1,] <- c(1, 2, 0, 0, 0, 0)
trainingset[2,] <- c(0, 2, 0, 0, 1, 0)
trainingset[3,] <- c(0, 1, 0, 1, 0, 0)
trainingset[4,] <- c(0, 1, 1, 0, 0, 1)
trainingset[5,] <- c(0, 3, 1, 0, 0, 1)
colnames(trainingset) <- c("Beijing", "Chinese",  "Japan", "Macao", "Shanghai", "Tokyo")
rownames(trainingset) <- paste("d", 1:5, sep="")

trainingset <- as.dfm(trainingset) # training data

trainingclass <- factor(c("Y", "Y", "Y", "N", NA), ordered = TRUE) # training/test classes -- last document is unknown


# replicate IIR p261 prediction for test set (document 5)
nb.p261 <- textmodel_nb(x = trainingset, y = trainingclass,
                        smooth = 1, prior="docfreq") # Smooth gives values of 1 for new words; NB wouldn't work very well


pr.p261 <- predict(nb.p261)
pr.p261


# 2 Classification using Word Scores

# Read in conservative and labour manifestos
filenames <- list.files(path = "cons_labour_manifestos")

# Party name and year are in the filename -- we can use regex to extract these to use as our docvars
party <- unlist(regmatches(unlist(filenames), gregexpr("^[[:alpha:]]{3}", unlist(filenames))))
year <- unlist(regmatches(unlist(filenames), gregexpr("[[:digit:]]+", unlist(filenames))))

# This is how you would make a corpus with docvars from this data
cons_labour_manifestos <- corpus(readtext("cons_labour_manifestos/*.txt"))
docvars(cons_labour_manifestos, field = c("party", "year") ) <- data.frame(cbind(party, year))

# We're going to use a dataframe
cons_labour_df <- data.frame(text = texts(cons_labour_manifestos),
                             party = party,
                             year = year, 
                             stringsAsFactors = FALSE)

# Identifying test speech: Labor
test_speech <- cons_labour_df[46,]

# Setting training speeches: The remaining 45 Labor and Conservative speeches

training_df <- cons_labour_df[1:45, ]

# Create DFMs
training_dfm <- dfm(corpus(training_df$text, docvars = training_df[, c("party", "year")]))

test_dfm <- dfm(corpus(test_speech$text, docvars = test_speech[, c("party", "year")]))

# Train Word Score model

ws_base <- textmodel_wordscores(training_dfm, 
                   y = (2 * as.numeric(training_df$party == "Lab")) - 1 # Y variable must be coded on a binary x in {-1,1} scale, so -1 = Conservative and 1 = Labour
                   )

# Look at strongest features

lab_features <- sort(ws_base$wordscores, decreasing = TRUE)

lab_features[1:10]

con_features <- sort(ws_base$wordscores, decreasing = FALSE)

con_features[1:10]

ws_base$wordscores[c("drugs", "minorities", "unemployment")]

# Trying it again with smoothing
ws_smooth <- textmodel_wordscores(training_dfm, 
                                y = (2 * as.numeric(training_df$party == "Lab")) - 1,
                                smooth = 1
                                )

ws_smooth$wordscores[c("drugs", "minorities", "unemployment")]
# Smoothing is giving stronger priors, decreasing the impact of new information

plot(ws_base$wordscores, ws_smooth$wordscores, xlim=c(-1, 1),
     xlab="No Smooth", ylab="Smooth")

## predict that last speech
predict(ws_base, newdata = test_dfm,
        rescaling = "none", level = 0.95) 

predict(ws_smooth, newdata = test_dfm,
        rescaling = "none", level = 0.95) # It makes the wrong prediction!

## 3 Applying Naive Bayes and Word Scores to Amicus texts from Evans et al

# Loading data
data("data_corpus_amicus")

amicus_dfm <- dfm(data_corpus_amicus)

amNBmodel <- textmodel_nb(amicus_dfm, docvars(data_corpus_amicus, "trainclass")) 

amNBpredict <- predict(amNBmodel)

# "confusion matrix": Naive Bayes
tab_NB <- table(docvars(data_corpus_amicus, "testclass"), amNBpredict$nb.predicted)

tab_NB

# Accuracy: Naive Bayes
(tab_NB[1,1]+tab_NB[2,2])/sum(tab_NB)

reference <- c(1, 1, -1, -1, rep(NA, 98)) # class labels

amWSmodel <- textmodel_wordscores(amicus_dfm, reference, smooth = 1)

plot(amWSmodel$wordscores, c(1, -1) %*% amNBmodel$PcGw, xlab="Wordscore", ylab="Linear Posterior Class Pr. Diff")

(amWSpredict <- predict(amWSmodel))

amWSresults <- ifelse(amWSpredict > 0, "P", "R")

# "confusion matrix": WordScores
(tab_WS <- table(docvars(data_corpus_amicus, "testclass"), amWSresults) )

# Accuracy: WordScores
(tab_WS[1,1]+tab_WS[2,2])/sum(tab_WS)
