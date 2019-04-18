# TA: Pedro L. Rodríguez
# Course: Text as Data
# Date: 04/18/2019
# Lab adapted from: Leslie Huang.
# additional resources: 
# original paper introducing LDA: http://www.jmlr.org/papers/volume3/blei03a/blei03a.pdf
# https://www.tidytextmining.com/topicmodeling.html
# https://medium.com/nanonets/topic-modeling-with-lsa-psla-lda-and-lda2vec-555ff65b0b05
# most recent addition to topic modeling methods: https://multithreaded.stitchfix.com/blog/2016/05/27/lda2vec/#topic=38&lambda=1&term= 
# human validation of topic models: https://dl.acm.org/citation.cfm?id=2984126

# basic intuition:
# a. documents are represented as random mixtures over latent topics.
# b. a topic is characterized by a distribution over words.
# we now propose a GENERATIVE MODEL OF THE DATA
# want to maximize the probability of a corpus as a function of our parameters (of the dirichlets) and latent variables (doc topic mixtures and topic word distributions).

rm(list = ls())

setwd("~/Drobox/GitHub/Text-as-Data-Lab-Spring-2019/W10_04_18_19/")

set.seed(1234)

# Check for these packages, install them if you don't have them
# install.packages("tidytext")
# install.packages("topicmodels")
# install.packages("ldatuning")
# install.packages("stringi")
# install.packages("rjson")

libraries <- c("ldatuning", "topicmodels", "ggplot2", "dplyr", "rjson", "quanteda", "lubridate", "parallel", "doParallel", "tidytext", "stringi", "tidyr")
lapply(libraries, require, character.only = TRUE)

## 1 Preprocessing

# Load data
blm_tweets <- read.csv("blm_samp.csv", stringsAsFactors = F)

# Create date vectors
blm_tweets$datetime <- as.POSIXct(strptime(blm_tweets$created_at, "%a %b %d %T %z %Y",tz = "GMT")) # full date/timestamp
blm_tweets$date <- mdy(paste(month(blm_tweets$datetime), day(blm_tweets$datetime), year(blm_tweets$datetime), sep = "-")) # date only

# Collapse tweets so we are looking at the total tweets at the day level
blm_tweets_sum <- blm_tweets %>% group_by(date) %>% summarise(text = paste(text, collapse = " "))

# Remove non ASCII characters
blm_tweets_sum$text <- stringi::stri_trans_general(blm_tweets_sum$text, "latin-ascii")

# Removes solitary letters
blm_tweets_sum$text <- gsub(" [A-z] ", " ", blm_tweets_sum$text)

# As always we begin with a DFM.
# Create DFM
blm_dfm <-dfm(blm_tweets_sum$text, stem = F, remove_punct = T, tolower = T, remove_twitter = T, remove_numbers = TRUE, remove = c(stopwords("english"), "http","https","rt", "t.co"))

# Topic models
# note; pLSA (a probabilistic take on LSA) is a precursor to LDA
## 2 Selecting K

# Identify an appropriate number of topics (FYI, this function takes a while)
k_optimize_blm <- FindTopicsNumber(
  blm_dfm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2017),
  mc.cores = detectCores(), # to usa all cores available
  verbose = TRUE
)

FindTopicsNumber_plot(k_optimize_blm)

# Where do these metrics come from? 

# Go here for the citations (and another tutorial)
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html

# What should you consider when choosing the number of topics you use in a topic model?

# What does robustness mean here?

## 3 Visualizing Word weights

# Set number of topics
k <- 19

# Fit the topic model with the chosen k
system.time(
  blm_tm <- LDA(blm_dfm, k = k, method = "Gibbs",  control = list(seed = 1234)))

# Other parameters that we do not use here (because they increase the time the model takes) can be passed to the control parameter
?`LDAcontrol-class`
# iter : num iterations
# thin : every thin iteration is returned for iter iterations
# burnin : number of initial iterations discarded

## Letter soup

# gamma = posterior document distribution over topics
# what are the dimensions of gamma?
dim(blm_tm@gamma)
blm_tm@gamma[1:5,1:5]
rowSums(blm_tm@gamma) # each row sums to?

# beta = topic distribution over words
dim(blm_dfm)  # how many features do we have?
dim(blm_tm@beta)
blm_tm@beta[1:5,1:5]
sum(blm_tm@beta[1,]) # each row sums to?
sum(exp(blm_tm@beta[10,])) # each row sums to?

# Per topic per word proabilities matrix (beta)
blm_topics <- tidy(blm_tm, matrix = "beta") 
head(blm_topics)

# Side note: You can pass objects between tidytext() and topicmodels() functions because tidytext() implements topic models from topicmodels()

# Generates a df of top terms
blm_top_terms <- blm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

head(blm_top_terms)

# Creates a plot of the weights and terms by topic
blm_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Creates a plot of features with greatest difference in word probabilities between two topics
blm_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  filter(topic %in% c("topic1", "topic2")) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1)) %>%
  arrange(-abs(log_ratio)) %>%
  slice(c(1:10,(nrow(.)-9):nrow(.))) %>%
  arrange(-log_ratio) %>%
  mutate(term = factor(term, levels = unique(term))) %>%
  ggplot(aes(as.factor(term), log_ratio)) +
  geom_col(show.legend = FALSE) +
  xlab("Terms") + ylab("Log-Ratio") +
  coord_flip()

## 4 Visualizing topic trends over time

# Store the results of the mixture of documents over topics 
doc_topics <- blm_tm@gamma

# Store the results of words over topics
#words_topics <- blm_tm@beta

# Transpose the data so that the days are columns
doc_topics <- t(doc_topics)
dim(doc_topics)
doc_topics[1:5,1:5]

# Arrange topics
# Find the top topic per column (day)
max <- apply(doc_topics, 2, which.max)

# Write a function that finds the second max
which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max2 <- apply(doc_topics, 2, which.max2)
max2 <- sapply(max2, max)

# Coding police shooting events
victim <- c("Freddie Gray", "Sandra Bland")
shootings <- mdy(c("04/12/2015","7/13/2015"))

# Combine data
top2 <- data.frame(top_topic = max, second_topic = max2, date = ymd(blm_tweets_sum$date))

# Plot
blm_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First")) 

blm_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) +theme_bw() + 
  ylab("Topic Number") + ggtitle("BLM-Related Tweets from 2014 to 2016 over Topics") + geom_point() + xlab(NULL) + 
  geom_vline(xintercept=as.numeric(shootings[1]), color = "blue", linetype=4) + # Freddie Gray (Topic)
  geom_vline(xintercept=as.numeric(shootings[2]), color = "black", linetype=4)  + # Sandra Bland
  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 

#----------------------------------
# Extra: LSA as topic modeling
#----------------------------------
# LSA decomponses a DFM into the product of three matrices: 
# a. truncated term matrix from term vector matrix T (left singular vectors from the SVD of the original matrix) -> can be thought of as a topic-term-matrix
# b. truncated document matrix from document vector matrix D (right singular vectors from the SVD of the original matrix) -> can be thought of as a document-topic-matrix
# c. singular values: Matrix of scaling values to ensure that multiplying these matrices reconstructs TDM

# set working directory
setwd("~/Drobox/GitHub/Text-as-Data-Lab-Spring-2019/W6_03_07_19/")
library(lsa)
#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------
news_data <- readRDS("news_data.rds")
table(news_data$category)

# let's work with 2 categories
set.seed(1984)
news_samp <- news_data %>% 
  filter(category %in% c("POLITICS")) %>% 
  group_by(category) %>%
  sample_n(1000) %>%  # sample 250 of each to reduce computation time (for lab purposes)
  ungroup() %>%
  select(headline, category) %>% 
  setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "POLITICS"])

# some pre-processing (the rest we'll let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes

#----------------------------------------
# 2. Prepare Data                        ---
#----------------------------------------

# create document feature matrix
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
news_fdm <- convert(news_dfm, to = "lsa")

#----------------------------------------
# 3. Estimate LSA                       ---
#----------------------------------------
news_lsa <- lsa(news_fdm, dims = 20)

# document-topic matrix
dim(news_lsa$dk)
news_lsa$dk[1:5,1:5]

# topic-term matrix
dim(news_fdm)
dim(news_lsa$tk)
news_lsa$tk[1:5,1:5]

# Per topic per word weights
library(reshape2)
lsa_topics <- news_lsa$tk %>% reshape2:::melt.matrix(.) %>% setNames(c("term", "topic", "weight"))
head(lsa_topics)

# Generates a df of top terms
lsa_top_terms <- lsa_topics %>%
  group_by(topic) %>%
  top_n(10, weight) %>%
  ungroup() %>%
  arrange(topic, -weight)

# Creates a plot of the weights and terms by topic
lsa_top_terms %>%
  mutate(term = reorder(term, weight)) %>%
  ggplot(aes(term, weight, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()




