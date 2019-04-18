# TA: Leslie Huang
# Course: Text as Data
# Date: 4/12/18
# Recitation 11: Unsupervised Learning IIa

rm(list = ls())

setwd("./Text-as-Data-Lab-Spr2018/W11_04_12_18/")

set.seed(1234)

# Check for these packages, install them if you don't have them
# install.packages("tidytext")
# install.packages("topicmodels")
# install.packages("ldatuning")
# install.packages("stringi")
# install.packages("rjson")

libraries <- c("ldatuning", "topicmodels", "ggplot2", "dplyr", "rjson", "quanteda", "lubridate", "parallel", "doParallel", "tidytext", "stringi")
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

# Create DFM
blm_dfm <-dfm(blm_tweets_sum$text, stem = F, remove_punct = T, tolower = T, remove_twitter = T, remove_numbers = TRUE, remove = c(stopwords("english"), "http","https","rt", "t.co"))

## 2 Selecting K

# Identify an appropriate number of topics (FYI, this function takes a while)
k_optimize_blm <- FindTopicsNumber(
  blm_dfm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2017),
  mc.cores = 6L,
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
blm_tm <- LDA(blm_dfm, k = k, method = "Gibbs",  control = list(seed = 1234))

# Other parameters that we do not use here (because they increase the time the model takes) can be passed to the control parameter
?`LDAcontrol-class`
# iter : num iterations
# thin : every thin iteration is returned for iter iterations
# burnin : number of initial iterations discarded

## Letter soup

# gamma = posterior topic distribution over documents
blm_tm@gamma
# Docs x topic_proportions array
rowSums(blm_tm@gamma) # Each row sums to 1

# beta = log word distributions over topics
blm_tm@beta
# Topics x log word proportion

# alpha = scaling parameter for symmetric Dirichlet distribution over topic distributions
# in this implementation, alpha is estimated as 50/k but can be set in parameters of LDA()
blm_tm@alpha 

# z = topic assignments of specific words
blm_tm@z

# Quickly extracts the word weights and transforms them into a data frame
blm_topics <- tidy(blm_tm, matrix = "beta") 

# Side note: You can pass objects between tidytext() and topicmodels() functions because tidytext() implements topic models from topicmodels()

# Generates a df of top terms
blm_top_terms <- blm_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Creates a plot of the weights and terms by topic
blm_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


## 4 Visualizing topic trends over time

# Store the results of the distribution of topics over documents
doc_topics <- blm_tm@gamma

# Store the results of words over topics
words_topics <- blm_tm@beta

# Transpose the data so that the days are columns
doc_topics <- t(doc_topics)

# Arrange topics
# Find the top topic per column (day)
max<-apply(doc_topics, 2, which.max)

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

