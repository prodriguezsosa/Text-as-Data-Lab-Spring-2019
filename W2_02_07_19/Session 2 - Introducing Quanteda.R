# TA: Pedro L. Rodriguez
# Course: Text as Data
# Date: 2/07/2019
# Lab adapted from: Kevin Munger, Patrick Chester and Leslie Huang.

# other "similar" interesting packages: tm, tidytext

#-----------------------------
# 1 SETTING UP
#-----------------------------

# 1.1 Workspace -----------------------

# Clear Global Environment
rm(list = ls())

# Set working directory
setwd("/Users/pedrorodriguez/Drobox/GitHub/Text-as-Data-Lab-Spring-2019/W2_02_07_19/")

# 1.2 Installing quanteda

# Install the latest stable version of quanteda from CRAN
#install.packages("quanteda") # run this if you don't have quanteda already installed

library(quanteda)
library(ggplot2)
library(dplyr)

# What version of quanteda do you have loaded? 
# How many threads (cores) are you using? See the printout in the console

# 1.3 Devtools and the quanteda corpus

# Install the package "devtools" which is used to install packages directly from Github
# install.packages("devtools")
#library("devtools")

# Use devtools to install some sample data
#devtools::install_github("quanteda/quanteda.corpora")

# Load it into our environment
library(quanteda.corpora)

# Read about the data available: https://github.com/quanteda/quanteda.corpora

### Note: Quanteda is still under development so it is changing! New features are being added but sometimes functions or function parameters are deprecated or renamed. This includes the very basic functions in this code demo!

# This means that you may encounter many code examples, StackOverflow questions, and websites with outdated documentation, etc. that include functions or options that have been deprecated or renamed.

# 1.4 Managing dependencies

# If you want to ensure that your code for a project will not break when you update quanteda, I recommend using a dependency manager for R called packrat so that you can specify a dependency on a specific version of quanteda.
# Find out about setting up packrat here: https://rstudio.github.io/packrat/walkthrough.html

# 1.5 Versions of quanteda

# How would you get an older version of quanteda? (For example, if you accidentally installed the dev version from GitHub but you want to go back to the last stable release, or you want a legacy version to support old code.)

# - Check the CRAN archive
# use the install_version function, e.g.:
# devtools::install_version("quanteda", version = "0.99.12", repos = "http://cran.us.r-project.org")

# If you want the latest dev version of quanteda, it's on GitHub, but we will use the latest version from CRAN for stability/sanity reasons
# devtools::install_github("quanteda/quanteda") 

# Concept review: Which of these are the same?
# token
# type
# feature
# word
# term

#-----------------------------
# 1 THE CORPUS OBJECT
#-----------------------------

# quanteda's main input object is called a "corpus" (a way of organizing text data: generally includes text + metadata)

# this is NOT the only way, see for example: https://www.tidytextmining.com/tidytext.html

# 1.1 load the State of the Union (SOTU) corpus and look at a summary ---------------------
data("data_corpus_sotu", package = "quanteda.corpora")

# a corpus consists of: (1) documents: text + doc level data (2) corpus metadata (3) settings
head(docvars(data_corpus_sotu))  # document-level variables
metacorpus(data_corpus_inaugural)  # corpus-level variables

# ndoc identifies the number of documents in a corpus
ndocs <- ndoc(data_corpus_sotu)

# summary of the corpus (provides some summary statistics on the text combined with the metadata)
corpusinfo <- summary(data_corpus_sotu, n = ndocs)  # note n default is 100
View(corpusinfo)

# quick visualization
token_plot <- ggplot(data = corpusinfo, aes(x = Date, y = Tokens, group = 1)) + geom_line() + geom_point() + theme_bw()
token_plot

# 1.2 subset corpus ---------------------
trump_sotu_corpus <- corpus_subset(data_corpus_sotu, President == "Trump")
summary(corpus_subset(data_corpus_sotu, President == "Trump"))

# key words in context (KWIC)
kwic_america <- kwic(trump_sotu_corpus, pattern = "america", valuetype = "regex")

# keep only the text of the most recent SOTU
trump_2018_text <- texts(trump_sotu_corpus)[2]

# same as
trump_2018_text <- trump_sotu_corpus[2]

#-----------------------------
# 2 TOKENIZING & STEMMING
#-----------------------------

## 2.1 Tokenizing text ---------------------
?tokens
tokenized_speech <- tokens(trump_2018_text)
head(unname(unlist(tokenized_speech)), 10)

# alternative using only base R
tokenized_speech <- strsplit(trump_2018_text, " ")

# remove punctuation when tokenizing
tokenized_speech <- tokens(trump_2018_text, remove_punct = TRUE)
head(unname(unlist(tokenized_speech)), 10)

## 2.2 Stemming ---------------------
# SnowballC stemmer is based on the Porter stemmer (varies by language, english is default)
?tokens_wordstem
stemmed_speech <- tokens_wordstem(tokenized_speech)
head(unname(unlist(stemmed_speech)), 10)

#-----------------------------
# 3 DOCUMENT FEATURE MATRIX
#-----------------------------

## 3.1 Creating a DFM ---------------------
# input can be a document, corpus, etc
trump_2018_dfm <- dfm(trump_2018_text, stem = TRUE)
?dfm  # see all options
# can also apply stemmer afterwards using dfm_wordstem() on a dfm object

# inspect the first few features (how many rows does this dfm have?)
trump_2018_dfm[, 1:10]
dim(trump_2018_dfm)

# top features in dfm
topfeatures(trump_2018_dfm)

# Are all of these features relevant?
# Words?
# Punctuation

## 3.2 Stopwords ---------------------
# Stopwords are commonly words that (presumably) add little understanding to the content of the document by themselves
# The stopwords function takes a language as an input and produces a vector of stopwords compiled from that language

stopwords("english")

# Fun fact: Quanteda also supports stopwords for english, SMART, danish, french, greek, hungarian, 
# norwegian, russian, swedish, catalan, dutch, finnish, german, italian, portuguese, spanish, and arabic

trump_2018_dfm_1 <- dfm(trump_2018_text, remove_punct = TRUE)
trump_2018_dfm_2 <- dfm(trump_2018_text, remove = stopwords("english"), remove_punct = TRUE)

topfeatures(trump_2018_dfm_1)
topfeatures(trump_2018_dfm_2)

#-----------------------------
# 4 WEIGHTED DOCUMENT FEATURE MATRIX
#-----------------------------
# Now we will create a DFM of all the SOTU speeches
full_dfm <- dfm(data_corpus_sotu, remove = stopwords("english"), remove_punct = TRUE)

topfeatures(full_dfm)

## 4.1 Visualizing the text contained within the DFM(s) --------

# Dominated by stopwords
textplot_wordcloud(trump_2018_dfm_1, max_words = 200)

# Stopwords removed
textplot_wordcloud(trump_2018_dfm_2, max_words = 200)

# Over all the SOTUs
textplot_wordcloud(full_dfm, max_words = 200)

# 4.2 tfidf - Frequency weighting

weighted_dfm <- dfm_tfidf(full_dfm) # Uses the absolute frequency of terms in each document

topfeatures(weighted_dfm)
?tfidf

# tfidf - Relative frequency weighting
normalized <- dfm_tfidf(full_dfm, scheme_tf = "prop") # Uses feature proportions within documents: divdes each term by the total count of features in the document
topfeatures(normalized)

# What do the different rankings imply?

#-----------------------------
# 5 COLLOCATIONS
#-----------------------------
# bigrams

head(textstat_collocations(trump_2018_sotu))
?textstat_collocations

# trigrams
head(textstat_collocations(trump_2018_sotu, size = 3))

# Are there any other terms you all think are interesting?

#-----------------------------
# 6 REGULAR EXPRESSIONS
#-----------------------------
# regular expressions are a very powerful tool in wrangling text
# not a focus of this class, but something to be aware of
# cheatsheet for regex: https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
?regex

s_index <- grep(" s ", texts(data_corpus_sotu))

?grep

# this returns every speech that contains " s " -- JUST THE LETTER S BY ITSELF
texts_with_s <- grep(" s ", texts(data_corpus_sotu), value = TRUE)

# Here we create a vector of documents with " s " removed
texts_without_s <- gsub(" s ", "",  data_corpus_sotu[s_index])

# What's the difference between grep and gsub?

#-----------------------------
# 7 PRE-PROCESSING CHOICES
#-----------------------------
# install.packages("preText")

library("preText")

# Run at home (takes a few minutes to run)
# Example below taken from preText vignette: https://cran.r-project.org/web/packages/preText/vignettes/getting_started_with_preText.html

preprocessed_documents <- factorial_preprocessing(
  data_corpus_sotu,
  use_ngrams = FALSE,
  infrequent_term_threshold = 0.2,
  verbose = FALSE)

preText_results <- preText(preprocessed_documents,
                           dataset_name = "SOTU Speeches",
                           distance_method = "cosine",
                           num_comparisons = 20,
                           verbose = FALSE)

preText_score_plot(preText_results)

# Questions?

# I recommend you check out: https://quanteda.io/articles/quickstart.html

