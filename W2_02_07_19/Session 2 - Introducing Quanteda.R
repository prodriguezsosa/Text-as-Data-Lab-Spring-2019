# TA: Leslie Huang
# Course: Text as Data
# Date: 02/01/2018
# Code originally from: Patrick Chester, Kevin Munger; updated and expanded by Leslie Huang

## 1 Setting up

# 1.1 Workspace

# Clear Global Environment
rm(list = ls())

# Set working directory
setwd(getwd())

# 1.2 Installing quanteda

# Install the latest stable version of quanteda from CRAN
install.packages("quanteda") # run this if you don't have quanteda already installed

library("quanteda")

# What version of quanteda do you have loaded? 
# How many threads (cores) are you using? See the printout in the console

# 1.3 Devtools and the quanteda corpus

# Install the package "devtools" which is used to install packages directly from Github
# install.packages("devtools")
library("devtools")

# Use devtools to install some sample data
devtools::install_github("quanteda/quanteda.corpora")

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


## 2 Running basic text analysis

# start with a short character vector
sampletxt <- "The police with their policing strategy instituted a policy of general 
iterations at the Data Science Institute."

# 2.1 Let's tokenize (break vector into individual words)
tokenized_text <- tokens(sampletxt)
?tokens

tokenized_text <- tokens(sampletxt, remove_punct = TRUE)

# Concept review: Which of these are the same?
# token
# type
# feature
# word
# term

# 2.2 Stemming examples
# SnowballC stemmer is based on the Porter stemmer 

stems <- tokens_wordstem(tokenized_text)
?tokens_wordstem

# 2.3 Loading State of the Union corpus

data("data_corpus_sotu", package = "quanteda.corpora")

# ndoc identifies the number of documents in a corpus

ndocs <- ndoc(data_corpus_sotu)

# Here, we grab the text of Obama's 2016 speech

obama_2016_sotu <- data_corpus_sotu[ndocs - 2]

# same as 

obama_2016_sotu <- texts(data_corpus_sotu)[ndocs - 2]

## 2.4 The DFM function creates a Document Feature Matrix from a document, corpus, etc
# in this case, from the 2016 speech

obama_dfm <- dfm(obama_2016_sotu, stem = TRUE)
?dfm

# What pre-processing options were used?

# if we wanted to stem the dfm of Obama's speech

dfm_wordstem(obama_dfm)

# Inspecting the components of a DFM object

str(obama_dfm) # You can see this in the RStudio "Environment" pane as well

obama_dfm[1,1:20]

# The topfeatures function by default shows the most frequently occuring terms in a given DFM

topfeatures(obama_dfm)

# Are all of these features relevant?
# Words?
# Punctuation

## 2.5 Stopwords

# Stopwords are commonly used words that add little understanding to the content of the document by themselves

# The stopwords function takes a language as an input and produces a vector of stopwords compiled from that language

stopwords("english")

# Fun fact: Quanteda also supports stopwords for english, SMART, danish, french, greek, hungarian, 
# norwegian, russian, swedish, catalan, dutch, finnish, german, italian, portuguese, spanish, and arabic

# Here we compare a DFM from the last SOTU while without English stopwords with one that has them

obama_dfm_no_preprocessing <- dfm(obama_2016_sotu, remove_punct = TRUE)
obama_dfm_pre_processed <- dfm(obama_2016_sotu, remove = stopwords("english"), remove_punct = TRUE)

topfeatures(obama_dfm_no_preprocessing)
topfeatures(obama_dfm_pre_processed)

## 3 Visualization and Weighted DFM

# Now we will create a DFM of all the SOTU speeches

full_dfm <- dfm(data_corpus_sotu, remove = stopwords("english"), remove_punct = TRUE)

topfeatures(full_dfm)

# 3.1 Visualizing the text contained within the DFM(s)

# Dominated by stopwords
textplot_wordcloud(obama_dfm_no_preprocessing, max.words = 200)

# Stopwords removed
textplot_wordcloud(obama_dfm_pre_processed, max.words = 200)

# Over all the SOTUs

textplot_wordcloud(full_dfm, max.words = 200)

# 3.2 tfidf - Frequency weighting

weighted_dfm <- dfm_tfidf(full_dfm) # Uses the absolute frequency of terms in each document

topfeatures(weighted_dfm)
?tfidf

# tfidf - Relative frequency weighting

normalized <- dfm_tfidf(full_dfm, scheme_tf = "prop") # Uses feature proportions within documents: divdes each term by the total count of features in the document

topfeatures(normalized)

# What do the different rankings imply?


## 4 Collocations

# bigrams

textstat_collocations(obama_2016_sotu)
?textstat_collocations

# trigrams

textstat_collocations(obama_2016_sotu, size = 3)

# Are there any other terms you all think are interesting?


## 5 Regular expressions

# regular expressions are a very powerful tool in wrangling text
# not a focus of this class, but something to be aware of

?regex

s_index <- grep(" s ", texts(data_corpus_sotu))

?grep

# this returns every speech that contains " s " -- JUST THE LETTER S BY ITSELF
texts_with_s <- grep(" s ", texts(data_corpus_sotu), value = TRUE)

# Here we create a vector of documents with " s " removed
texts_without_s <- gsub(" s ", "",  data_corpus_sotu[s_index])

# What's the difference between grep and gsub?

## 6 Preprocessing choices

# install.packages("preText")

library("preText")

# Run at home (takes a few minutes to run)
# Example below taken from preText vignette: https://cran.r-project.org/web/packages/preText/vignettes/getting_started_with_preText.html

preprocessed_documents <- factorial_preprocessing(
                          data_corpus_sotu,
                          use_ngrams = FALSE,
                          infrequent_term_threshold = 0.2,
                          verbose = FALSE)

preText_results <- preText(
                            preprocessed_documents,
                            dataset_name = "SOTU Speeches",
                            distance_method = "cosine",
                            num_comparisons = 20,
                            verbose = FALSE)

preText_score_plot(preText_results)

# Questions?
