# TA: Pedro L. Rodriguez
# Course: Text as Data
# Date: 4/25/2019

# Supervised vs. Unsupervised
# topic-models: excellent for exploration
# supervised exploration of topics: classification on text snippets with keywords (e.g. Venezuela project)
# semi-supervised approaches: https://github.com/gregversteeg/corex_topic
# see: https://medium.com/pew-research-center-decoded/overcoming-the-limitations-of-topic-models-with-a-semi-supervised-approach-b947374e0455

# -----------------------------------------------
# Structural Topic Models                       ---
# -----------------------------------------------
rm(list = ls())
libraries <- c("topicmodels", "dplyr", "stm", "quanteda")
lapply(libraries, require, character.only = T)
setwd("/Users/pedrorodriguez/Drobox/GitHub/Text-as-Data-Lab-Spring-2019/W10_04_18_19/")  # set working directory

# Loading data: Political blogs from the 2008 election on a conservative-liberal dimension
data(poliblog5k)
head(poliblog5k.meta)
head(poliblog5k.voc)

# Fits an STM model with 3 topics
system.time(
blog_stm <- stm(poliblog5k.docs, poliblog5k.voc, 3, prevalence = ~rating + s(day), data = poliblog5k.meta))

# A plot that summarizes the topics by what words occur most commonly in them
plot(blog_stm, type = "labels")

# A summary plot of the topics that ranks them by their average proportion in the corpus
plot(blog_stm, type = "summary")

# A visualization of what words are shared and distinctive to two topics
plot(blog_stm, type="perspectives", topics = c(1,2))

# Estimates a regression with topics as the dependent variable and metadata as the independent variables
# s() is a wrapper for bs() from the splines package
# A spline of degree D is a function formed by connecting polynomial segments of degree D
prep <- estimateEffect(1:3 ~ rating + s(day) , blog_stm, meta = poliblog5k.meta)

# Plots the distribution of topics over time
plot(prep, "day", blog_stm, topics = c(1,2), 
     method = "continuous", xaxt = "n", xlab = "Date")

# Plots the Difference in coverage of the topics according to liberal or conservative ideology
plot(prep, "rating", model = blog_stm,
     method = "difference", cov.value1 = "Conservative", cov.value2 = "Liberal")

# -----------------------------------------------
# Word Embeddings                                ---
# -----------------------------------------------
# Are word embeddings supervised or unsupervised?
# KEY DIFFERENCE between embeddings and other distributional semantic models we've seen: how we define context.
# Context in the case of word embeddings is defined by a window (usually symmetric) around the target word.
# GloVe vs. Word2Vec
# cool/intuitive intro to W2V: http://mccormickml.com/2016/04/19/word2vec-tutorial-the-skip-gram-model/

library(text2vec)

# choice parameters
WINDOW_SIZE <- 6
DIM <- 300
ITERS <- 10
MIN_COUNT <- 10

# load data
corpus <- readRDS("~/Dropbox/Research/Neuropolitics/WordSelection/R/GloVe/data/corpora.rds")
text <- corpus[["R"]]
rm(corpus)

# shuffle text
set.seed(42L)
text <- sample(text)

# ================================
# create vocab
# ================================
tokens <- space_tokenizer(text)
rm(text)
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = MIN_COUNT)  # keep only words that meet count threshold

# ================================
# create term co-occurrence matrix
# ================================
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = WINDOW_SIZE, skip_grams_window_context = "symmetric")

# ================================
# set model parameters
# ================================
glove <- GlobalVectors$new(word_vectors_size = DIM, 
                           vocabulary = vocab, 
                           x_max = 100,
                           lambda = 1e-5)

# ================================
# fit model
# ================================
word_vectors_main <- glove$fit_transform(tcm, 
                                         n_iter = ITERS,
                                         convergence_tol = 1e-3, 
                                         n_check_convergence = 1L,
                                         n_threads = RcppParallel::defaultNumThreads())

# ================================
# get output
# ================================
word_vectors_context <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_context) # word vectors

# pre-estimated embeddings
word_vectors_6_300_D <- readRDS("/Users/pedrorodriguez/Dropbox/Research/Neuropolitics/WordSelection/R/GloVe/CR-5yrs/word_vectors_R_6_300_D_3.rds") # local
word_vectors_6_300_R <- readRDS("/Users/pedrorodriguez/Dropbox/Research/Neuropolitics/WordSelection/R/GloVe/CR-5yrs/word_vectors_R_6_300_R_3.rds") # local
pretrained <- readRDS("/Users/pedrorodriguez/Dropbox/NYU/Teaching/Text as Data/homeworks/HW3/data/pretrained.rds") # GloVe pretrained (https://nlp.stanford.edu/projects/glove/)

# function to compute nearest neighbors
nearest_neighbors <- function(cue, embeds, N = 5, norm = "l2"){
  cos_sim <- sim2(x = embeds, y = embeds[cue, , drop = FALSE], method = "cosine", norm = norm)
  nn <- cos_sim <- cos_sim[order(-cos_sim),]
  return(names(nn)[2:(N + 1)])  # cue is always the nearest neighbor hence dropped
}

# e.g. 
nearest_neighbors("welfare", word_vectors_6_300_D, N = 10, norm = "l2")
nearest_neighbors("welfare", word_vectors_6_300_R, N = 10, norm = "l2")
nearest_neighbors("welfare", pretrained, N = 10, norm = "l2")

nearest_neighbors("abortion", word_vectors_6_300_D, N = 10, norm = "l2")
nearest_neighbors("abortion", word_vectors_6_300_R, N = 10, norm = "l2")
nearest_neighbors("abortion", pretrained, N = 10, norm = "l2")




