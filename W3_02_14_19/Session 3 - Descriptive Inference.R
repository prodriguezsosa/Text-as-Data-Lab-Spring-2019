# TA: Leslie Huang
# Course: Text as Data
# Date: 02/08/2018
# Code originally from: Patrick Chester; updated and expanded by Leslie Huang

## Set up Quanteda 

# Clear Global Environment
rm(list = ls())

# Libraries
library(quanteda)
library(quanteda.corpora)

## 1 Non English texts

# 1.1 Non-English stopwords

stopwords(language = "spanish")

stopwords(language = "german")

stopwords(language = "zh", source = "misc")


# 1.2 Text encoding

# What is text encoding?
# How do you figure out what kind you have (e.g. scraped text from the Internet)?
# What kind of encoding can R and/or quanteda handle?

# 1.3 Some types of text encoding
# UTF-8
# ASCII (subset of UTF-8)
# Latin-1

# UTF-8 represents characters from European languages (English, Spanish, German, French, etc) and some characters from Chinese/Japanese/Korean, plus emojis.

# Note: Text obtained from Internet sources can be messy. Issues can especially arise when you are working with texts from multiple sources and you end up with a mixture of encodings. This can cause the encoding to be detected incorrectly when you read in the text.

# 1.4 What encoding do you have?

# You can check with this function in base R
validUTF8("This is a sentence")

# You can use the package utf8(), written by Patrick Perry from NYU
# Read about it here: https://github.com/patperry/r-utf8
# install.packages("utf8")
library("utf8")

as_utf8("\xF0\x9F\x98\x81")
print("\xF0\x9F\x98\x81") # There are issues with base R's print() function for Unicode
# any guesses what this is?
utf8_print("\xF0\x9F\x98\x81")

# 1.5 What if you get a weird character and you're not sure?

# install.packages("stringi")
library("stringi")

# Use the encoding guesser to guess what this character is
stri_enc_detect("0x00E3")

# It's only a guess!

# What's ISO-8859-1?
# This is another name for the Latin-1 encoding. 

# 1.6 How do you convert encodings?

test_str <- "São Paulo"
validUTF8(test_str)

converted_str <- iconv("São Paulo", from = "UTF-8", to = "latin1")

converted_str
# Looks the same right?

charToRaw(converted_str) # Latin-1 encoding

charToRaw(test_str) # UTF-8 encoding

# But what about here?
iconv("ã", from = "UTF-8", to = "ASCII")

# In most cases, your text will probably already be in UTF-8. 
# In most cases, you want to convert your text to UTF-8 (with the possible exception of languages that do not use the Latin alphabet)

# The authors of quanteda have also written a package called readtext() that can also deal with encodings in text corpora!


## 2 Demonstrate Heap's law 
# Token-type relationship in corpus

#     M = kT^b

# M = vocab size (num of types)
# T = number of tokens

# k, b are constants
# 30 <= k <= 100
# 0.4 <= b <= 0.6

# 2.1 Example using data from the corpus of inaugural speeches
tokens <- tokens(data_corpus_inaugural, remove_punct = TRUE) 
Tee <- sum(lengths(tokens))

inaug_dfm <- dfm(data_corpus_inaugural)

M <- nfeat(inaug_dfm)

# Let's check using parameter values from MRS Ch. 5 for a corpus with more than 100,000 tokens

k <- 44
b <- .49

k * (Tee)^b

M

# Let's think about why


# New parameters

k <- 41
b <- 0.46

k * (Tee)^b

## 3 Demonstrate Zipf's law
# Term frequency in corpus and rank

# x-axis: log of ranks 1 through 100
# y axis log of frquency of top 100 terms from the DFM

plot(log10(1:100), log10(topfeatures(inaug_dfm, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", main = "Top 100 Words in U.S. Presidential Inaugural Speech Corpus")

# Fits a linear regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(inaug_dfm, 100)) ~ log10(1:100))

# Adds the fitted line from regression to the plot
abline(regression, col = "red")

# Returns the 95% confidence intervals for the regression coefficients
confint(regression)

# Provides R-squared, F-test, and cofficient estimates from regression
summary(regression)

## 4 Stopwords: do they affect Zipf's law?

mydfm <- dfm(data_corpus_inaugural, remove=stopwords("english"))

plot(log10(1:100), log10(topfeatures(mydfm, 100)),
     xlab = "log10(rank)", ylab = "log10(frequency)", main = "Top 100 Words")

# Regression to check if slope is approx -1.0
regression <- lm(log10(topfeatures(mydfm, 100)) ~ log10(1:100))
abline(regression, col = "red")
confint(regression)


## 5 Key Words In Context (KWIC) is a good way to summarize info about a topic

kwic(data_corpus_inaugural, "America", 3, case_insensitive = FALSE)

help(kwic)

# Suggested terms?


## 6 Measuring similarity

# This helps illustrate the value of the vector representation

# 6.1 Cosine similarity--take the dot product of two vectors

# x * y = |x||y|cos
# cos = x*y/|x||y|

x <- c(1, 2, 3)
y <- c(1, 2, 3)
 
# Define the norm function (to calculate the denominator |x| and |y|)

norm_vec <- function(x) sqrt(sum(x^2))

x %*% y / (norm_vec(x) * norm_vec(y))

# What if they're different

a <- c(1, 2, 3)
b <- c(1, 2, 4000)

a %*% b / (norm_vec(a) *norm_vec(b) )

# Let's do it with texts

last_speech_text <- data_corpus_inaugural[ndoc(data_corpus_inaugural)]
first_speech_text <- data_corpus_inaugural[ndoc(data_corpus_inaugural) - 2]

# Make a dfm of these two

first_last_dfm <- dfm(c(last_speech_text, first_speech_text), remove = stopwords("english"), stem = TRUE)

# Calculate similarity

similarity_first_last_with_preprocessing <- textstat_simil(first_last_dfm, margin = "documents", method = "correlation")

as.matrix(similarity_first_last_with_preprocessing)

# 6.2 Let's see how stopwords/stemming affect similarity

first_last_dfm_no_preprocessing <- dfm(c(last_speech_text, first_speech_text))

# Calculate similarity

similarity_first_last_no_preprocessing <- textstat_simil(first_last_dfm_no_preprocessing, margin = "documents", method = "correlation")

as.matrix(similarity_first_last_no_preprocessing)

# Make a dfm of a several documents

several_inaug_dfm <- dfm(corpus_subset(data_corpus_inaugural , Year > 1980), remove = stopwords("english"), stem = TRUE)

# Calculate similarity

similarity_several <- textstat_simil(several_inaug_dfm, margin = "documents", method = "correlation")

View(as.matrix(similarity_several))

# Other options available: Manhattan distance, cosine, etc.
?textstat_simil

# Specific comparisons with Obama's first inauguration speech

textstat_simil(several_inaug_dfm, "2009-Obama", margin = "documents", method = "correlation")
