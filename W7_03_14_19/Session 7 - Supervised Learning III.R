# TA: Leslie Huang
# Course: Text as Data
# Date: 3/8/2018
# Recitation 7: Supervised Learning III
# Credit: Material updated from Patrick Chester

# Clear Global Environment
rm(list = ls())

# Setting WD
setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/W7_03_08_18/")

# Installing / Loading Libraries
# install.packages("tm")
# install.packages("NLP")
# install.packages("RTextTools")

library(NLP)
library(tm)
library(RTextTools)
library(wordcloud)

## 1 Visualizing Bullying Data---Example from Pablo Barbera's Short Course on R, NYU 2016   

# https://github.com/pablobarbera/data-science-workshop

tweets_df <- read.csv("bullying.csv", stringsAsFactors = F)

# Identify posts with and without bullying traces and create large documents
no_bullying <- paste(tweets_df$text[tweets_df$bullying_traces == "n"], collapse = " ")
yes_bullying <- paste(tweets_df$text[tweets_df$bullying_traces == "y"], collapse = " ")

# Create DTM and preprocess
groups <- VCorpus(VectorSource(c("No bullying" = no_bullying, "Yes bullying" = yes_bullying)))
groups <- tm_map(groups, content_transformer(tolower))
groups <- tm_map(groups, removePunctuation)
groups <- tm_map(groups, stripWhitespace)
bullying_dtm <- DocumentTermMatrix(groups)

# Label the two groups
bullying_dtm$dimnames$Docs = c("No bullying", "Yes bullying")

# Transpose matrix so that we can use it with comparison.cloud
bullying_tdm <- t(bullying_dtm)

# Compute TF-IDF transformation
bullying_tdm <- as.matrix(weightTfIdf(bullying_tdm))

# Display the two word clouds 
comparison.cloud(bullying_tdm, max.words = 100, colors = c("red", "blue")) # Function is from the wordcloud package

print_algorithms()

## 2 Classification with SVM 

# A) Linear - whole sample

# Let's train an SVM
tweets_df$type <- as.numeric(factor(tweets_df$bullying_traces))

# New package, better for SVM
?create_matrix
bullying_dfm  <- create_matrix(tweets_df$text, 
                      language = "english", 
                      stemWords = FALSE,
                      weighting = weightTfIdf, 
                      removePunctuation = FALSE
                      )
str(bullying_dfm)


# Make it all in-sample
?create_container
container <- create_container(bullying_dfm, 
                              t(tweets_df$type), 
                              trainSize = 1:length(tweets_df$type),
                              virgin = FALSE
                              )

# Train the model
?cross_validate
cv.svm <- cross_validate(container, nfold = 4, algorithm = 'SVM', kernel = 'linear')

## Comments:
# linear vs radial kernels, radial can overfit
# linear kernel is faster
# nfold is the number of times you have a different test set

# B) Linear - 90% Training data

training_break <- as.integer(0.9 * nrow(tweets_df))

container      <- create_container(bullying_dfm, 
                                   t(tweets_df$type), 
                                   trainSize = 1:training_break,
                                   testSize = (training_break+1):nrow(tweets_df), 
                                   virgin = FALSE
                                   )

# Let's train the model 
cv.svm <- cross_validate(container, 
                         nfold = 4,
                         algorithm = 'SVM', 
                         kernel = 'linear'
                         )


# Validate
cv.svm$meanAccuracy

prop.table(table(tweets_df$type)) # baseline


# How well did we do?

# C) Radial - 90% training data

# Let's try again with the radial kernel
cv.svm <- cross_validate(container, 
                         nfold = 4, 
                         algorithm = 'SVM', 
                         kernel = 'radial'
                         )

cv.svm$meanAccuracy

# D) Linear - 50% training data

# What if we try with different % test/train?
training_break <- as.integer(0.5 * nrow(tweets_df))

# There is no theoretical reason to choose .5 or .9
container      <- create_container(bullying_dfm, 
                                   t(tweets_df$type), 
                                   trainSize = 1:training_break,
                                   testSize = (training_break+1):nrow(tweets_df), 
                                   virgin = FALSE
)

cv.svm <- cross_validate(container, 
                         nfold = 4, 
                         algorithm = 'SVM', 
                         kernel = 'radial'
)

cv.svm$meanAccuracy

prop.table(table(tweets_df$type)) # baseline



## 3 Virality of stories from NYT

nyt_fb <- read.csv("nyt-fb.csv", stringsAsFactors = FALSE)

str(nyt_fb)


# Create variables for month and hour
head(nyt_fb$created_time)

month <- substr(nyt_fb$created_time, 6, 7)

hour <- substr(nyt_fb$created_time, 12, 13)

nyt_fb <- data.frame(nyt_fb, month, hour)


# Create a "viral" index
total.resp <- nyt_fb$likes_count + nyt_fb$shares_count + nyt_fb$comments_count

# Look at the extreme of the distribution
perc_90 <- quantile(total.resp, .9) 

# Create a binary y variable with values 2 being viral and 1 being non-viral
nyt_fb$viral <- as.numeric(total.resp > perc_90)

# For the purposes of not destroying my laptop, let's choose a set of features
training_break <- as.integer(0.9*nrow(nyt_fb))

# A) Classification with SVM
nyt_dtm       <- create_matrix(nyt_fb$message, language = "english", stemWords = FALSE,
                           weighting = weightTfIdf, removePunctuation = FALSE)

container      <- create_container(nyt_dtm, t(nyt_fb$type), trainSize = 1:training_break,
                                   testSize = (training_break+1):nrow(nyt_fb), virgin = FALSE)

cv.svm <- cross_validate(container, nfold = 2, algorithm = 'SVM', kernel = 'linear')

cv.svm$meanAccuracy

prop.table(table(nyt_fb$viral))


# B) Classification with logistic regression

message <- removePunctuation(tolower(nyt_fb$message))
nyt_fb$israel <- grepl("israel", message)
nyt_fb$trump <- grepl("trump", message)
nyt_fb$hillary <- grepl("hillary", message)
nyt_fb$obama <- grepl("barack|obama", message)
nyt_fb$terror <- grepl("terror|isis|isil|qaeda", message)
nyt_fb$kill <- grepl("kill|murder|shot", message)
nyt_fb$debate <- grepl("debat", message)

# Fitting a logistic model
glm.viral <- glm(as.factor(viral) ~ month + hour + 
                   israel + trump + hillary + obama + terror + kill + 
                   debate, 
                 data = nyt_fb, 
                 family = binomial(logit)
                 )


tab <- table(round(glm.viral$fitted.values), nyt_fb$viral)

# Accuracy of Logistic Regression
sum(diag(tab))/sum(tab)

# In this case, SVM had a higher level of accuracy than logit. Why might that be? 

