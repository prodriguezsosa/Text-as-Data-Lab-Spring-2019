# TA: Leslie Huang
# Course: Text as Data
# Date: 4/18/2017
# Recitation 11: Unsupervised Learning IIb

rm(list=ls())

setwd("./Text-as-Data-Lab-Spr2018/W11_04_12_18/")

set.seed(1234)

# Possibly new packages to install
#install.packages("lattice")
#install.packages("bursts")

libraries <- c("Matrix", "ldatuning", "topicmodels", "readtext", "dplyr", "stm", "quanteda", "lda", "bursts", "tidytext", "ggplot2", "lattice", "quanteda.corpora")
lapply(libraries, require, character.only = T)

## 1 Correlated Topic Models (CTM)

# What is a CTM?

# Loading the data and creating a DFM
data("data_corpus_movies", package = "quanteda.corpora")

movies_corp <- corpus_sample(data_corpus_movies, size = 20)

movies_dfm <- dfm(movies_corp, remove = stopwords("english"), remove_punct = T)

# FYI, this function takes a long time to run
as_ctm <- CTM(movies_dfm, k = 10, control = list(seed = 1234))

# Summarization
betas <- t(as_ctm@beta)

words <- as_ctm@terms

# Gets the top terms
get_terms(as_ctm,10)

# Since tidytext doesn't work with CTM, I manually extract the top 10 terms for each topic with this function
df <- lapply(1:ncol(betas), function(x){data.frame(Topic = x, Term = words, Beta = betas[,x]) %>% arrange(desc(Beta)) %>% slice(1:10)})

df_ctm <- bind_rows(df)


df_ctm %>%
  mutate(Term = reorder(Term, Beta)) %>%
  ggplot(aes(Term, Beta, fill = factor(Topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Topic, scales = "free") +
  coord_flip()


# Sigma is the Variance Covariance Matrix of the all Topics
topic_var_matrix<-as_ctm@Sigma
topic_cor_matrix <- cov2cor(topic_var_matrix)

#Visualizing correlation matrix between topics
rgb.palette <- colorRampPalette(c("green", "blue"), space = "rgb")
levelplot(topic_cor_matrix, xlab="", ylab="", col.regions=rgb.palette(120))


## 2 Structural Topic Models (STM)

# What is an STM?

# Loading data: Political blogs from the 2008 election on a conservative-liberal dimension

data(poliblog5k)

head(poliblog5k.meta)

head(poliblog5k.voc)

# Fits an STM model with 3 topics
blog_stm <- stm(poliblog5k.docs, poliblog5k.voc, 3,
            prevalence=~rating + s(day), data=poliblog5k.meta)


# A plot that summarizes the topics by what words occur most commonly in them
plot(blog_stm,type="labels")

# A summary plot of the topics that ranks them by their average proportion in the corpus
plot(blog_stm, type="summary")

# A visualization of what words are shared and distinctive to two topics
plot(blog_stm, type="perspectives", topics=c(1,2))

# Estimates a regression with topics as the dependent variable and metadata as the independent variables
prep <- estimateEffect(1:3 ~ rating + s(day) , blog_stm, meta=poliblog5k.meta)

# Plots the distribution of topics over time
plot(prep, "day", blog_stm, topics = c(1,2), 
     method = "continuous",xaxt = "n", xlab = "Date")

# Plots the Difference in coverage of the topics according to liberal or conservative ideology
plot(prep, "rating", model=blog_stm,
     method="difference",cov.value1="Conservative",cov.value2="Liberal")

