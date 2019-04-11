# TA: Pedro L. Rodríguez
# Course: Text as Data
# Date: 04/11/2019
# Lab adapted from: Leslie Huang.

# Set up workspace
rm(list = ls())

setwd("~/Drobox/GitHub/Text-as-Data-Lab-Spring-2019/W9_04_11_19/")

# Loading packages
#install.packages("lsa")
#install.packages("factoextra")

library(quanteda)
library(quanteda.corpora)
library(dplyr)

## 1 PCA

# 1.1 Two functions in base R for PCA:
# see: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
?prcomp # uses the singular value decomposition approach: examines the covariances/correlations between individuals
?princomp # uses the spectral decomposition approach: examines the covariances/correlations between variables (need more individuals than variables)

# Remember to center your data! (default = TRUE) -- use scale() on your matrix beforehand, or the option in prcomp()
# And don't have any missing values!

library(factoextra) # makes it easy to work with PCA (great for visualization)

# 1.2 Example
data("data_corpus_sotu")
SOTU <- corpus_subset(data_corpus_sotu, Date > "1900-01-01")

SOTU_dfm <- dfm(SOTU, 
                stem = T, 
                remove_punct = T, 
                remove = stopwords("english")
)


SOTU_mat <- convert(SOTU_dfm, to = "matrix") # convert to matrix

# run pca
SOTU_pca <- prcomp(SOTU_mat, center = TRUE, scale = TRUE)

# visualize eigenvalues (scree plot: shows percentage of variance explained by each dimension)
fviz_eig(SOTU_pca, addlabels = TRUE)

# Loadings for each variable: columns contain the eigenvectors
SOTU_pca$rotation[1:10, 1:5]
dim(SOTU_pca$rotation)

# Q: can we interpret the dimensions?
# top loadings on PC1
pc_loadings <- SOTU_pca$rotation

# what do we expect this correlation to be?
cor(pc_loadings[,1], pc_loadings[,2])  # these should be orthogonal

# token loadings
N <- 10
pc1_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,1])) %>% arrange(-loading)
pc1_loading$loading <- scale(pc1_loading$loading, center = TRUE)
pc1_loading <- rbind(top_n(pc1_loading, N, loading),top_n(pc1_loading, -N, loading))
pc1_loading <- transform(pc1_loading, token = factor(token, levels = unique(token)))

# plot top tokens according to absolute loading values
ggplot(pc1_loading, aes(token, loading)) + 
  geom_bar(stat = "identity", fill = ifelse(pc1_loading$loading <= 0, "grey20", "grey70")) +
  coord_flip() + 
  xlab("Tokens") + ylab("Tokens with Top Loadings on PC1") +
  scale_colour_grey(start = .3, end = .7) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=16),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "top",
        legend.spacing.x = unit(0.25, 'cm'),
        plot.margin=unit(c(1,1,0,0),"cm"))

# Value of the rotated data: your "new", dimensionality reduced data
View(SOTU_pca$x)  # each observation 

# retrieve most similar documents
library(text2vec)

# function computes cosine similarity between query and all documents and returns N most similar
nearest_neighbors <- function(query, low_dim_space, N = 5, norm = "l2"){
  cos_sim <- sim2(x = low_dim_space, y = low_dim_space[query, , drop = FALSE], method = "cosine", norm = norm)
  nn <- cos_sim <- cos_sim[order(-cos_sim),]
  return(names(nn)[2:(N + 1)])  # query is always the nearest neighbor hence dropped
}

# apply to document retrieval
nearest_neighbors(query = "Obama-2009", low_dim_space = SOTU_pca$x, N = 5, norm = "l2")

# Visualization resources:

# Tutorial from factoextra author about how to use his package to explore and visualize PCA results: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# See here for visualizing PCA with the ggbiplot library: https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/


## 2 Latent Semantic Analysis (LSA) aka Latent Semantic Indexing (LSI)

library(lsa)
# foundational theory: distributional hypothesis
# what is the context in LSA? document

# Let's keep using the SOTU data from before
SOTU_mat_lsa <- convert(SOTU_dfm, to = "lsa") # convert to transposed matrix (so terms are rows and columns are documents = TDM)
SOTU_mat_lsa <- lw_logtf(SOTU_mat_lsa) * gw_idf(SOTU_mat_lsa) # local - global weighting (akin to TFIDF)

# 2.1 Create LSA weights using TDM
SOTU_lsa <- lsa(SOTU_mat_lsa)
#lsa(myMatrix, dims = dimcalc_share(share = 0.8)) # share = fraction of the sum of the selected singular values over the sum of all singular values, default is 0.5

# what do we expect this correlation to be?
cor(SOTU_lsa$tk[,1], SOTU_lsa$tk[,2])  # these should be orthogonal

# 2.2 Check to see what a good number of dimensions is
?dimcalc_share

# lsa_obj$tk = truncated term matrix from term vector matrix T (constituting left singular vectors from the SVD of the original matrix)
# lsa_obj$dk = truncated document matrixfrom document vector matrix D (constituting right singular vectors from the SVD of the original matrix)
# lsa_obj$sk = singular values: Matrix of scaling values to ensure that multiplying these matrices reconstructs TDM
# see: https://cran.r-project.org/web/packages/lsa/lsa.pdf

# Lecture example uses dims = 5
SOTU_lsa_5 <- lsa(SOTU_mat_lsa, 5)

# display generated LSA space
?as.textmatrix
SOTU_lsa_5_mat <- t(as.textmatrix(SOTU_lsa_5))

# 2.3 Q: What are these documents about?
# Compare features for a few speeches
SOTU_dfm@Dimnames$docs[9]
topfeatures(SOTU_dfm[9,])

# With 5 dims:
sort(SOTU_lsa_5_mat[9,], decreasing=T)[1:10]

# With auto (21) dims:
sort(t(as.textmatrix(SOTU_lsa))[9, ], decreasing = T)[1:10]

# Another example:
SOTU_dfm@Dimnames$docs[55]
topfeatures(SOTU_dfm[55,])

sort(SOTU_lsa_5_mat[55,], decreasing=T)[1:10]
sort(t(as.textmatrix(SOTU_lsa_auto))[55, ], decreasing = T)[1:10]

# Q: How are words related?
# associate(): a method to identify words that are most similar to other words using a LSA
?associate
# uses cosine similarity between input term and other terms
SOTU_lsa_mat <- as.textmatrix(SOTU_lsa) 

oil <- associate(SOTU_lsa_mat, "oil", "cosine", threshold = .7)
oil[1:10]

health <- associate(SOTU_lsa_mat, "health", "cosine", threshold = .7)
health[1:10]

# Keep this in mind when we do topic models!

## 2 WORDFISH
# one-dimensional text scaling method.
# unlike wordscores, it does not require reference texts

# How is it different from other approaches we've used for scaling?

# 2.1 Read in conservative and labour manifestos (from Recitation 6)
setwd("~/Drobox/GitHub/Text-as-Data-Lab-Spring-2019/W6_03_07_19/cons_labour_manifestos")

files <- list.files( full.names=TRUE)
text <- lapply(files, readLines)
text <- unlist(lapply(text, function(x) paste(x, collapse = " ")))

# Name data
files <- unlist(files)
files <- gsub("./", "", files )
files <- gsub(".txt", "", files )

# Create metadata
year <- unlist(strsplit(files, "[^0-9]+"))
year <- year[year!=""]

party <- unlist(strsplit(files, "[^A-z]+"))
party <- party[party!="a" & party!="b"]

#create data frame
man_df <- data.frame(year = factor(as.numeric(year)),
                     party = factor(party),
                     text = text,
                     stringsAsFactors = FALSE)

# add text labels
man_df$text_label <- paste(man_df$party, man_df$year, sep = "_")


lab_con_dfm <- dfm(man_df$text, 
                   stem = T, 
                   remove = stopwords("english"), 
                   remove_punct = T
)

# 2.2 fit wordfish

# Setting the index on parties
manifestos_fish <- textmodel_wordfish(lab_con_dfm, c(1,24)) # second parameter corresponds to index texts

# visualize one-dimensional scaling
textplot_scale1d(manifestos_fish)
textplot_scale1d(manifestos_fish, groups = man_df$party)

# Plot of document positions
plot(year[1:23], manifestos_fish$theta[1:23]) # These are the conservative manifestos
points(year[24:46], manifestos_fish$theta[24:46], pch = 8) # These are the Labour manifestos

plot(as.factor(party), manifestos_fish$theta)

# most important features--word fixed effects
words <- manifestos_fish$psi # values
names(words) <- manifestos_fish$features # the words

sort(words)[1:50]
sort(words, decreasing=T)[1:50]

# Guitar plot
weights <- manifestos_fish$beta

plot(weights, words)

# also check out wordshoal!