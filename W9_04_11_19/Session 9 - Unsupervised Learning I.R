# TA: Leslie Huang
# Course: Text as Data
# Date: 3/29/2018
# Recitation 9: Unsupervised Learning I

# Set up workspace
rm(list = ls())

setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/W9_03_29_18/")

# Loading packages
#install.packages("lsa")
#install.packages("factoextra")

library(quanteda)
library(quanteda.corpora)

## 1 PCA

# 1.1 Two functions in base R:

?prcomp # SVD on the (centered) input data
?princomp # eigendecomposition on the covariance matrix of the input data -- can also use option for covariance matrix 

# Remember to center your data! -- use scale() on your matrix beforehand, or the option in prcomp()
# And don't have any missing values!

library(factoextra) # makes it easy to work with PCA

# 1.2 Example
data("data_corpus_sotu")

SOTU_dfm <- dfm(data_corpus_sotu[145:223,], 
                stem = T, 
                remove_punct = T, 
                remove = stopwords("english")
)

SOTU_mat <- convert(SOTU_dfm, to = "matrix") # convert to matrix

SOTU_pca <- prcomp(SOTU_mat, center = TRUE, scale = TRUE)

# Elbow plot
plot(SOTU_pca, type = "l")

# How much variance do the first few PCs account for?
summary(SOTU_pca)

# Eigenvalues
head(get_eigenvalue(SOTU_pca))

fviz_eig(SOTU_pca, addlabels = TRUE, ylim = c(0, 50))

# Loadings for each variable: columns contain the eigenvectors
SOTU_pca$rotation[1:10, 1:5]

# Value of the rotated data: your "new", dimensionality reduced data
View(SOTU_pca$x)

# Visualization resources:

# Tutorial from factoextra author about how to use his package to explore and visualize PCA results: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# See here for visualizing PCA with the ggbiplot library: https://www.r-bloggers.com/computing-and-visualizing-pca-in-r/


## 2 Latent Semantic Analysis (LSA) aka Latent Semantic Indexing (LSI)

library(lsa)

# Let's keep using the SOTU data from before

# 2.1 Create LSA weights using TDM
SOTU_lsa_auto <- lsa(t(SOTU_dfm))

# Note: We are *not* doing the local/global weighting in this example!
?gw_idf
# From lecture
# Local weight function: log(tf_ij + 1)
# global weight function: 1 + sum( (p_ij * log(p_ij) ) / log(n) ) where p_ij = tf_ij/gf_i


# 2.2 Check to see what a good number of dimensions is
?dimcalc_share

# lsa_obj$tk -- truncated matrix Tk from term vector matrix T (constituting left singular vectors from the SVD of the original matrix)
# svd(matrix)$d -- singular values of svd(matrix)
SOTU_lsa_auto_svd <- svd(SOTU_lsa_auto$tk)$d

dimcalc_share(share = 0.5)(SOTU_lsa_auto_svd)


# By default, share is set to .5; let's try .9
# share = fraction of the sum of the selected singular values over the sum of all singular values
dimcalc_share(share = 0.9)(SOTU_lsa_auto_svd)

# Lecture example uses dims = 5
SOTU_lsa_5 <- lsa(t(SOTU_dfm), 5 )

SOTU_lsa_5_mat <- t(as.textmatrix(SOTU_lsa_5) )


# 2.3 Compare features for a few speeches

SOTU_dfm@Dimnames$docs[9]
topfeatures(SOTU_dfm[9,])

# With 5 dims:
sort(SOTU_lsa_5_mat[9,], decreasing=T)[1:10]

# With auto (14) dims:
sort(t(as.textmatrix(SOTU_lsa_auto))[9, ], decreasing = T)[1:10]

# Another example:
SOTU_dfm@Dimnames$docs[55]
topfeatures(SOTU_dfm[55,])

sort(SOTU_lsa_5_mat[55,], decreasing=T)[1:10]
sort(t(as.textmatrix(SOTU_lsa_auto))[55, ], decreasing = T)[1:10]


# associate(): a method to identify words that are most similar to other words using a LSA
?associate
# uses cosine similarity between input term and other terms

SOTU_lsa_3 <- lsa(t(SOTU_dfm), 3 )

SOTU_lsa_3_mat <- as.textmatrix(SOTU_lsa_3) 

china <- associate(SOTU_lsa_3_mat, "china", "cosine", threshold = .7)
china[1:10]


oil <- associate(SOTU_lsa_3_mat, "oil", "cosine", threshold = .7)
oil[1:10]

america <- associate(SOTU_lsa_3_mat, "america", "cosine", threshold = .7)
america[1:10]

health <- associate(SOTU_lsa_3_mat, "health", "cosine", threshold = .7)
health[1:10]

# Keep this in mind when we do topic models!


## 2 WORDFISH

# How is it different from other approaches we've used for scaling?

# 2.1 Read in conservative and labour manifestos (from Recitation 6)
setwd("../W6_02_27_18/cons_labour_manifestos")

files <- list.files( full.names=TRUE)
text <- lapply(files, readLines)
text<-unlist(lapply(text, function(x) paste(x, collapse = " ")))

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

lab_con_dfm <- dfm(man_df$text, 
                   stem = T, 
                   remove = stopwords("english"), 
                   remove_punct = T
                   )

# 2.2 fit wordfish

# Setting the index on parties
manifestos_fish <- textmodel_wordfish(lab_con_dfm, c(1,24)) # second parameter corresponds to index texts

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