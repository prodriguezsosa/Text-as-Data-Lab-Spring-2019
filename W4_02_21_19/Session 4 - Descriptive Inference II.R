# TA: Leslie Huang
# Course: Text as Data
# Date: 2/15/2017
# Recitation 4: Descriptive Inference II
# Credits: Updated and expanded from materials from Patrick Chester; some examples taken wholesale from Ken Benoit's NYU Dept. of Politics short course Fall 2014, available on his website: www.kenbenoit.net

# Setup environment
rm(list = ls())
set.seed("1234")

# 1 Loading packages
library(quanteda)
library(quanteda.corpora)
library(dplyr)

# 2 Load in data: Irish budget proposals from 2008-2012
data("data_corpus_irishbudgets")

irish_budget_texts <- texts(data_corpus_irishbudgets)

# 3 Lexical diversity measures

# 3.1 TTR 
budget_tokens <- tokens(irish_budget_texts, remove_punct = TRUE) 

# Num tokens per document
num_tokens <- lengths(budget_tokens)

num_types <- ntype(budget_tokens)

irish_budget_TTR <- num_types / num_tokens

head(irish_budget_TTR)

View(irish_budget_TTR)

# Would you expect the budgets to become more or less complex over time?

# 3.2 Mean per-document TTR scores by year, party

TTR_by_year <- aggregate(irish_budget_TTR, by = list(data_corpus_irishbudgets[["year"]]$year), FUN = mean)

plot(TTR_by_year)

aggregate(irish_budget_TTR, by = list(data_corpus_irishbudgets[["party"]]$party), FUN = mean)


# 3.3 Calculate TTR score by year, party 

# by year
textstat_lexdiv(dfm(data_corpus_irishbudgets, groups = "year", remove_punct = TRUE, verbose = TRUE), measure = "TTR")

# Sidebar: using the "groups" parameter is how to group documents by a covariate -- note how this changes the ndocs of your corpus

textstat_lexdiv(dfm(data_corpus_irishbudgets, groups = "party", remove_punct = TRUE, verbose = TRUE), measure = "TTR")

# Thoughts on TTR


# 4 Readability measure

# 4.1 FRE
View(textstat_readability(data_corpus_irishbudgets, "Flesch"))

textstat_readability(texts(data_corpus_irishbudgets, groups = "year"), "Flesch")

textstat_readability(texts(data_corpus_irishbudgets, groups = "party"), "Flesch")


# 4.2 Dale-Chall measure

textstat_readability(data_corpus_irishbudgets, "Dale.Chall.old")

textstat_readability(texts(data_corpus_irishbudgets, groups = "year"), "Dale.Chall.old")

textstat_readability(texts(data_corpus_irishbudgets, groups = "party"), measure = "Dale.Chall.old")

# 4.3 let's compare each measure

all_readability_measures <- textstat_readability(data_corpus_irishbudgets, c("Flesch", "Dale.Chall", "SMOG", "Coleman.Liau", "Fucks"))

readability_matrix <- cbind(all_readability_measures$Flesch, all_readability_measures$Dale.Chall, all_readability_measures$SMOG, all_readability_measures$Coleman.Liau, all_readability_measures$Fucks)

readability_cor <- cor(readability_matrix)
rownames(readability_cor) <- c("Flesch", "Dale-Chall", "SMOG", "Coleman Liau", "Fucks")
colnames(readability_cor) <- c("Flesch", "Dale-Chall", "SMOG", "Coleman Liau", "Fucks")
readability_cor

# 5 Bootstrapping

# data prep: remove smaller parties so we're left with the 6 largest
iebudgetsCorpSub <- corpus_subset(data_corpus_irishbudgets, !(party %in% c("WUAG", "SOC", "PBPA" )))

# convert corpus to df for stratified sampling
iebudgets_df <- data.frame(texts = iebudgetsCorpSub[["texts"]]$texts, 
                 party = iebudgetsCorpSub[["party"]]$party,
                 year = as.numeric(iebudgetsCorpSub[["year"]]$year),
                 stringsAsFactors = FALSE)

# Let's filter out empty speeches
iebudgets_df <- na.omit(iebudgets_df)

# We will use a loop to bootstrap a sample of texts and subsequently calculate standard errors

iters <- 10

# initialize data frames to store results
party_FRE <- data.frame(matrix(ncol = length(unique(iebudgets_df$party)), nrow = iters))
colnames(party_FRE) <- names(table(iebudgets_df$party))

# run the bootstrap

for(i in 1:iters) {
  
  iebudgets_grouped <- group_by(iebudgets_df, party)
  
  # take a sample of 20 documents per level (party)
  bootstrap_sample <- sample_n(iebudgets_grouped, 20, replace = TRUE)
  
  readability_results <- textstat_readability(bootstrap_sample$texts, measure = "Flesch")
  
  #store results
  
  readability_grouped <- group_by(readability_results, bootstrap_sample$party)
  readability_means <- summarize(readability_grouped, mean(Flesch))
  
  party_FRE[i, ] <- t(readability_means[, 2])
  
}

# inspect the results
View(party_FRE)

# Define the standard error function
std <- function(x) sd(x)/sqrt(length(x))

# Calculate standard errors and point estimates

party_ses <- apply(party_FRE, 2, std)

party_means <- apply(party_FRE, 2, mean)

# Plot results--party

coefs<-party_means
ses<-party_ses

y.axis <- c(1:6)
min <- min(coefs - 2*ses)
max <- max(coefs + 2*ses)
var.names <- colnames(party_FRE)
adjust <- 0
par(mar=c(2,8,2,2))

plot(coefs, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = .8, 
     xlim=c(min,max),ylim = c(.5,6.5), main = "")
rect(min,.5,max,1.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,1.5,max,2.5, col = c("grey95"), border="grey90", lty = 2)
rect(min,2.5,max,3.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,3.5,max,4.5, col = c("grey95"), border="grey90", lty = 2)
rect(min,4.5,max,5.5, col = c("grey97"), border="grey90", lty = 2)
rect(min,5.5,max,6.5, col = c("grey97"), border="grey90", lty = 2)

axis(1, at = seq(min,max,(max-min)/10), 
     labels = c(round(min+0*((max-min)/10),3),
                round(min+1*((max-min)/10),3),
                round(min+2*((max-min)/10),3),
                round(min+3*((max-min)/10),3),
                round(min+4*((max-min)/10),3),
                round(min+5*((max-min)/10),3),
                round(min+6*((max-min)/10),3),
                round(min+7*((max-min)/10),3),
                round(min+8*((max-min)/10),3),
                round(min+9*((max-min)/10),3),
                round(max,3)),tick = T,cex.axis = .75, mgp = c(2,.7,0))
axis(2, at = y.axis, label = var.names, las = 1, tick = FALSE, cex.axis =.8)
abline(h = y.axis, lty = 2, lwd = .5, col = "white")
segments(coefs-qnorm(.975)*ses, y.axis+2*adjust, coefs+qnorm(.975)*ses, y.axis+2*adjust, lwd =  1)

segments(coefs-qnorm(.95)*ses, y.axis+2*adjust-.035, coefs-qnorm(.95)*ses, y.axis+2*adjust+.035, lwd = .9)
segments(coefs+qnorm(.95)*ses, y.axis+2*adjust-.035, coefs+qnorm(.95)*ses, y.axis+2*adjust+.035, lwd = .9)
points(coefs, y.axis+2*adjust,pch=21,cex=.8, bg="white")

# Compare with calculating the per-party mean Flesch statistic

summarize(
          group_by(
                  textstat_readability(iebudgets_df$texts, measure = "Flesch"),
                  iebudgets_df$party
          ),
          mean(Flesch)
)

# How well did we do?

# 6 Sophistication

#devtools::install_github("kbenoit/sophistication")
library("sophistication")

# We'll run through the example from https://github.com/kbenoit/sophistication

# Load data
data(data_corpus_sotu, package = "quanteda.corpora")

# Make snippets of 1 sentence each, then clean them
snippetData <- snippets_make(data_corpus_sotu, nsentence = 1, minchar = 150, maxchar = 250)
snippetData <- snippets_clean(snippetData)

# Sample the snippets
testData <- snippetData[sample(1:nrow(snippetData), 5), ]

# generate n-1 pairs from n test snippets for a minimum spanning tree
(snippetPairsMST <- pairs_regular_make(testData))

# generate more pairs from a larger sample of data
snippetPairsAll <- pairs_regular_make(snippetData[sample(1:nrow(snippetData), 1000), ])

# Make some "Gold" questions -- for use with CrowdFlower workers 
# default reading level is Flesch and the default difference in readability of the two snippets in the pair is the 0.1 and 0.9 quintiles
pairs_gold_make(snippetPairsAll, n.pairs = 10)
