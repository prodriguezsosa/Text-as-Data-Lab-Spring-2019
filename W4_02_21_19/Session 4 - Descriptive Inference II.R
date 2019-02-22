# TA: Pedro L. Rodríguez
# Course: Text as Data
# Date: 2/21/2019
# Lab adapted from: Kevin Munger, Patrick Chester and Leslie Huang.

# Setup environment
rm(list = ls())

# 1 Loading packages ---------------------------------------
library(quanteda)
library(quanteda.corpora)
library(dplyr)
library(ggplot2)

# 2 Load in data: Irish budget proposals from 2008-2012 ----
# "speeches and document-level variables from the debate over the Irish budget".

data("data_corpus_irishbudgets")
irish_budget_texts <- texts(data_corpus_irishbudgets)

#------------------------------
# 3 LEXICAL DIVERSITY MEASURES
#------------------------------

# 3.1 TTR 
budget_tokens <- tokens(irish_budget_texts, remove_punct = TRUE) 

# Num tokens per document
num_tokens <- lengths(budget_tokens)

num_types <- ntype(budget_tokens)

irish_budget_TTR <- num_types / num_tokens

head(irish_budget_TTR)

# Would you expect the budgets to become more or less diverse over time?

# 3.2 Mean per-document TTR scores by year, party

TTR_by_year <- aggregate(irish_budget_TTR, by = list(data_corpus_irishbudgets[["year"]]$year), FUN = mean, na.rm = TRUE) %>% setNames(c("year", "TTR"))

plot(TTR_by_year)

aggregate(irish_budget_TTR, by = list(data_corpus_irishbudgets[["party"]]$party), FUN = mean) %>% setNames(c("party", "TTR"))

# 3.3 Calculate TTR score by year, party 

# by year
# textstat_lexdiv: "calculates the lexical diversity or complexity of text(s)" using any number of measures.'
TTR <- textstat_lexdiv(budget_tokens, measure = "TTR")
aggregate(TTR$TTR, by = list(data_corpus_irishbudgets[["year"]]$year), FUN = mean, na.rm = TRUE) %>% setNames(c("year", "TTR"))

# Sidebar: using the "groups" parameter is how to group documents by a covariate -- note how this changes the ndocs of your corpus
aggregate(TTR$TTR, by = list(data_corpus_irishbudgets[["party"]]$party), FUN = mean, na.rm = TRUE) %>% setNames(c("party", "TTR"))

# Thoughts on TTR

#------------------------------
# 4 COMPLEXITY (READIBILITY) MEASURES
#------------------------------

# 4.1 FRE (https://en.wikipedia.org/wiki/Flesch–Kincaid_readability_tests)
textstat_readability(data_corpus_irishbudgets, "Flesch") %>% head()

textstat_readability(texts(data_corpus_irishbudgets, groups = "year"), "Flesch") 

textstat_readability(texts(data_corpus_irishbudgets, groups = "party"), "Flesch")

# 4.2 Dale-Chall measure (https://en.wikipedia.org/wiki/Dale–Chall_readability_formula)

textstat_readability(data_corpus_irishbudgets, "Dale.Chall.old") %>% head()

textstat_readability(texts(data_corpus_irishbudgets, groups = "year"), "Dale.Chall.old")

textstat_readability(texts(data_corpus_irishbudgets, groups = "party"), measure = "Dale.Chall.old")

# 4.3 let's compare each measure

all_readability_measures <- textstat_readability(data_corpus_irishbudgets, c("Flesch", "Dale.Chall", "SMOG", "Coleman.Liau", "Fucks"))

readability_matrix <- cbind(all_readability_measures$Flesch, all_readability_measures$Dale.Chall, all_readability_measures$SMOG, all_readability_measures$Coleman.Liau, all_readability_measures$Fucks)

readability_cor <- cor(readability_matrix)
rownames(readability_cor) <- c("Flesch", "Dale-Chall", "SMOG", "Coleman Liau", "Fucks")
colnames(readability_cor) <- c("Flesch", "Dale-Chall", "SMOG", "Coleman Liau", "Fucks")
readability_cor

#------------------------------
# 5 BOOTSTRAPPING
#------------------------------
# there are packages in R that help with bootstrapping: e.g. https://cran.r-project.org/web/packages/boot/boot.pdf

# data prep: remove smaller parties (parties with only 1 document)
large_parties <- data_corpus_irishbudgets$documents %>% group_by(party) %>% tally() %>% arrange(-n) %>% filter(n > 1) %>% select(party) %>% unlist() %>% unname()
irbudgetsCorpSub <- corpus_subset(data_corpus_irishbudgets, (party %in% large_parties))

# convert corpus to df 
irbudgets_df <- irbudgetsCorpSub$documents %>% select(texts, party, year) %>% mutate(year = as.integer(year))

# Let's filter out any NAs
irbudgets_df <- na.omit(irbudgets_df)

# mean Flesch statistic per party
flesch_point <- irbudgets_df$texts %>% textstat_readability(measure = "Flesch") %>% group_by(irbudgets_df$party) %>% summarise(mean_flesch = mean(Flesch)) %>% setNames(c("party", "mean")) %>% arrange(party)

# ggplot point estimate
ggplot(flesch_point, aes(x = party, y = mean, colour = party)) +
  geom_point() +
  coord_flip() + theme_bw() + scale_y_continuous(breaks=seq(floor(min(flesch_point$mean)), ceiling(max(flesch_point$mean)), by = 2)) +
  xlab("") + ylab("Mean Fleisch Score by Party") + theme(legend.position = "none")

# We will use a loop to bootstrap a sample of texts and subsequently calculate standard errors
iters <- 10

library(pbapply)
# build function to be used in bootstrapping
boot_flesch <- function(party_data){
  N <- nrow(party_data)
  bootstrap_sample <- sample_n(party_data, N, replace = TRUE)
  readability_results <- textstat_readability(bootstrap_sample$texts, measure = "Flesch")
  return(mean(readability_results$Flesch))
}

# apply function to each party
boot_flesch_by_party <- pblapply(large_parties, function(x){
  sub_data <- irbudgets_df %>% filter(party == x)
  output_flesch <- lapply(1:iters, function(i) boot_flesch(sub_data))
  return(unlist(output_flesch))
})
names(boot_flesch_by_party) <- large_parties

# Define the standard error function
std <- function(x) sd(x)/sqrt(length(x))

# compute mean and std.errors
party_means <- lapply(boot_flesch_by_party, mean) %>% unname() %>% unlist()
party_ses <- lapply(boot_flesch_by_party, std) %>% unname() %>% unlist()

# Plot results--party
plot_dt <- tibble(party = large_parties, mean = party_means, ses = party_ses)

# confidence intervals
interval1 <- -qnorm((1-0.9)/2)   # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# ggplot point estimate + variance
ggplot(plot_dt, aes(colour = party)) +
  geom_linerange(aes(x = party, ymin = mean - ses*interval1, ymax = mean + ses*interval1), lwd = 1, position = position_dodge(width = 1/2)) +
  geom_pointrange(aes(x = party, y = mean, ymin = mean - ses*interval2, ymax = mean + ses*interval2), lwd = 1/2, position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  coord_flip() + theme_bw() + scale_y_continuous(breaks=seq(floor(min(plot_dt$mean)), ceiling(max(plot_dt$mean)), by = 2)) +
  xlab("") + ylab("Mean Fleisch Score by Party") + theme(legend.position = "none")

#------------------------------
# 6 SOPHISTICATION
#------------------------------
rm(list = ls())
#devtools::install_github("kbenoit/sophistication")
library("sophistication")

# We'll run through the example from https://github.com/kbenoit/sophistication

# Load data
data(data_corpus_sotu, package = "quanteda.corpora")

# Make snippets of 1 sentence each, then clean them
snippetData <- snippets_make(data_corpus_sotu, nsentence = 1, minchar = 150, maxchar = 250)
snippetData <- snippets_clean(snippetData)
head(snippetData)

# Sample the snippets
testData <- sample_n(snippetData, 5)

# generate n-1 pairs from n test snippets for a minimum spanning tree
snippetPairsMST <- pairs_regular_make(testData)

# generate more pairs from a larger sample of data
snippetPairsAll <- pairs_regular_make(snippetData[sample(1:nrow(snippetData), 1000), ])

# Make some "Gold" questions -- for use with CrowdFlower workers 
# default reading level is Flesch and the default difference in readability of the two snippets in the pair is the 0.1 and 0.9 quintiles
gold_questions <- pairs_gold_make(snippetPairsAll, n.pairs = 10)
