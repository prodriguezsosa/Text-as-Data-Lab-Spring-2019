# TA: Leslie Huang
# Course: Text as Data
# Date: 4/26/2018
# Recitation 13: Special Topics I

# First... Course Assessments!
# Link: https://nyu.qualtrics.com/jfe/form/SV_3mGvDResIGQDtGZ


setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/W12_04_26_18")

#install.packages("bursts")
library(bursts)
library(quanteda)
library(readtext)

# 1  Loading bursty function: a repurposing of some guts of kleinberg()

bursty <- function(word = "sioux", DTM, date) {
  word.vec <- DTM[, which(colnames(DTM) == word)]
  if(length(word.vec) == 0) {
    print(word, " does not exist in this corpus.")
  } 
  else {
    word.times <- c(0,which(as.vector(word.vec)>0))
    
    kl <- kleinberg(word.times, gamma = 0.5)
    kl$start <- date[kl$start+1]
    kl$end <- date[kl$end]
    max_level <- max(kl$level)
    
    plot(c(kl$start[1], kl$end[1]), c(1,max_level),
         type = "n", xlab = "Time", ylab = "Level", bty = "n",
         xlim = c(kl$start[1], kl$end[1]), ylim = c(1, max_level),
         yaxt = "n")
    axis(2, at = 1:max_level)
    
    for (i in 1:nrow(kl)) {
      if (kl$start[i] != kl$end[i]) {
        arrows(kl$start[i], kl$level[i], kl$end[i], kl$level[i], code = 3, angle = 90,
               length = 0.05)
      } 
      else {
        points(kl$start[i], kl$level[i])
      }
    }
    
    print(kl)
  }
    #note deviation from standard defaults bec don't have that much data
}


# 2 Let's use this on the Conservative and Labour manifestos

setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/W6_02_27_18/cons_labour_manifestos")
list.files()

# Loading data

manifesto <- readtext("*.txt", docvarsfrom=c("filenames"))

manifesto_corpus <- corpus(manifesto)

docvars(manifesto_corpus)$date <- as.numeric(gsub("[[:alpha:]]","",docvars(manifesto_corpus)$docvar1))

manifesto_dfm <- dfm(manifesto_corpus)

# 3.1 Evaluating the burstiness of several key words

bursty("thatcher", manifesto_dfm, docvars(manifesto_corpus)$date)

bursty("churchill", manifesto_dfm, docvars(manifesto_corpus)$date)

bursty("argentina", manifesto_dfm, docvars(manifesto_corpus)$date)

# Suggestions?

# 3.2 Native American Treaties

setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/Homeworks/HW3")
list.files(path = "treaties/")
treaties <- readtext("treaties/*.txt", docvarsfrom=c("filenames"))
treaties_corpus <- corpus(treaties)

# grab the treaty dates
cases <- read.csv("treaties/universecases.csv")
date <- as.Date(as.character(cases$Date[1:365]), "%m-%d-%Y")

# set as docvars
docvars(treaties_corpus)$Date <- date

# Create DFM
treaties_dtm <- dfm(treaties_corpus)

bursty("sioux", treaties_dtm, date)

