# TA: Leslie Huang
# Course: Text as Data
# Date: 1/25/2018
# This lab is adapted from Patrick Chester (thanks Patrick!)

# Before you start:
# Download the file "national_clinton_trump_6_20_2016.csv" from the repo


################
# 1 Setting up #
################

# 1.1 Clearing environment
rm(list = ls())

# 1.2 Working directory

# Where am I?
getwd()

# Setting working directory
setwd("/Users/lesliehuang/Text-as-Data-Lab-Spr2018/W1_01-25-18")

# or even
setwd(getwd())

# 1.3 Installing and loading some useful packages
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("xtable")

library(dplyr)
library(ggplot2)
library(xtable)

# Loading multiple packages
libraries <- c("foreign", "stargazer")
lapply(libraries, require, character.only=TRUE)


# 1.5 Managing dependencies

# If you want to ensure that your code will run with specific package dependencies, I recommend using a dependency manager for R called packrat so that you can specify which version of libraries that you use.
# Find out about setting up packrat here: https://rstudio.github.io/packrat/walkthrough.html

# For R packages that are actively being developed, functions and function names can change and this can break your code if you update the package but not your code! (More about this next week.)

# 1.6 Loading data
polling_data  <- read.csv("national_clinton_trump_6_20_2016.csv", stringsAsFactors = FALSE)

#######################
# 2 Working with data #
#######################

# 2.1 How to subset dataframes

# A) Get column with dollar sign operator
polling_data$Pollster

# B) Matrix identifier: df[rowname, colname]
polling_data[, "Pollster"]

# That was pretty impossible to read in the console, let's try this:
View(polling_data[, c("Pollster", "Number.of.Observations")])

# C) dplyr

# dplyr is a very powerful package with intuitive commands for subsetting, selecting, and transforming your data
# Read about it here: https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html

# Using pipe notation
polling_data %>% select(Pollster)
polling_data %>% select(Pollster, Number.of.Observations)

# Alternative syntax
select(polling_data, Pollster, Number.of.Observations)

# 2.2 How to locate row(s) in a data frame

# A) Dollar sign operator
polling_data$Number.of.Observations[1] # Returns the first row of the data frame in the specified column

polling_data$Number.of.Observations[1:5] # Returns the first 5 rows of the data frame in the specified column

polling_data$Number.of.Observations[polling_data$Pollster == "Quinnipiac"] # Returns all rows where Pollster = Quinnipiac

# B) Column name
polling_data[1, "Number.of.Observations"] 
polling_data[1:5, "Number.of.Observations"] 
polling_data[polling_data$Pollster == "Quinnipiac","Number.of.Observations"] 

# C) dplyr
# Pipe syntax
polling_data %>% slice(1) %>% select(Number.of.Observations) 
polling_data %>% slice(1:5) %>% select(Number.of.Observations)
polling_data %>% filter(Pollster == "Quinnipiac") %>% select(Number.of.Observations)

# Alternate syntax

select(filter(polling_data, Pollster == "Quinnipiac"), Number.of.Observations)

# 2.3 Creating new variables (columns) in a data frame

# A) Dollar sign operator
polling_data$net_clinton_a  <- polling_data$Clinton - polling_data$Trump

# B) Matrix identifier
polling_data[, "net_clinton_b"]  <- polling_data[, "Clinton"] - polling_data[, "Trump"]


# C) dplyr
# Pipe syntax
polling_data  <- polling_data %>% mutate(net_clinton_c = Clinton - Trump)

# Alternate syntax
polling_data  <- mutate(polling_data, net_clinton_d = Clinton - Trump)

# Are these variables equivalent to one another?
all.equal(polling_data$net_clinton_a,polling_data$net_clinton_b)  
all.equal(polling_data$net_clinton_b,polling_data$net_clinton_c) 
all.equal(polling_data$net_clinton_c,polling_data$net_clinton_d)  

# Yes. Yes they are.

# 2.4 Removing columns
polling_data$net_clinton_b  <- NULL

# Using dplyr
polling_data  <- subset(polling_data, select = -c(net_clinton_d))

# 2.5 Summarizing Data

# A) Identifying the number of rows and columns and getting their names
nrow(polling_data)
ncol(polling_data)

colnames(polling_data)
rownames(polling_data) # no row names were set

# B) Getting a quick summary of your data
summary(polling_data)

str(polling_data) # Structure not string!

glimpse(polling_data)

# C) Summarizing variables by another variable in a table
table1  <- polling_data %>% group_by(Pollster) %>% summarise(mean(net_clinton_a))
# this gives mean net_clinton_a per pollster

View(table1)

# D) Summarizing a variable with a histogram

# Basic R graphics
hist(polling_data$net_clinton_a)

# ggplot2 graphics
plot1  <- ggplot(aes(net_clinton_a), data = polling_data) + geom_histogram(bins = 15) + theme_light()

plot1

# 2.6 Exporting data

# Exporting table to CSV
write.csv(table1,file = "table1.csv")

# Creating LaTeX table
xtable(table1,caption = "Average Clinton Polling Advantage by Polling Firm")

stargazer(table1, summary = FALSE)

# Exporting graph to pdf
pdf(width = 4, height = 3, "plot1.pdf")
plot1
dev.off()

#########################
# 3 Control flow and functions #
#########################

# 3.1 For Loops

for(col_name in names(polling_data)){ # A loop that identifies and stores variables that contain characters
  if(is.character(polling_data[, col_name]) ) {
    print(col_name)
  }
}

# Python users: R cannot return multiple values from a function -- you will have to return a list of the values you want to return. There is also no equivalent to "yield"  


# 3.2 Apply functions (with regex)

names(polling_data)  <- sapply(names(polling_data), function(i) {
  i <- gsub("\\.", "_", i) # Replaces all instances of "." with an "_"
  i <- gsub("__", "_", i) # Replaces all instances of "__" with "_"
} )


# Python users: The function passed to sapply() is the equivalent of a lambda function

# 3.3 User written functions

# Calculates the absolute distance between two vectors
calculate_abs_distance <- function(vec1, vec2) { 
  sqrt(sum(abs(vec1 - vec2)))
}


x  <- rnorm(10) # Creates a vector of random normally distributed numbers
y  <- x*2 + 3

calculate_abs_distance(x,y)

#########################
# 4 Finishing up #
#########################


# 4.1 Save workspace after running it -- all objects, functions, etc  (e.g. if you have run something computationally intensive and want to save the object for later use)
# Similar to pickle() in Python

save.image("workspace.RData")

# 4.2 Pick up where you left off (but note that the workspace does not include packages. You need packrat for that)

rm(list = ls())

load("workspace.RData")

####################
# 4 Free Resources #
####################

# UCLA
# http://www.ats.ucla.edu/stat/r/

# Rbloggers
# https://www.r-bloggers.com/how-to-learn-r-2/

# Data Camp
# https://www.datacamp.com/

# Swirl
# http://swirlstats.com/

# If you have a question, it's probably been asked before on StackOverflow/StackExchange!

