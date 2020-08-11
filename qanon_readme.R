# This script intends to estimate certain occurences of categories in a dataset of german tweets referencing the
#conspiracy theory of QAnon with the readme2-package (https://github.com/iqss-research/readme-software). 
# The categories use to make the estimation are: 0: no match, 1: critizising Qanon, 2: affirmative of Qanon

rm(list=ls())
library(mongolite)
library(jsonlite)
library(tidyverse)
library(magrittr)

library(tensorflow)
library(readme)

# Read from MongoDB
db <- mongo(collection = "tweet", db = "Q", url = "mongodb://localhost", verbose = FALSE, options = ssl_options())
tweets_full <- db$find()
#Disconnect
rm(db)
gc()

#Just take a subset of the data
tweets <- tweets_full %>%
  select(datetime, text, code)

#Parse the date string into a datetime in order to do a timeline analysis (with magrittr)
# tweets %<>%
#   mutate(
#     datetime = datetime %>%
#       parse_datetime(format = ' %Y-%m-%d %H:%M:%S') # Parse date.
#   )

#Select the tweets which have a code
tweets_coded <- tweets %>%
  filter(!is.na(code))

#Split coded tweets into a test and a training set
rnd_train <- sample(c(0,1), nrow(tweets_coded), replace = T)
tweets_coded$trainingset <- c(rnd_train)
#length(which(tweets_coded$trainingset == 1))
#length(which(tweets_coded$trainingset == 0))

loadVecs <- function(path){
  wordVecs_corpus <- data.table::fread(path)
  wordVecs_keys <- wordVecs_corpus[[1]]## first row is the name of the term
  wordVecs_corpus <- as.matrix (  wordVecs_corpus[,-1] )  #
  row.names(wordVecs_corpus) <- wordVecs_keys
  wordVecs <- wordVecs_corpus
  rm(wordVecs_corpus)
  rm(wordVecs_keys) ## Remove the original loaded table to save space 
  return(wordVecs)
}



## Generate a word vector summary for each document
#german:
wordVecs <-loadVecs('Models/Word\ embeddings/deepset.ai.german.wikipedia.glove.txt')
# english: 
#wordVecs <- as.matrix(read.table('Models/Word\ embeddings/glove.6B.200d.txt', header=F,sep=" "))
wordVec_summaries = undergrad(documentText = cleanme(tweets_coded$text), wordVecs = wordVecs)
#wordVec_summaries = undergrad(documentText = cleanme(tweets_coded$text), wordVecs = NULL)

# First attempt with english GloVe:
# will be bullshit with default english dictionary.. -> just around 30% matched:
# Number of documents: 1104
# Number of unique word stems: 5054
# Number of word vector terms: 400000
# First attempt: Matched 1494 of 5054 (29.6%) terms to word vectors
# Second attempt: Matched 1698 of 5054 (33.6%) terms to word vectors

# Next attempt with german GloVe & 597 coded tweets (8.8.2020):
# Number of documents: 597
# Number of unique word stems: 3698
# Number of word vector terms: 1309281
# First attempt: Matched 2610 of 3698 (70.6%) terms to word vectors
# Second attempt: Matched 2762 of 3698 (74.7%) terms to word vectors
# Results:
# Estimate:  
#   0         1         2 
# 0.2509273 0.1644557 0.5846169 
# Actual:
# 0          1          2 
# 0.24723247 0.08856089 0.66420664 

#Round 2:
# Estimate
# 0          1          2 
# 0.26926204 0.09087922 0.63985874 
# Actual
# 0         1         2 
# 0.2384106 0.0794702 0.6821192 

# Round 3:
# Estimate
# 0         1         2 
# 0.2553142 0.1212680 0.6234178 
# Actual:
# 0          1          2 
# 0.22866894 0.09897611 0.67235495 

# Estimate category proportions
set.seed(2223) # Set a seed if you choose
readme.estimates <- readme(dfm = wordVec_summaries , labeledIndicator = tweets_coded$trainingset, categoryVec = tweets_coded$code)

# Output proportions estimate
readme.estimates$point_readme

# Compare to the truth
table(tweets_coded$code[tweets_coded$trainingset == 0])/sum(table((tweets_coded$code[tweets_coded$trainingset == 0])))
table(tweets_coded$code[tweets_coded$trainingset == 0])
sum(table((tweets_coded$code[tweets_coded$trainingset == 0])))

# -----------------------------------------------------------

# Apply to whole dataset
tweets_all <- tweets %>%
  mutate(trainingset = ifelse(!is.na(code), 1, ifelse(is.na(code), 0, NA)))

wordVec_summaries_all = undergrad(documentText = cleanme(tweets_all$text), wordVecs = wordVecs)

set.seed(2223) # Set a seed if you choose
readme.estimates <- readme(dfm = wordVec_summaries_all , 
                           labeledIndicator = tweets_all$trainingset, 
                           categoryVec = tweets_all$code,
                           verbose = T,
                           diagnostics = T)

# Output proportions estimate
readme.estimates$point_readme

# Overall estimates (11'363 Tweets, 597 coded, 10766 uncoded)
# 0 N/A        1 Critique     2 Support
# 0.2713525    0.1571725      0.5714749 

