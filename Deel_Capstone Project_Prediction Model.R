
#cleans environment
rm(list = ls())

#loads needed packages and sets needed parameters
library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)
library(tidytext)
library(doMC)
library(foreach)
library(tm)
registerDoMC(7)
getDoParWorkers()

#sets working directory and seed
setwd("C:/Users/jdeel/Documents/Training/JHU - Coursera/Capstone Project/final")
set.seed(1)


## Data Loading and Processing

#reads in data
usnews <- readLines("en_US/en_US.news.txt")
ustwitter<- readLines("en_US/en_US.twitter.txt")
usblogs <- readLines("en_US/en_US.blogs.txt")

#joins each data source into one corpus
fullsample <- list(usnews, usblogs, ustwitter) %>% set_names(c("n", "b", "t"))
fullsample %<>% map_df(~ tibble(text = ., entry = 1:length(.)), .id = "source")

#creates workable random sample using 15% of the full text sample
smallsample <- sample_frac(fullsample, .15)

#removes original data objects to save memory in program
rm(fullsample, ustwitter, usnews, usblogs)

#generates list of bigrams, trigrams, and quadgrams with associated frequencies
ngramslist <- foreach (i = 2:4) %dopar% {
  unnest_tokens(smallsample, gram, text, token = "ngrams", n = i) %>%
    mutate(next_word = gsub(".*( .*$)", " \\1", gram),
           gram = gsub("(.*) .*$", "\\1", gram) ) %>%
    group_by(gram) %>%
    count(next_word, sort = T) %>%
    mutate(total_next = sum(n),
           coverage = cumsum(n / total_next)) %>%
    filter(coverage < .500000001)
}

#saves ngrams list for use later
saveRDS(ngramslist, "ngramslist.RDS")
saveRDS(ngramslist, "Deel_DataScienceCapstone_ShinyApp/ngramslist.RDS")

#stores code for reading in ngrams later
ngramslist <- readRDS("ngramslist.RDS")


## Model Creation

#creates model algorithm
txtpredict <- function(st) {
  #sources input text string and trims down to 4 words if necessary
  st %<>% tolower() %>% str_extract_all("[a-z]+'?[a-z]*") %>% unlist()
  num_words <- length(st)
  if (num_words > 3) {
    st <- st[-(1:(num_words - 3))]
    num_words <- 3
  }

  st_guess <- paste(st, collapse = " ")
  
  #looks up input text in ngramslist and returns most frequent "next words"
  guess <- ngramslist[[num_words]] %>% filter(gram == st_guess) %>%
    ungroup() %>%
    mutate(prop = n / total_next,
           coverage = cumsum(prop)) %>%
    select(next_word, prop)

  #uses backoff model to retreat to next input word if initial last word of input returns no frequent "next words"
  while (nrow(guess) < 1) {
    num_words <- num_words - 1
    st <- st[-1]
    if (length(st) > 0) {
      st_guess <- paste(st, collapse = " ")
      guess <- ngramslist[[num_words]] %>% filter(gram == st_guess) %>%
        ungroup() %>%
        mutate(prop = n / total_next) %>%
        select(gram, next_word, prop)
    }
    
    st_guess <- paste(st, collapse = " ")
     guess <- ngramslist[[num_words]] %>% filter(gram == st_guess) %>%
       ungroup() %>%
       mutate(prop = n / total_next) %>%
       select(gram, next_word, prop)
  }
  
  #returns list of most likely "next words" sorted by likelihood, and coverage score of each word
  return(guess)}


#saves txtpredict function
dump("txtpredict", file="txtpredict.R")

#sources in txtpredict function for use later
source("txtpredict.R")

#deploys app to Shiny Apps server
options(rsconnect.check.certificate = FALSE)
rsconnect::deployApp("Deel_DataScienceCapstone_ShinyApp")

