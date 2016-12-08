library(tidyverse)
library(lubridate)
library(stringr)
library(tidytext)
library(broom)
library(scales)
library(dplyr)
library(ggplot2)

theme_set(theme_bw())

# get text
Livy <- read_csv("All_Livy.csv")%>%
  mutate(Book = parse_number(Book))

# convert to tokens
Livy_tokens <- Livy %>%
  group_by(Book) %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words)

# convert tokens to count of words in each book
Livy_count <- Livy_tokens %>%
  count(Book, word) %>%
  na.omit()

# convert to document term matrix
Livy_dtm <- cast_dtm(Livy_count, Book, word, n)

# estimate topic model
library(topicmodels)

livy_tm <- LDA(Livy_dtm, k = 4, control = list(seed = 1234))

# top terms for each topic
livy_tm_td <- tidy(livy_tm)

top_terms <- livy_tm_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms
  
