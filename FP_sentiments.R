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
Livy <- Livy %>%
  group_by(Book, Chapter) %>%
  unnest_tokens(word, Text) %>%
  mutate(linenumber = row_number())


# remove stop words
cleaned_livy <- Livy %>%
  anti_join(stop_words)

cleaned_livy %>%
  count(word, sort = TRUE) 

# add sentiment from NRC dictionary
LivySentiment <- cleaned_livy %>%
  inner_join(get_sentiments("nrc"))

# summarize sentiment by chapter
LivySentimentCount <- LivySentiment %>%
  count(Book, Chapter, sentiment) %>%
  left_join(cleaned_livy %>%
              count(Book, Chapter) %>%
              rename(n_total = n)) %>%
  mutate(pct = n / n_total)

chapter_id <- Livy %>%
  select(Book, Chapter) %>%
  distinct() %>%
  ungroup %>%
  mutate(livy_id = row_number())

LivySentimentCount <- LivySentimentCount %>%
  left_join(chapter_id) %>%
  arrange(livy_id, sentiment)%>%
  parse_number(Book, na = c("", "NA"), locale = default_locale())%>%
  
  

ggplot(LivySentimentCount, aes(livy_id, pct, group = sentiment,
                               color = sentiment)) +
  geom_smooth(se = FALSE)+
  ggtitle("Sentiments In Livy")+
  xlab("Book and Chapter")+
  ylab("Percent of Text per Chapter")+
  scale_x_continuous(seq(from = 1, to = 36, by = 5))


#Topic Modeling
#Not working
top_terms <- LivySentiment %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip()