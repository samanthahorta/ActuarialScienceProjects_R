# Twitter Text Mining 

library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)
library(tidyr)
library(widyr)


api_key <- ""
api_secret_key <- ""
access_token <- ""
access_token_secret <- ""
app_name <- ""

token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)


topic_tweets <- search_tweets(q = "#topic",
                                  lang = "es",
                                include_rts = FALSE,
                                type ="recent")
?search_tweets
head(topic_tweets$text)

# Remueve elementos http manualmente
topic_tweets$stripped_text <- gsub("http.*","",  topic_tweets$text)
topic_tweets$stripped_text <- gsub("https.*","", topic_tweets$stripped_text)

# Remover puntuación, convertir a minúscula cada tweet
topic_tweets_clean <- topic_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)


# Gráfica de palabras top (15) 
topic_tweets_clean %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")



# Tidytext package lista de stop words
data("stop_words")
head(stop_words)
nrow(topic_tweets_clean)

# Remover stop words de las listas de palabras
cleaned_tweet_words <- topic_tweets_clean %>%
  anti_join(stop_words)
nrow(cleaned_tweet_words)



# Gráfico de palabras top 15 
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Count of unique words found in tweets",
       subtitle = "Stop words removed from the list")

# Remover puntuación, convertir a minúsculas para cada tweet 
topic_tweets_paired_words <- topic_tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

topic_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

topic_tweets_separated_words <- topic_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

topic_tweets_filtered <- topic_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Contabilizador
topic_words_counts <- topic_tweets_filtered %>%
  count(word1, word2, sort = TRUE)

head(topic_words_counts)


# Gráfica 
topic_words_counts %>%
  filter(n >= 2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - Topic",
       subtitle = "Text mining twitter data ",
       x = "", y = "")
