library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
# date time
library(lubridate)
library(zoo)

options(stringsAsFactors = FALSE)

# sentiment analysis
sentiments

tweet_data_db <- search_tweets(q = "#marchepourleclimat", n = 180000,
                               # lang = "fr",
                               include_rts = FALSE)



# create new df with just the tweet texts & usernames
tweet_data <- data.frame(date_time = tweet_data_db$created_at,
                         username = tweet_data_db$name,
                         tweet_text = tweet_data_db$text)

# flood start date sept 13 - 24 (end of incident)
start_date <- as.POSIXct('2018-09-11 00:00:00')
end_date <- as.POSIXct('2018-10-15 00:00:00')

# cleanup
final_tweets <- tweet_data %>%
  mutate(date_time = as.POSIXct(date_time, format = "%a %b %d %H:%M:%S +0000 %Y")) %>%
  filter(date_time >= start_date & date_time <= end_date ) %>%
  mutate(tweet_text = gsub("http://*|https://*)", "", tweet_text))

data("stop_words")

# get a list of words
final_tweet_clean <- final_tweets %>%
  dplyr::select(tweet_text) %>%
  unnest_tokens(word, tweet_text) %>%
  anti_join(get_stopwords(language="fr", source="stopwords-iso")) %>%
  filter(!word %in% c("rt", "t.co"))
## Joining, by = "word"

# plot the top 15 words -- notice any issues?
final_tweet_clean %>%
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

# join sentiment classification to the tweet words
bing_word_counts <- final_tweet_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
## Joining, by = "word"

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


# cleanup
final_tweets_2013 <- tweet_data %>%
  mutate(date_time = as.POSIXct(date_time, format = "%a %b %d %H:%M:%S +0000 %Y")) %>%
  mutate(tweet_text = gsub("http://*|https://*)", "", tweet_text),
         month = as.yearmon(date_time))

# get a list of words
final_tweet_clean_2013 <- final_tweets_2013 %>%
  dplyr::select(tweet_text, month) %>%
  unnest_tokens(word, tweet_text) %>%
  anti_join(get_stopwords(language="fr", source="stopwords-iso")) %>%
  filter(!word %in% c("rt", "t.co"))
## Joining, by = "word"


# plot the top 15 words -- notice any issues?
final_tweet_clean_2013 %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in a year's worth of tweets")
## Selecting by n


# join sentiment classification to the tweet words
bing_sentiment_2013 <- final_tweet_clean_2013 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, month, sort = TRUE) %>%
  group_by(sentiment) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  
  group_by(month, sentiment) %>%
  top_n(n = 5, wt = n) %>%
  # create a date / sentiment column for sorting
  mutate(sent_date = paste0(month, " - ", sentiment)) %>%
  arrange(month, sentiment, n)
## Joining, by = "word"


bing_sentiment_2013$sent_date <- factor(bing_sentiment_2013$sent_date,
                                        levels = unique(bing_sentiment_2013$sent_date))


# group by month and sentiment and then plot top 5 words each month
bing_sentiment_2013 %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sent_date, scales = "free_y", ncol = 2) +
  labs(title = "Sentiment during the 2013 flood event by month.",
       y = "Number of Times Word Appeared in Tweets",
       x = NULL) +
  coord_flip()

