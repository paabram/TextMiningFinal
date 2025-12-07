library(textdata)
#Getting sentiments
positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")
negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

#Table of positive and negative words in fake/true
amount_pos <- tokens_clean %>%
  semi_join(positive) %>%
  count(is_fake, name = "positive_words")
amount_neg <- tokens_clean %>%
  semi_join(negative) %>%
  count(is_fake, name = "negative_words")

#Merge tables to see positive and negative words in each category
amounts <- merge(amount_pos, amount_neg, by = "is_fake")

#Make it long format
amounts_long <- pivot_longer(amounts, positive_words:negative_words,
                             names_to = "word_sentiment",
                             values_to = "word_count")

#Graphing it
sentiment_analysis <- ggplot(amounts_long, aes(x = is_fake,y = word_count,
                                               fill = factor(word_sentiment))) +
  geom_col(position = "dodge") +
  labs(x = "Is Fake", y = "Word Count",
       title = "Negative and Positive Words Per Category")

#Labeling the graph
sentiment_analysis <- sentiment_analysis + guides(fill = guide_legend(title=
                                                                          "Legend")) +
  scale_fill_discrete(labels = c("Negative Words", "Positive Words"))
sentiment_analysis

#Now doing sentiment analysis on fear, anger, trust, sadness, disgust,
#surprise, and joy

fear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")
anger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")
trust <- get_sentiments("nrc") %>%
  filter(sentiment == "trust")
sadness <- get_sentiments("nrc") %>%
  filter(sentiment == "sadness")
disgust <- get_sentiments("nrc") %>%
  filter(sentiment == "disgust")
surprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")
joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

#Matching all sentiments to fake and true articles

amount_fear <- tokens_clean %>%
  semi_join(fear) %>%
  count(is_fake, name = "fear_words")
amount_anger <- tokens_clean %>%
  semi_join(anger) %>%
  count(is_fake, name = "anger_words")
amount_trust <- tokens_clean %>%
  semi_join(trust) %>%
  count(is_fake, name = "trust_words")
amount_sadness <- tokens_clean %>%
  semi_join(sadness) %>%
  count(is_fake, name = "sadness_words")
amount_disgust <- tokens_clean %>%
  semi_join(disgust) %>%
  count(is_fake, name = "disgust_words")
amount_surprise <- tokens_clean %>%
  semi_join(surprise) %>%
  count(is_fake, name = "surprise_words")
amount_joy <- tokens_clean %>%
  semi_join(joy) %>%
  count(is_fake, name = "joy_words")

#Merging all tables
amounts_nrc <- merge(amount_fear, amount_anger, by = "is_fake")
amounts_nrc <- merge(amounts_nrc, amount_trust, by = "is_fake")
amounts_nrc <- merge(amounts_nrc, amount_sadness, by = "is_fake")
amounts_nrc <- merge(amounts_nrc, amount_disgust, by = "is_fake")
amounts_nrc <- merge(amounts_nrc, amount_surprise, by = "is_fake")
amounts_nrc <- merge(amounts_nrc, amount_joy, by = "is_fake")

#Make amounts_nrc long
amounts_nrc_long <- pivot_longer(amounts_nrc, fear_words:joy_words,
                                 names_to = "sentiment_type",
                                 values_to = "word_count")

#Graphing NRC sentiments
sentiment_analysis_nrc <- ggplot(amounts_nrc_long,
                                 aes(x = is_fake, y = word_count,
                                     fill = sentiment_type)) +
  geom_col(position = "dodge") +
  labs(x = "Is Fake", y = "Word Count", title = "Sentiment Type Per Category")
sentiment_analysis_nrc <- sentiment_analysis_nrc +
  guides(fill = guide_legend(title = "Legend")) +
  scale_fill_discrete(labels = c("Anger", "Disgust", "Fear", "Joy", "Sadness",
                                 "Surprise", "Trust"))
sentiment_analysis_nrc
