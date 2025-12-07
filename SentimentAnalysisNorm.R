#install.packages(c("tidyverse", "tidytext", "tm")) 
library(tidyverse)
library(tidytext)
library(tm)
install.packages("tidyverse")
install.packages("tidytext")
install.packages("tm")
#read data
Fake <- read.csv("C:\\Users\\Colin\\Downloads\\FinalProjectLIS4761\\data\\Fake.csv")
True <- read.csv("C:\\Users\\Colin\\Downloads\\FinalProjectLIS4761\\data\\True.csv")

#tokenize the text column into words
fake_words <- Fake %>%
  select(text) %>%           # use the article body
  unnest_tokens(word, text)  # lowercases, no punctuation, splits into words

true_words <- True %>%
  select(text) %>%
  unnest_tokens(word, text)

# Remove common stopwords
data("stop_words")

fake_words <- fake_words %>%
  anti_join(stop_words, by = "word")

true_words <- true_words %>%
  anti_join(stop_words, by = "word")

#frequency tables
fake_freq <- fake_words %>%
  count(word, sort = TRUE) %>%
  rename(fake_count = n)

true_freq <- true_words %>%
  count(word, sort = TRUE) %>%
  rename(true_count = n)

head(fake_freq, 20)   # top 20 in Fake
head(true_freq, 20)   # top 20 in True

#difference column
comparison <- full_join(fake_freq, true_freq, by = "word") %>%
  replace_na(list(fake_count = 0, true_count = 0)) %>%
  mutate(
    total = fake_count + true_count,
    diff  = fake_count - true_count   # >0 = more common in Fake; <0 = more common in True
  ) %>%
  arrange(desc(total))

head(comparison, 20)

# Words most skewed toward Fake (with at least some overall frequency)
comparison %>%
  filter(total > 5000) %>%
  arrange(desc(diff)) %>%
  View()


# Words most skewed toward True
comparison %>%
  filter(total > 5000) %>%
  arrange(diff) %>%
  View()

#cleaning for model
# Add label and combine into one data frame
Fake <- Fake %>% mutate(is_fake = TRUE)
True <- True %>% mutate(is_fake = FALSE)

news <- bind_rows(Fake, True) %>%
  mutate(doc_id = row_number())  # unique ID per article

# Tokenize + remove stopwords again, now keeping doc_id and label
tokens <- news %>%
  select(doc_id, is_fake, text) %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  filter(str_length(word) >= 3)%>%
  anti_join(stop_words, by = "word")

# Count tokens per article
article_lengths <- tokens %>%
  count(doc_id, name = "n_tokens")

# Minimum-length rule: keep docs with at least 50 cleaned words
min_len <- 50
valid_docs <- article_lengths %>%
  filter(n_tokens >= min_len) %>%
  pull(doc_id)

# Filter to valid articles
tokens_clean <- tokens %>%
  filter(doc_id %in% valid_docs)

news_clean <- news %>%
  filter(doc_id %in% valid_docs)

#check how many docs were kept
nrow(news)       # original number of articles
nrow(news_clean) # after filtering by length

#TF-IDF table
#term Frequency counts per document 
tf_counts <- tokens_clean %>%
  count(doc_id, word, name = "tf")

# DTM with raw term frequencies (TF)
dtm_tf <- tf_counts %>%
  cast_dtm(document = doc_id, term = word, value = tf)

dtm_tf   # quick summary in console

#TF-IDF table
tfidf_tbl <- tf_counts %>%
  bind_tf_idf(term = word, document = doc_id, n = tf)
# columns: doc_id, word, tf, idf, tf_idf
#view random sample
tfidf_tbl %>% sample_n(50)

#------------------------------------------------------ Colin Part
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
  labs(x = "News Type", y = "Word Count", 
       title = "Negative and Positive Words Per Category")

#Labeling the graph
sentiment_analysis <- sentiment_analysis + guides(fill = guide_legend(title=
                                                                          "Legend")) +
  scale_fill_discrete(labels = c("Negative Words", "Positive Words")) +
  scale_x_discrete(labels = c("FALSE" = "Real News", "TRUE" = "Fake News"))
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
  labs(x = "News Type", y = "Word Count", title = "Sentiment Type Per Category")
sentiment_analysis_nrc <- sentiment_analysis_nrc + 
  guides(fill = guide_legend(title = "Legend")) +
  scale_fill_discrete(labels = c("Anger", "Disgust", "Fear", "Joy", "Sadness",
                                 "Surprise", "Trust")) +
  scale_x_discrete(labels = c("FALSE" = "Real News", "TRUE" = "Fake News"))
sentiment_analysis_nrc

#Normalizing nrc data

amounts_nrc_long$percent <- NA

for (i in 1:7) {
  amounts_nrc_long$percent[i] <- 
    amounts_nrc_long$word_count[i]/(sum(amounts_nrc_long$word_count[1:7]))
}

for (i in 8:14) {
  amounts_nrc_long$percent[i] <- 
    amounts_nrc_long$word_count[i]/(sum(amounts_nrc_long$word_count[8:14]))
}

#Normalizing positive and negative data

amounts_long$percent <- NA
for (i in 1:2) {
  amounts_long$percent[i] <-
    amounts_long$word_count[i]/(sum(amounts_long$word_count[1:2]))
}

for (i in 3:4) {
  amounts_long$percent[i] <-
    amounts_long$word_count[i]/(sum(amounts_long$word_count[3:4]))
}

#Making normalized graph for nrc data

normal_analysis_nrc <- ggplot(amounts_nrc_long,
                                 aes(x = is_fake, y = percent,
                                     fill = sentiment_type)) +
  geom_col(position = "dodge") +
  labs(x = "News Type", y = "Percent Word Count", title = "Sentiment Type Per Category Normalized")
normal_analysis_nrc <- normal_analysis_nrc + 
  guides(fill = guide_legend(title = "Legend")) +
  scale_fill_discrete(labels = c("Anger", "Disgust", "Fear", "Joy", "Sadness",
                                 "Surprise", "Trust")) +
  scale_x_discrete(labels = c("FALSE" = "Real News", "TRUE" = "Fake News"))
normal_analysis_nrc

#Making normalized graph for the basic pos/neg data
normal_analysis <- ggplot(amounts_long, aes(x = is_fake,y = percent, 
                                               fill = factor(word_sentiment))) +
  geom_col(position = "dodge") +
  labs(x = "News Type", y = "Percent Word Count", 
       title = "Negative and Positive Words Per Category Normalized")

#Labeling the graph
normal_analysis <- normal_analysis + guides(fill = guide_legend(title=
                                                                        "Legend")) +
  scale_fill_discrete(labels = c("Negative Words", "Positive Words")) +
  scale_x_discrete(labels = c("FALSE" = "Real News", "TRUE" = "Fake News"))
normal_analysis

