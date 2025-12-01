#install.packages(c("tidyverse", "tidytext", "tm")) 
library(tidyverse)
library(tidytext)
library(tm)
#read data
Fake <- read_csv("data/Fake.csv")
True <- read_csv("data/True.csv")

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

# drop exceedingly rare tokens
tokens_clean <- tokens_clean %>%
  add_count(word, name="global_count") %>%
  filter(global_count >= 5)
# drop exceedingly common terms
doc_count <- tokens_clean %>% distinct(doc_id, word) %>% count(word, name="df")
n_docs <- nrow(news_clean)

tokens_clean <- tokens_clean %>%
  inner_join(doc_count, by="word") %>%
  filter(df / n_docs <= 0.7)

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

dim(tfidf_tbl)
length(unique(tfidf_tbl$word)) #still over 100k unique words
# add doc frequency to table
tfidf_tbl$df <- 1 / tfidf_tbl$idf
extra_stopwords <- unique(filter(tfidf_tbl, df >= 0.7)$word)

# drop individual tf/idf columns
tfidf_long <- tfidf_tbl %>% select(-c(tf, idf))
# convert to single row per doc
tfidf_sparse <- tfidf_long %>%
  cast_sparse(doc_id, word, tf_idf)

