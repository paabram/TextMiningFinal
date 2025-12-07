library(tidyverse)
library(tidytext)
library(tm)
library(qdap)

#read data
Fake <- read_csv("data/Fake.csv")
True <- read_csv("data/True.csv")

# add label and join datasets
Fake <- Fake %>% mutate(is_fake = TRUE)
True <- True %>% mutate(is_fake = FALSE)

news <- bind_rows(Fake, True) %>%
  mutate(doc_id = row_number())

# take sample of articles for memory usability
news <- news[sample(nrow(news), 1500), ] # if not storing to csv this value can go up to at least 4000
summary(news)

# add article length column
news$nwords <- str_count(news$text, "\\S+")
# remove exceedingly long/short articles
max_words <- mean(news$nwords, na.rm=T) + (3 * sd(news$nwords, na.rm=T))
dim(news)
news <- news %>% filter(nwords >= 10 & nwords <= max_words)
dim(news)

# form article corpus
news <- news %>% select(doc_id, text, title, date, subject, is_fake, nwords) # reorder columns
news_source <- DataframeSource(news)
news_corpus <- VCorpus(news_source)

# clean corpus
clean_corpus <- function(corpus) {
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  remove_quotes <- content_transformer(function(x) {
    gsub("[“”\"\'‘’`]", "", x) # Removes double quotes, single quotes, and typographic quotes
  })
  corpus <- tm_map(corpus, remove_quotes)
  # remove numbers
  corpus <- tm_map(corpus, removeNumbers)
  # Transform to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # remove contractions
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  # Remove stopwords
  corpus <- tm_map(corpus, removeWords, words = stopwords("en"))
  # Strip whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
news_corpus <- clean_corpus(news_corpus)

# cast to dtm
news_dtm <- DocumentTermMatrix(news_corpus)
# weight tf-idf
news_tfidf <- weightTfIdf(news_dtm)

# save data and labels for model
X <- as.matrix(news_tfidf)
# get is_fake column
doc_ids <- as.numeric(rownames(X))
y <- news$is_fake[match(doc_ids, news$doc_id)]

saveRDS(y, "data/labels.rds")
write.csv(X, "data/news_tfidf.csv")
