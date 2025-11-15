#install.packages(c("tidyverse", "tidytext")) 
library(tidyverse)
library(tidytext)
library(stringr)

Fake <- read_csv("data/Fake.csv")
True <- read_csv("data/True.csv")

# exploration
word_counts_F <- str_count(Fake$text, "\\S+")
max(word_counts_F, na.rm = T)
min(word_counts_F, na.rm = T) # will need to remove articles with little/missing body
mean(word_counts_F, na.rm = T)

word_counts_T <- str_count(True$text, "\\S+")
max(word_counts_T, na.rm = T)
min(word_counts_T, na.rm = T)
mean(word_counts_T, na.rm = T)


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
  head(20)

# Words most skewed toward True
comparison %>%
  filter(total > 5000) %>%
  arrange(diff) %>%
  head(20)
