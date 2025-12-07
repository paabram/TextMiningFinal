library(stringr)
library(dplyr)

Fake <- read.csv("data/Fake.csv")
True <- read.csv("data/True.csv")

# exploration
word_counts_F <- str_count(Fake$text, "\\S+")
max(word_counts_F, na.rm = T)
min(word_counts_F, na.rm = T) # will need to remove articles with little/missing body
mean(word_counts_F, na.rm = T)
sd(word_counts_F, na.rm=T) * 4

word_counts_T <- str_count(True$text, "\\S+")
max(word_counts_T, na.rm = T)
min(word_counts_T, na.rm = T)
mean(word_counts_T, na.rm = T)

# add label and join datasets
Fake <- Fake %>% mutate(is_fake = TRUE)
True <- True %>% mutate(is_fake = FALSE)

news <- bind_rows(Fake, True) %>%
  mutate(doc_id = row_number())

# convert date column to object
news$date <- as.Date(news$date, "%B %d, %Y")
