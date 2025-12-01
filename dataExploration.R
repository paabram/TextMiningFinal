library(stringr)

Fake <- read_csv("data/Fake.csv")
True <- read_csv("data/True.csv")

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
