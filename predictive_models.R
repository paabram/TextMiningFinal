library(caret)
library(naivebayes)
library(glmnet)
library(dplyr)
library(readr)
library(e1071)

# read in news CSV and label object
news <- read.csv("data/news_tfidf.csv", row.names = 1)
y <- readRDS("data/labels.rds")

# attach labels to df
news$is_fake <- as.factor(y)

cat("Class balance:\n")
print(table(news$is_fake))

# REMOVE NEAR-ZERO VARIANCE FEATURES
# Keep label safe, only clean TF-IDF columns
predictors <- news %>% select(-is_fake)

nzv <- nearZeroVar(predictors)
predictors_reduced <- predictors[, -nzv]

news_reduced <- cbind(predictors_reduced, is_fake = news$is_fake)

cat("\nRemoved NZV features:", length(nzv), "\n")
cat("Remaining columns:", ncol(news_reduced), "\n")

# TRAIN / TEST SPLIT
set.seed(123)

train_index <- createDataPartition(news_reduced$is_fake, p = 0.8, list = FALSE)

train <- news_reduced[train_index, ]
test  <- news_reduced[-train_index, ]

x_train <- train %>% select(-is_fake)
y_train <- train$is_fake

x_test  <- test %>% select(-is_fake)
y_test  <- test$is_fake

# Matrix versions for glmnet & SVM
x_train_mat <- as.matrix(x_train)
x_test_mat  <- as.matrix(x_test)

# LOGISTIC REGRESSION (GLMNET) 
log_model <- cv.glmnet(
  x_train_mat, y_train,
  family = "binomial",
  alpha = 0,
  type.measure = "class"
)

log_pred <- predict(log_model, x_test_mat, type = "class")
log_pred <- factor(log_pred, levels = c(FALSE, TRUE))

log_cm <- confusionMatrix(as.factor(log_pred), y_test)
cat("\n================ LOGISTIC REGRESSION RESULTS ================\n")
print(log_cm) # 92.76% accuracy

# NAIVE BAYES 
nb_model <- naive_bayes(is_fake ~ ., data = train)
nb_pred  <- predict(nb_model, newdata = x_test)

nb_cm <- confusionMatrix(nb_pred, y_test)
cat("\n==================== NAIVE BAYES RESULTS ====================\n")
print(nb_cm) # 92.07% accuracy

# SUPPORT VECTOR MACHINE
svm_model <- svm(
  x = x_train_mat,
  y = y_train,
  kernel = "linear",
  probability = TRUE
)
svm_pred <- predict(svm_model, x_test_mat)

svm_cm <- confusionMatrix(svm_pred, y_test)
cat("\n======================= SVM RESULTS ==========================\n")
print(svm_cm) # 92.07% accuracy


# FEATURE IMPORTANCE (FROM REGRESSION)

coefs <- coef(log_model)
coefs_df <- data.frame(
  term = labels(coefs),
  weight = as.vector(coefs)
)

# Strongest positive weights point toward is_fake = TRUE (per output of log_model$levels)
top_fake <- coefs_df %>%
  arrange(desc(weight)) %>%
  head(20) %>% 
  select(1,3)
colnames(top_fake) <- c("term", "weight")

# Strongest negative weights impact is_fake = FALSE (realnews)
top_real <- coefs_df %>%
  arrange(weight) %>%
  head(20) %>% 
  select(1,3)
colnames(top_real) <- c("term", "weight")

cat("\n=============== TOP 20 WORDS PREDICTING FAKE NEWS =============\n")
print(top_fake)

cat("\n=============== TOP 20 WORDS PREDICTING REAL NEWS =============\n")
print(top_real)

