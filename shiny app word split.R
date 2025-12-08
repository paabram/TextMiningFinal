library(shiny)
library(readr)
library(tm)
library(tidyverse)
library(stringr)

# --- Load pretrained objects (must exist in global env or load from .RData) ---
# valid_words: character vector of tokens used by training model
# idf: named numeric vector, same order as valid_words
# model: your trained classifier (glmnet, SVM, etc.)

# load("saved_model_objects.RData")  # if applicable


# -----------------------
# Cleaning function
# -----------------------
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  
  remove_quotes <- content_transformer(function(x) {
    gsub("[“”\"\'‘’`]", "", x)
  })
  corpus <- tm_map(corpus, remove_quotes)
  
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, removeWords, words = stopwords("en"))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

model <- svm_model

# -----------------------
# UI
# -----------------------
ui <- fluidPage(
  titlePanel("Fake vs Real News Classifier"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a text file (.txt)", accept = ".txt")
    ),
    
    mainPanel(
      h3("Prediction"),
      verbatimTextOutput("prediction"),
      
      h3("Top Words (after cleaning)"),
      tableOutput("topwords")
    )
  )
)


# -----------------------
# SERVER
# -----------------------
server <- function(input, output) {
  
  output$prediction <- renderText({
    req(input$file)
    
    # Read file
    text <- read_file(input$file$datapath)
    
    # Build a corpus containing this 1 document
    corpus <- VCorpus(VectorSource(text))
    
    # Clean text exactly like training
    corpus <- clean_corpus(corpus)
    
    # Build TF DTM
    dtm_tf <- DocumentTermMatrix(corpus,
                                 control = list(wordLengths = c(1, Inf)))
    
    # Align to training vocabulary
    vocab <- intersect(valid_words, Terms(dtm_tf))
    dtm_tf_aligned <- dtm_tf[, vocab, drop = FALSE]
    
    # Convert to matrix
    tf <- as.matrix(dtm_tf_aligned)
    
    # Add missing columns (terms not present in this new doc)
    missing <- setdiff(valid_words, vocab)
    if (length(missing) > 0) {
      tf <- cbind(tf, matrix(0, nrow = 1, ncol = length(missing),
                             dimnames = list(NULL, missing)))
    }
    
    # Reorder columns to match training order
    tf <- tf[, valid_words]
    
    # Compute tf-idf for this doc
    # idf is a numeric vector with names(valid_words)
    # Compute tf-idf
    tfidf <- tf * idf
    
    print(setdiff(colnames(tfidf), valid_words))
    pred <- predict(model, newdata = as.data.frame(tfidf))
    
    
    paste("Prediction:", pred)
  })
  
  
  # --- Show top cleaned words ---
  output$topwords <- renderTable({
    req(input$file)
    
    text <- read_file(input$file$datapath)
    corpus <- VCorpus(VectorSource(text))
    corpus <- clean_corpus(corpus)
    
    # Extract cleaned text to count words
    cleaned <- sapply(corpus, as.character)
    
    words <- str_extract_all(cleaned, "\\b[\\w']+\\b")[[1]]
    tibble(word = words) %>% 
      count(word, sort = TRUE) %>% 
      head(20)
  })
  
}

shinyApp(ui = ui, server = server)
