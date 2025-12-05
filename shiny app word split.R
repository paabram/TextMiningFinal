library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(tidytext)

ui <- fluidPage(
  titlePanel("Word Count App (Stop Words Removed)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a text file (.txt)", 
                accept = c(".txt")),
      helpText("This app reads the file, extracts all words, removes stop words, 
               and lists the remaining words ordered by frequency.")
    ),
    
    mainPanel(
      tableOutput("wordtable")
    )
  )
)

server <- function(input, output) {
  
  output$wordtable <- renderTable({
    req(input$file)
    
    # Read file
    text <- read_file(input$file$datapath)
    
    # Extract words: letters, numbers, apostrophes
    words <- str_to_lower(text)
    words <- str_extract_all(words, "\\b[\\w']+\\b")[[1]]
    
    # Convert to tibble
    df <- tibble(word = words)
    
    # Remove stop words (from tidytext)
    df_clean <- df %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE)
    
    df_clean
  })
}

shinyApp(ui = ui, server = server)

