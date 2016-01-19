library(shiny)
library(shinydashboard)
library(stringr)
library(dplyr)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Word Predictor", tabName = "word", icon = icon("dashboard")),
    menuItem("Information",tabName = "info", icon = icon("th"))
 #   downloadButton("downloadData", 'Download Data')
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "word",
            column(4,
                   fluidRow(box(title="Jeremiah Lowhorn")),
                   fluidRow(box(title="Instructions","Begin typing or press the button in the word prediction box to add it to the sentence. The data used in the prediction can be changed by the 'Add/Remove Datasets' check box below.")),
                   fluidRow(box(title="Add/Remove Datasets",
                                checkboxGroupInput("rmv","Add/Remove Datasets",choices=c("Twitter","News","Blogs"),selected=c("Twitter","News","Blogs"))))
                   ),
            column(8,fluidRow(box(textInput("text", label = h2("Next Word Predictor Input"), value = "Hello how are"))),
        #    fluidRow(box(title="Word Additions","Press the button in the word prediction box to add it to the sentence.")),
            fluidRow(box(title="Word Prediction",actionButton("addword",textOutput("word")))),
            fluidRow(box(title="Sentence Prediction",(textOutput("sentence")))))
    ),
    tabItem(tabName = "info",
            column(12,
                   box(title="Synopsis","This application is for the Johns Hopkins Coursera Data Science Capstone. We are provided with a text corpus that we are to perform exploratory analysis upon and use in model building for our word prediction algorithm which is found in the 'Word predictor' tab. The text contains special characters, unneeded spaces, and profanity that must first be removed. The text comes from a document of Tweets, news articles, and blog posts. R's base package functions and regular expressions are used in this analysis to clean the data and make it ready to split into n-gram models. The n-grams are used to predict the next word in the sentence based upon the user's input."),
                   (box(title="Methodology","This application parses out the words in the written sentence to build one, two, three, or four grams. These ngrams are then used to subset dataframes where the first column is n-1 words and the second column is the last word in the ngram.")),
                   (box(title="Stop Words","Stop words are words that refer to the most common words in a language. In our case words such as 'the' & 'and' would be considered stop words. For this application stop words are not removed from the ngrams unless the previous word in the sentence is a stop word. Then the dataframes are reactively subset to remove the stop words")),
                   (box(title="N-Gram Definition","In the fields of computational linguistics and probability, an n-gram is a contiguous sequence of n items from a given sequence of text or speech. The items can be phonemes, syllables, letters, words or base pairs according to the application.",
                                "                 ",tags$a(href="https://en.wikipedia.org/wiki/N-gram","Wikipedia"))),
            (box(title="Natural Language Processing","Natural language processing (NLP) is a field of computer science, artificial intelligence, and computational linguistics concerned with the interactions between computers and human (natural) languages",
                                    tags$a(href="https://en.wikipedia.org/wiki/Natural_language_processing","Wikipedia"))))
    )
    
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "Word Prediction App"),
  sidebar,
  body
)

server <- function(input, output, session) {
  
  string <- reactive({
    string <- c(input$rmv[1],input$rmv[2],input$rmv[3])
    string
  })
  
  
  five_gram <- reactive({
    if("Twitter" %in% string()){
      twitter <- twitter_n5
    } else{
      twitter <- NULL
    }
    if("Blogs" %in% string()){
      blogs <- blogs_n5
    } else{
      blogs <- NULL
    }
    if("News" %in% string()){
      news <- news_n5
    } else{
      news <- NULL
    }
    five_gram <- rbind(twitter,blogs,news) %>%
      arrange(desc(Freq))
    five_gram
  })
  
  four_gram <- reactive({
    if("Twitter" %in% string()){
      twitter <- twitter_n4
    } else{
      twitter <- NULL
    }
    if("Blogs" %in% string()){
      blogs <- blogs_n4
    } else{
      blogs <- NULL
    }
    if("News" %in% string()){
      news <- news_n4
    } else{
      news <- NULL
    }
    four_gram <- rbind(twitter,blogs,news) %>%
      arrange(desc(Freq))
    four_gram
  })
  
  three_gram <- reactive({
    if("Twitter" %in% string()){
      twitter <- twitter_n3
    } else{
      twitter <- NULL
    }
    if("Blogs" %in% string()){
      blogs <- blogs_n3
    } else{
      blogs <- NULL
    }
    if("News" %in% string()){
      news <- news_n3
    } else{
      news <- NULL
    }
    three_gram <- rbind(twitter,blogs,news) %>%
      arrange(desc(Freq))
    three_gram
  })
  
  two_gram <- reactive({
    if("Twitter" %in% string()){
      twitter <- twitter_n2
    } else{
      twitter <- NULL
    }
    if("Blogs" %in% string()){
      blogs <- blogs_n2
    } else{
      blogs <- NULL
    }
    if("News" %in% string()){
      news <- news_n2
    } else{
      news <- NULL
    }
    two_gram <- rbind(twitter,blogs,news) %>%
      arrange(desc(Freq))
    two_gram
  })
  
  
  
  
  find_next_word <- function(sentence) { 
    sentence <- str_replace_all(sentence, "[[:punct:]]", "")
    sentence <- tolower(sentence)
    sentence <- sub("\\s+$", "", sentence)
    sentence <- gsub(pattern="\\s+", x=sentence, replacement=' ')
    lastword <- word(sentence,-1)
    
    sentence<-data.frame(setNames(strsplit(x = sentence, split = " "),nm="x"),stringsAsFactors=FALSE)
    sentence <- if(length(sentence$x) >=4) {
      tail(sentence,4)} else{
        tail(sentence)}
    sentence <- unlist(sentence)
    sentence <- data.frame(sentence,stringsAsFactors=FALSE)
    
    word <- c(paste(sentence$sentence,collapse=" "))
    if(length(sentence$sentence)==4){
      word2 <- c(paste(sentence[2,1],sentence[3,1],sentence[4,1],collapse=" "))
      word3 <- c(paste(sentence[3,1],sentence[4,1],collapse=" "))
      word4 <- c(paste(sentence[4,1],collapse=" "))
    } else if(length(sentence$sentence)==3){
      word2 <- c(paste(sentence[1,1],sentence[2,1],sentence[3,1],collapse=" "))
      word3 <- c(paste(sentence[2,1],sentence[3,1],collapse=" "))
      word4 <- c(paste(sentence[3,1],collapse=" "))  
    } else if(length(sentence$sentence)==2){
      word2 <- c(paste(sentence[1,1],sentence[2,1],collapse=" "))
      word3 <- c(paste(sentence[2,1],collapse=" "))
      word4 <- NA
    } else{
      word2 <- c(paste(sentence[1,1],collapse=" "))
      word3 <- NA
      word4 <- NA
    }
    
    if((lastword %in% stopwords)==TRUE){
      five <- five_gram()[!five_gram()$X2 %in% stopwords,]
      four <- four_gram()[!four_gram()$X2 %in% stopwords,]
      three <- three_gram()[!three_gram()$X2 %in% stopwords,]
      two <- two_gram()[!two_gram()$X2 %in% stopwords,]
    } else{
      five <- five_gram()
      four <- four_gram()
      three <- three_gram()
      two <- two_gram()
    }
    
    if(length(sentence$sentence) == 4){
      match <- five[five$X1 == word,]
      match <- match[1,2]
    } else if(length(sentence$sentence) == 3){
      match <- four[four$X1 == word,]
      match <- match[1,2]
    } else if(length(sentence$sentence) == 2){
      match <- three[three$X1 == word,]
      match <- match[1,2]
    } else if(length(sentence$sentence)==1){
      match <- two[two$X1 == word,]
      match <- match[1,2]
    } else{
      match <- NA
    }
    
    if(is.na(match) == TRUE && length(sentence$sentence)==4){
      match <- four[four$X1 == word2,]
      match <- match[1,2]
    }
    if(is.na(match) == TRUE && length(sentence$sentence)>=3){
      match <- three[three$X1 == word3,]
      match <- match[1,2]
    }
    
    if(is.na(match) == TRUE && length(sentence$sentence)>=2){
      match <- two[two$X1 == word4,]
      match <- match[1,2]
    }
    
    if(is.na(match) == TRUE && length(sentence$sentence)>=2){
      match <- two[two$X1 == word3,]
      match <- match[1,2]
    }
    
    if(is.na(match) == TRUE && length(sentence$sentence)>=1){
      match <- "and"
    }
    
    if(is.na(match) == TRUE){
      match <- "the"
    }
    match
  }
  
  output$word <- renderText({find_next_word(tolower(input$text)) })
  output$value <- renderPrint({ find_next_word(tolower(input$text)) })
  output$sentence <- renderText({ paste((input$text),find_next_word(tolower(input$text))) })
  
  
  observe({
    if (input$addword == 0) return()
    isolate({
      updateTextInput(session, "text",
                      value = paste(input$text, find_next_word(tolower(input$text))))
    })
  })
}
shinyApp(ui, server)
