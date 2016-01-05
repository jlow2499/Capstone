library(shiny)

Trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    find_next_word(tolower(input$current_sentence))
  })

  output$value <- renderPrint({ paste(tolower(input$text), find_next_word(tolower(input$text))) })
  
  find_next_word <- function(sentence) { 
    sentence <- str_replace_all(sentence, "[[:punct:]]", "")
    sentence <- tolower(sentence)
    sentence <- gsub(pattern="\\s+", x=sentence, replacement=' ')
    
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
      word4 <- c(paste(sentence[1,1],collapse=" "))  
    } else if(length(sentence$sentence)==2){
      word2 <- c(paste(sentence[1,1],sentence[2,1],collapse=" "))
      word3 <- c(paste(sentence[2,1],collapse=" "))
      word4 <- NA
    } else{
      word2 <- c(paste(sentence[1,1],collapse=" "))
      word3 <- NA
      word4 <- NA
    }
    
    if(length(sentence$sentence) == 4){
      match <- five_gram[five_gram$X1 == word,]
      match <- match[1,2]
    } else if(length(sentence$sentence) == 3){
      match <- four_gram[four_gram$X1 == word,]
      match <- match[1,2]
    } else if(length(sentence$sentence) == 2){
      match <- three_gram[three_gram$X1 == word,]
      match <- match[1,2]
    } else if(length(sentence$sentence)==1){
      match <- two_gram[two_gram$X1 == word,]
      match <- match[1,2]
    } else{
      match <- NA
    }
    
    if(is.na(match) == TRUE && length(sentence$sentence)==4){
      match <- four_gram[four_gram$X1 == word2,]
      match <- match[1,2]
    }
    if(is.na(match) == TRUE && length(sentence$sentence)>=3){
      match <- three_gram[three_gram$X1 == word3,]
      match <- match[1,2]
    }
    
    if(is.na(match) == TRUE && length(sentence$sentence)>=2){
      match <- two_gram[two_gram$X1 == word4,]
      match <- match[1,2]
    }
    
    
    if(is.na(match) == TRUE){
      match <- "the"
    }
    match
  }
  
  output$value <- renderPrint({ find_next_word(tolower(input$text)) })
})
