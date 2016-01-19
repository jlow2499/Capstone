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
