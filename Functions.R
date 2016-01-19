build_tables <- function(text,size) {
  
  n2 <- Make_Ngrams(text,size=size)
  
  n2_df <- table(n2)
  n2_df<- as.data.frame(n2_df) %>%
    arrange(desc(Freq))
  
  n2_df$n2 <- as.character(n2_df$n2)
  Freq <- n2_df$Freq
  
  table <-data.frame((str_split_fixed(n2_df$n2, ' (?=[^ ]+$)',2)),stringsAsFactors=FALSE)
  table <- cbind(table,Freq)
  table
}

Trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

clean <- function(text) {
  text <- gsub(pattern=';|\\.|!|\\?', x=text, replacement='ootoo')
  text <- str_replace_all(text, "[[:punct:]]", "")
  text <- gsub(pattern="[^[:alpha:]]", x=text, replacement = ' ')
  text <- tolower(text)
  #text <- gsub(pattern="\\W*\\b\\w{1,2}\\b", x=text, replacement=' ')
  text <- gsub(pattern="\\s+", x=text, replacement=' ')
  text <- unlist(strsplit(x=text, split='ootoo',fixed = TRUE))
  text <- gsub(pattern=paste0('\\<',profanity,collapse="|"),
               x=text,
               replacement="")
  text
}

Make_Ngrams <- function(sentence_splits, size=2) {
  ngrams <- c()
  for (sentence in sentence_splits) {
    sentence <- Trim(sentence)
    if ((nchar(sentence) > 0) && (sapply(gregexpr("\\W+", sentence), length) >= size)) {
      ngs <- ngram(sentence , n=size)
      ngrams <- c(ngrams, get.ngrams(ngs))
    }
  }
  return (ngrams)
}
