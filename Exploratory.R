library(ngram)
library(dplyr)
library(ggplot2)
library(stringr)
news <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.news.txt", 20000,encoding="UTF-8")
blogs <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.blogs.txt", 20000,encoding="UTF-8")
twitter <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.twitter.txt", 20000,encoding="UTF-8")
profanity <- read.csv("C:/Users/193344/Desktop/final/en_US/profanity.txt", header=FALSE, stringsAsFactors=FALSE)

text <- c(news,twitter,blogs)

rm(news);rm(blogs);rm(twitter)

profanity <- profanity$V1

text <- gsub(pattern=';|\\.|!|\\?', x=text, replacement='ootoo')
text <- str_replace_all(text, "[[:punct:]]", "")
text <- gsub(pattern="[^[:alpha:]]", x=text, replacement = ' ')
text <- tolower(text)
text <- gsub(pattern="\\W*\\b\\w{1,2}\\b", x=text, replacement=' ')
text <- gsub(pattern="\\s+", x=text, replacement=' ')
text <- unlist(strsplit(x=text, split='ootoo',fixed = TRUE))
text <- gsub(pattern=paste0('\\<',profanity,collapse="|"),
             x=text,
             replacement="")

Trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
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

n1 <- Make_Ngrams(text,size=1)
n2 <- Make_Ngrams(text,size=2)
n3 <- Make_Ngrams(text,size=3)
n4 <- Make_Ngrams(text,size=4)

n1_df <- table(n1)
n1_df <- as.data.frame(n1_df) %>%
  arrange(desc(Freq))
n1_df$n1 <- as.character(n1_df$n1)

n2_df <- table(n2)
n2_df<- as.data.frame(n2_df) %>%
  arrange(desc(Freq))

n3_df <- table(n3)
n3_df <- as.data.frame(n3_df) %>%
  arrange(desc(Freq))

n4_df <- table(n4)
n4_df <- as.data.frame(n4_df) %>%
  arrange(desc(Freq))
  
  n1_df$n1 <- as.character(n1_df$n1)
n2_df$n2 <- as.character(n2_df$n2)
n3_df$n3 <- as.character(n3_df$n3)
n4_df$n4 <- as.character(n4_df$n4)


four_gram <-data.frame((str_split_fixed(n4_df$n4, ' (?=[^ ]+$)',2)),stringsAsFactors=FALSE)
three_gram <-data.frame((str_split_fixed(n3_df$n3, ' (?=[^ ]+$)',2)),stringsAsFactors=FALSE)
two_gram <-data.frame((str_split_fixed(n2_df$n2, ' (?=[^ ]+$)',2)),stringsAsFactors=FALSE)


sentence <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
sentence <- str_replace_all(sentence, "[[:punct:]]", "")
sentence <- tolower(sentence)
sentence <- gsub(pattern="\\s+", x=sentence, replacement=' ')

sentence<-data.frame(setNames(strsplit(x = sentence, split = " "),nm="x"),stringsAsFactors=FALSE)
sentence <- if(length(sentence$x) >=3) {
  tail(sentence,3)} else{
    tail(sentence)}
sentence <- unlist(sentence)
sentence <- data.frame(sentence,stringsAsFactors=FALSE)


word <- c(paste(sentence$sentence,collapse=" "))
word2 <- c(paste(sentence[2,1],sentence[3,1],collapse=" "))

if(length(sentence$sentence) == 3){
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

if(is.na(match) == TRUE & length(sentence$sentence)==3){
  match <- three_gram[three_gram$X1 == word2,]
  match <- match[1,2]
} 

if(is.na(match) == TRUE){
  match <- "the"
}


