library(ngram)
library(dplyr)
library(ggplot2)
library(stringr)
news <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.news.txt", 20000,encoding="UTF-8")
blogs <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.blogs.txt", 20000,encoding="UTF-8")
twitter <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.twitter.txt", 20000,encoding="UTF-8")

sw1 <- readLines("C:/Users/193344/Desktop/final/en_US/anewhope.txt", encoding="UTF-8")
sw2 <- readLines("C:/Users/193344/Desktop/final/en_US/attackoftheclones.txt", encoding="UTF-8")
sw3 <- readLines("C:/Users/193344/Desktop/final/en_US/returnofthejedi.txt", encoding="UTF-8")
sw4 <- readLines("C:/Users/193344/Desktop/final/en_US/revengeofthesith.txt", encoding="UTF-8")
sw5 <- readLines("C:/Users/193344/Desktop/final/en_US/theempirestrikesback.txt", encoding="UTF-8")
sw6 <- readLines("C:/Users/193344/Desktop/final/en_US/theforceawakens.txt", encoding="UTF-8")
sw7 <- readLines("C:/Users/193344/Desktop/final/en_US/thephantommenace.txt", encoding="UTF-8")


profanity <- read.csv("C:/Users/193344/Desktop/final/en_US/profanity.txt", header=FALSE, stringsAsFactors=FALSE)

text <- c(news,twitter,blogs,sw1,sw2,sw3,sw4,sw5,sw6,sw7)

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
n5 <- Make_Ngrams(text,size=5)

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

n5_df <- table(n5)
n5_df <- as.data.frame(n5_df) %>%
  arrange(desc(Freq))
  
n1_df$n1 <- as.character(n1_df$n1)
n2_df$n2 <- as.character(n2_df$n2)
n3_df$n3 <- as.character(n3_df$n3)
n4_df$n4 <- as.character(n4_df$n4)
n5_df$n5 <- as.character(n5_df$n5)

five_gram <- data.frame((str_split_fixed(n5_df$n5, ' (?=[^ ]+$)',2)),stringsAsFactors=FALSE)
four_gram <-data.frame((str_split_fixed(n4_df$n4, ' (?=[^ ]+$)',2)),stringsAsFactors=FALSE)
three_gram <-data.frame((str_split_fixed(n3_df$n3, ' (?=[^ ]+$)',2)),stringsAsFactors=FALSE)
two_gram <-data.frame((str_split_fixed(n2_df$n2, ' (?=[^ ]+$)',2)),stringsAsFactors=FALSE)


sentence <- "you are a big piece"
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
