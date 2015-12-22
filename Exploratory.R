library(ngram)
library(dplyr)
news <- readLines("C:/Users/Jeremiah Lowhorn/Desktop/final/en_US/en_US.news.txt", 10000,encoding="UTF-8")
blogs <- readLines("C:/Users/Jeremiah Lowhorn/Desktop/final/en_US/en_US.blogs.txt", 10000,encoding="UTF-8")
twitter <- readLines("C:/Users/Jeremiah Lowhorn/Desktop/final/en_US/en_US.twitter.txt", 10000,encoding="UTF-8")
profanity <- read.csv("C:/Users/Jeremiah Lowhorn/Desktop/final/en_US/profanity.txt", header=FALSE, stringsAsFactors=FALSE)

text <- c(news,blogs,twitter)
rm(news);rm(blogs);rm(twitter)

profanity <- profanity$V1

Clean <- function(text_blob) {
  
  text_blob <- gsub(';|\\.|!|\\?', 
                    text_blob, 
                    'ootoo')
   
  text_blob <- gsub(paste0('\\<',profanity,collapse="|"),
                    text_blob,
                     "expletive ",
                    ignore.case=TRUE)
  
  text_blob <- gsub("[^[:alpha:]]",
                    text_blob,
                     ' ')
  
  text_blob <- tolower(text_blob)

  text_blob <- gsub("\\W*\\b\\w{1,2}\\b",
                    text_blob,
                    ' ')
  
  text_blob <- gsub("\\s+",
                    text_blob, 
                    ' ')
  
  sentence_vector <- unlist(strsplit(text_blob,
                                     split='ootoo',
                                     fixed = TRUE))
  return (sentence_vector)
}

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

a <- Clean(text)

n1 <- Make_Ngrams(a,size=1)
n2 <- Make_Ngrams(a,size=2)
n3 <- Make_Ngrams(a,size=3)
n4 <- Make_Ngrams(a,size=4)
#n5 <- Make_Ngrams(a,size=5)

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

n_all <- c(n2,n3,n4)

word <- 'montecristo '

matches <- c()
for (sentence in n_all) {
  if (grepl(paste0('\\<',word), sentence)) {
    print(sentence)
    matches <- c(matches, sentence)
  }
}

if(length(matches)==0){
  final_match <- n1_df[1,1]
}else{

split_match <- c()
for (a_match in matches) {
  split_match <- c(split_match,nchar(strsplit(x = a_match, split = word)[[1]][[1]]))
}

final_matched_sentence <- sample(matches[split_match == max(split_match)],size = 1)

print(final_matched_sentence)

final_match <- strsplit(x = final_matched_sentence, split = word)[[1]]
final_match <-  strsplit(x = final_match[[2]], split = " ")[[1]]
final_match <- final_match[[1]]
}

print(final_match)
