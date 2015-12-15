library(dplyr)
news <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.news.txt", 10000,encoding="UTF-8")
blogs <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.blogs.txt", 10000,encoding="UTF-8")
twitter <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.twitter.txt", 10000,encoding="UTF-8")
profanity <- read.csv("C:/Users/193344/Desktop/final/en_US/profanity.txt", header=FALSE, stringsAsFactors=FALSE)

text <- c(news,blogs,twitter)
rm(news);rm(blogs);rm(twitter)

profanity <- profanity$V1

Text_To_Clean_Sentences <- function(text_blob) {
  # swap all sentence ends with code 'ootoo'
  text_blob <- gsub(pattern=';|\\.|!|\\?', x=text_blob, replacement='ootoo')
  
  # filter out profanity
  #text_blob <- text_blob[!grepl(pattern=paste(profanity,collapse="|"),x=text_blob,ignore.case=TRUE)] 
  text_blob <- gsub(pattern=paste0('\\<',profanity,collapse="|"),
                    x=text_blob,
                    replacement = "expletive ",
                    ignore.case=TRUE)
  
  # remove all non-alpha text (numbers etc)
  text_blob <- gsub(pattern="[^[:alpha:]]",
                    x=text_blob,
                    replacement = ' ')
  
  # force all characters to lower casedf
  text_blob <- tolower(text_blob)
  
  # remove any small words {size} or {min,max}
  text_blob <- gsub(pattern="\\W*\\b\\w{1,2}\\b",
                    x=text_blob,
                    replacement=' ')
  
  # remove contiguous spaces
  text_blob <- gsub(pattern="\\s+",
                    x=text_blob, 
                    replacement=' ')
  
  # split sentences by split code
  sentence_vector <- unlist(strsplit(x=text_blob,
                                     split='ootoo',
                                     fixed = TRUE))
  return (sentence_vector)
}

Trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

Get_Ngrams <- function(sentence_splits, ngram_size=2) {
  ngrams <- c()
  for (sentence in sentence_splits) {
    sentence <- Trim(sentence)
    if ((nchar(sentence) > 0) && (sapply(gregexpr("\\W+", sentence), length) >= ngram_size)) {
      ngs <- ngram(sentence , n=ngram_size)
      ngrams <- c(ngrams, get.ngrams(ngs))
    }
  }
  return (ngrams)
}

a <- Text_To_Clean_Sentences(text)

n1 <- Get_Ngrams(a,ngram_size=1)
n2 <- Get_Ngrams(a,ngram_size=2)
n3 <- Get_Ngrams(a,ngram_size=3)

n1 <- table(n1)
n1 <- as.data.frame(n1) %>%
  arrange(desc(Freq))

n2 <- table(n2)
n2 <- as.data.frame(n2) %>%
  arrange(desc(Freq))

n3 <- table(n3)
n3 <- as.data.frame(n3) %>%
  arrange(desc(Freq))

