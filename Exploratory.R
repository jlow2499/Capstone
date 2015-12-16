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
n4 <- Get_Ngrams(a,ngram_size=4)
n5 <- Get_Ngrams(a,ngram_size=5)

n1_df <- table(n1)
n1_df <- as.data.frame(n1_df) %>%
  arrange(desc(Freq))

n2_df <- table(n2)
n2_df<- as.data.frame(n2_df) %>%
  arrange(desc(Freq))

n3_df <- table(n3)
n3_df <- as.data.frame(n3_df) %>%
  arrange(desc(Freq))

n_all <- c(n1,n2,n3,n4,n5)

# notice the trailing space at end to avoid picking last word
word <- 'infection '

matches <- c()
for (sentence in n_all) {
  # find exact match with double backslash and escape
  if (grepl(paste0('\\<',word), sentence)) {
    print(sentence)
    matches <- c(matches, sentence)
  }
}

# find highest probability word
precision_match <- c()
for (a_match in matches) {
  # how many spaces in from of search word
  precision_match <- c(precision_match,nchar(strsplit(x = a_match, split = word)[[1]][[1]]))
}

# use highest number and a random of highest for multiples
best_matched_sentence <- sample(matches[precision_match == max(precision_match)],size = 1)

print(best_matched_sentence)

# split the best matching sentence by the search word
best_match <- strsplit(x = best_matched_sentence, split = word)[[1]]
# split second part by spaces and pick first word
best_match <-  strsplit(x = best_match[[2]], split = " ")[[1]]
best_match <- best_match[[1]]

print(best_match)

library(ggplot2)
n <- n3[1:10,]



plot <- ggplot(n,aes(x=n3,y=Freq)) + geom_bar(stat="identity")
plot
