library(ngram)
library(dplyr)
library(ggplot2)
library(stringr)
news <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.news.txt", 10000,encoding="UTF-8")
blogs <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.blogs.txt", 10000,encoding="UTF-8")
twitter <- readLines("C:/Users/193344/Desktop/final/en_US/en_US.twitter.txt", 10000,encoding="UTF-8")
stopwords <- read.table("C:/Users/193344/Desktop/final/en_US/stopwords.csv", quote="\"", stringsAsFactors=FALSE)
stopwords <- str_replace_all(stopwords$V1, "[[:punct:]]", "")

profanity <- read.csv("C:/Users/193344/Desktop/final/en_US/profanity.txt", header=FALSE, stringsAsFactors=FALSE)

profanity <- profanity$V1

news <- clean(news)
blogs <- clean(blogs)
twitter <- clean(twitter)

news_n2 <- build_tables(news,2)
blogs_n2 <- build_tables(blogs,2)
twitter_n2 <- build_tables(twitter,2)

news_n3 <- build_tables(news,3)
blogs_n3 <- build_tables(blogs,3)
twitter_n3 <- build_tables(twitter,3)

news_n4 <- build_tables(news,4)
blogs_n4 <- build_tables(blogs,4)
twitter_n4 <- build_tables(twitter,4)

news_n5 <- build_tables(news,5)
blogs_n5 <- build_tables(blogs,5)
twitter_n5 <- build_tables(twitter,5)
