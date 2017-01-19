# Read in the English text data sets previously extracted from zips
twitter <- readLines("/home/oscar/Documents/DSSCapstone/en_US/en_US.twitter.txt")
## Allegedly deleting some lines that appears with warnings when reading
twitter <- twitter[-c(167155, 268547, 1274086, 1759032)]
news <- readLines("/home/oscar/Documents/DSSCapstone/en_US/en_US.news.txt")
blogs <- readLines("/home/oscar/Documents/DSSCapstone/en_US/en_US.blogs.txt")
## Subsampling the texts
## Assuming there are not a order in the recopilation or scrapping of the texts, I'll split each one of the
## text data sets in three parts: 60% for train the model, 20% for tunning the model and 20% for measure

## ttt for twitter train, ttm for twitter model tunning and tms for twitter measure
train <- c(1, round(length(twitter) * 0.6, 0))
tunning <- c(train[2] + 1, train[2] + round(length(twitter) * 0.2, 0))
measure <- c(tunning[2] + 1, tunning[2] + round(length(twitter) * 0.2, 0))

ttt <- twitter[train[1]:train[2]]
ttm <- twitter[tunning[1]:tunning[2]]
tms <- twitter[measure[1]:measure[2]]
## bbt for blog train, bbm for blog model tunning and bms for blog measure
train <- c(1, round(length(blogs) * 0.6, 0))
tunning <- c(train[2] + 1, train[2] + round(length(blogs) * 0.2, 0))
measure <- c(tunning[2] + 1, tunning[2] + round(length(blogs) * 0.2, 0))

bbt <- blogs[train[1]:train[2]]
bbm <- blogs[tunning[1]:tunning[2]]
bms <- blogs[measure[1]:measure[2]]
## nnt for news train, nnm for news model tunning and nms for news measure
train <- c(1, round(length(news) * 0.6, 0))
tunning <- c(train[2] + 1, train[2] + round(length(news) * 0.2, 0))
measure <- c(tunning[2] + 1, tunning[2] + round(length(news) * 0.2, 0))

nnt <- news[train[1]:train[2]]
nnm <- news[tunning[1]:tunning[2]]
nms <- news[measure[1]:measure[2]]

## Saving the files and cleaning the enviroment
## The ones for training
setwd("~/Documents/finalCapstone/datasets/train")
write(ttt, "./ttt.txt")
write(bbt, "./bbt.txt")
write(nnt, "./nnt.txt")
## The ones for model tunning
setwd("~/Documents/finalCapstone/datasets/model")
write(ttm, "./ttm.txt")
write(bbm, "./bbm.txt")
write(nnm, "./nnm.txt")
## The ones for measure
setwd("~/Documents/finalCapstone/datasets/measure")
write(nms, "./nms.txt")
write(tms, "./tms.txt")
write(bms, "./bms.txt")
rm(list = ls())
######################################################################################
## Cleaning the data
setwd("~/Documents/finalCapstone/datasets/train") # getting one level up
## Thanks to Gerald Gendron for this cleaning text, you can see all his job in
## http://www.linkedin.com/in/jaygendron/

######################################################################################
twitter <- readLines("./ttt.txt")
library(text2vec)
## Here can star a function which takes the text as arg, then split it in chunks and cleaning it up
splits <- split_into(twitter, 100)
library(foreach)
library(doParallel)
library(tm)

s1 <- Sys.time()
final <- foreach(i=1:length(splits), .combine = rbind, .packages = c("tm")) %dopar% {
        splitsn <- as.vector(splits[[i]])
        df <- gsub("-", " ", splitsn)
        df <- gsub("/", " ", df)
        df <- gsub("<>", "\\'", df)
        df <- gsub("\\. |\\.$","  <EOS> ", df)
        df <- gsub("\\? |\\?$","  <EOS> ", df)
        df <- gsub("\\! |\\!$","  <EOS> ", df)
        df <- gsub("<85>"," <EOS> ", df)
        df <- gsub("<92>","'", df)
        df <- gsub("\\&", " and ", df)
        df <- gsub("[^[:alnum:][:space:]\'<>]", " ", df)
        df <- gsub(" www(.+) ", " ", df)
        df <- gsub(" [b-hj-z] "," ", df)
        df <- gsub(" ' "," ", df)        
        df <- gsub("\\' ", " ", df)
        df <- gsub(" ' ", " ", df)
        df <- gsub("<[^EOS].+>"," ", df)
        df <- gsub("[0-9]+"," <NUM> ", df)
        df <- gsub("<>"," ", df)
        df <- gsub("#", " ", df)
        df <- gsub("RT", " ", df)
        df <- tolower(df)
}
s2 <- Sys.time()
s2 - s1
#######################################################################################
## After all this cleansing, you can start the vocab with text2vec
## When you tokenize, take in account that "<eos>" is a separator
library(text2vec)
library(data.table)
library(foreach)
library(doParallel)   

tok_fun = word_tokenizer

it_train = itoken(word_tokenizer(tokenize_regex(d, "<eos>")), progressbar = TRUE)
vocab = create_vocabulary(it_train, ngram = c(ngram_min = 2L, ngram_max = 2L))

tokenize_sentences(d, lowercase = FALSE, strip_punctuation = FALSE,
                   simplify = FALSE)
#######################################################################################
s1 <- Sys.time()
final <- foreach(i=1:length(splits), .combine = rbind, .packages = c("text2vec")) %dopar% {
        splitsn <- as.vector(splits[[i]])
        vocab <- create_vocabulary()
        
}
s2 <- Sys.time()
s2 - s1