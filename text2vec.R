# Read in the English text data sets previously extracted from zips
twitter <- readLines("/home/oscar/Documents/DSSCapstone/en_US/en_US.twitter.txt")
## Allegedly deleting some lines that appears with warnings when reading
twitter <- twitter[-c(167155, 268547, 1274086, 1759032)]
news <- readLines("/home/oscar/Documents/DSSCapstone/en_US/en_US.news.txt")
blogs <- readLines("/home/oscar/Documents/DSSCapstone/en_US/en_US.blogs.txt")
## Subsampling the texts
## Assuming there are not an order in the recopilation or scrapping of the texts, I'll split each one
## of the text data sets in three parts: 
## 60% for train the model, 
## 20% for tunning the model and 
## 20% for measure

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

## Then, let's go with the ones for training
## First, go to the dir
setwd("~/Documents/finalCapstone/datasets/train")
## Save the sample
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
## Thanks to Gerald Gendron (http://www.linkedin.com/in/jaygendron/) for all the gsub applications 
## for get a really cleaning text. You can see all his job in 
## https://github.com/jgendron/datasciencecoursera
## Here we got a function which takes the text as argument, and then split it in chunks and cleaning 
## it up. Because of the time it takes to clean the data and the RAM consumption, it's necessary to 
## parallelize the code; even when the process got already faster with text2vec library. 
## Thanks to Dmitriy Selivanov, what a great package.
## You can find all about on http://text2vec.org/ or on https://github.com/dselivanov/text2vec
cleaner <- function(x){
        ## This is the principal library
        library(text2vec)
        ## This library also allow us to split the data set in several chunks for parallelization
        splits <- split_into(x, 100)
        ## Load required libraries for parallelization
        library(foreach)
        library(doParallel)
        ## Activate 3 clusters for parallelizing the work
        cl <- makeCluster(3)
        registerDoParallel(cl)
        library(tm)
        ## This is the loop parallelized
        final <- foreach(i=1:length(splits), .combine = rbind, .packages = c("tm")) %dopar% {
                ## The splitted as vector
                splitsn <- as.vector(splits[[i]])
                ## Replace some strings
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
                df <- gsub("<[^EOS].+>"," ", df)
                df <- gsub("[0-9]+"," ", df)
                df <- gsub("<>"," ", df)
                df <- gsub("#", " ", df)
                df <- gsub("RT", " ", df)
                df <- tolower(df)
        }
        ## splitting into sentences by <eos> marker
        EOS <- strsplit(final, "<eos>")
        ## Making it all a text vector
        EOS <- unlist(EOS)
        ## Split in chunks for parallelizing (again)
        splits <- split_into(EOS, 100)
        library(foreach)
        library(doParallel)
        library(tm)

        final <- foreach(i=1:length(splits), .combine = rbind, .packages = c("tm")) %dopar% {
        splitsn <- as.vector(splits[[i]])
        ## Removing some extra spaces
        df <- sub("^\\s+", "", splitsn)
        df <- sub("\\s+$", "", df)
        df <- gsub("^\\s+|\\s+$", "", df)
        df <- gsub("\\s+", " ", df)
        }
        final <- final[grepl(" ", final)]
}
## Setting path to the training files dir
setwd("~/Documents/finalCapstone/datasets/train")
## Finally save the data cleaned.
df <- cleaner(readLines("./ttt.txt"))
saveRDS(df, "./tRDS")
df <- cleaner(readLines("./bbt.txt"))
saveRDS(df, "./bRDS")
df <- cleaner(readLines("./nnt.txt"))
saveRDS(df, "./nRDS")
#######################################################################################
## Now will construct a text2vec vocabulary (token counts) for each training set.
## I found so usefull this function because we really don't need a dtm or something like that and
## then flatening or summarize it. So, the vocabulary it's computationally cheaper and give us
## exactly what we need: the overall frecuencies of the tokens.

countTokens <- function(x, n){
        library(text2vec)
        ## Because of the cleaning tha we have done so far, the tokenization have to split
        ## by space: space_tokenizer
        tok_fun = space_tokenizer
        it_train = itoken(x, tokenizer = tok_fun, progressbar = TRUE)
        vocab = create_vocabulary(it_train, ngram = c(ngram_min = n, ngram_max = n))
}
## It could looks like a ugly way to do it, but I don't write a loop because I've to restart RStudio
## each time to get free RAM (I didn't get a way to avoid the colapse of the pc because the lack of
## available RAM)

## Twitter n-grams
setwd("~/Documents/finalCapstone/datasets/train")
x <- readRDS("./tRDS")

v <- countTokens(x, 1)
saveRDS(v$vocab[, 1:2], "./t1gRDS")

v <- countTokens(x, 2)
saveRDS(v$vocab[, 1:2], "./t2gRDS")

v <- countTokens(x, 3)
saveRDS(v$vocab[, 1:2], "./t3gRDS")

## Blogs n-grams
setwd("~/Documents/finalCapstone/datasets/train")
x <- readRDS("./bRDS")

v <- countTokens(x, 1)
saveRDS(v$vocab[, 1:2], "./b1gRDS")

v <- countTokens(x, 2)
saveRDS(v$vocab[, 1:2], "./b2gRDS")

v <- countTokens(x, 3)
saveRDS(v$vocab[, 1:2], "./b3gRDS")

## News n-grams
setwd("~/Documents/finalCapstone/datasets/train")
x <- readRDS("./nRDS")

v <- countTokens(x, 1)
saveRDS(v$vocab[, 1:2], "./n1gRDS")

v <- countTokens(x, 2)
saveRDS(v$vocab[, 1:2], "./n2gRDS")

v <- countTokens(x, 3)
saveRDS(v$vocab[, 1:2], "./n3gRDS")
#######################################################################################
## Exploratory data analysis
library(data.table)
## Onegrams
setwd("~/Documents/finalCapstone/datasets/train")
t1 <- as.data.table(readRDS("./t1gRDS"))
b1 <- as.data.table(readRDS("./b1gRDS"))
n1 <- as.data.table(readRDS("./n1gRDS"))
## Featuring the percentage and the cumulative percentage
featuring <- function(x){
        x <- setorder(x, -terms_counts)
        x$perc <- x$terms_counts / sum(x$terms_counts)
        x$cumPerc <- cumsum(x$perc)
        return(x)
}

## Let's get bind all the tokens and summarise to get the total number of times each token appears.

bindup <- function(x, y, z){
allOne <- rbind(x, y, z)
print(paste("Initial number of tokens:", nrow(allOne), sep = " "))
allOne <- allOne[, .(total = sum(terms_counts)), by = .(terms)]
print(paste("Final number of tokens:", nrow(allOne), sep = " "))
allOne <- setorder(allOne, -total)
allOne$perc <- allOne$total / sum(allOne$total)
allOne$cumPerc <- cumsum(allOne$perc)
return(allOne)
}

onegram <- bindup(t1, b1, n1)
library(ggplot2)
ggplot(onegram[1:20, ], aes(x = reorder(terms, -total), y = total)) + geom_bar(stat = "identity")
## 1. split the tokens with strsplit
## 2. Take the first part 
onegram <- onegram[, c(1, 2)]
## Save it
saveRDS(onegram, "./onegramRDS")
## Let's see how much it is loaded
format(object.size(onegram), units = "Mb")
## Clean space
rm(list = c("t1", "b1", "n1"))
##############################################################################################
## Bigrams
t2 <- as.data.table(readRDS("./t2gRDS"))
b2 <- as.data.table(readRDS("./b2gRDS"))
n2 <- as.data.table(readRDS("./n2gRDS"))
## Let's get bind all the tokens and summarise to get the total number of times each token appears.

bigram <- bindup(t2, b2, n2)
ggplot(bigram[1:20, ], aes(x = reorder(terms, -total), y = total)) + geom_bar(stat = "identity")

## 1. split the tokens with strsplit and keep the first part or token-root
root <- unlist(strsplit(bigram$terms, "_"))[seq(1, dim(bigram)[1] * 2 - 1, 2)]
bigram$root <- root
## Let's define token-root as the token without its last word. i.e: for "of_the", its token-root
## it's "of", for "of_the_year", it's "of_the".
## When we are looking for predict, we take the phrase and prune it to no more than  two words. 
## Then we look up for this two words, a bigram, the token-root and take the three more frequent  
## trigrams which are conformated by it (the token-root), and extract its last word: they are the 
## predicted words. If the phrase have no more than two words or if the token-root was not found or 
## by any reason didn't give back a predicted word -according to stupid back-off model-, we got to 
## look up a onegram token-root to give back the last word of the three more frequents bigrams which 
## are composed by this onegram token-root.
## For each one of the token-root there are bunches, too many of trigrams which are conformated by 
## them, but I realize that we will use just the three more frequents.
## Let's see how much bigrams there are for each token-root (onegram) 
bigramCount <- bigram[, .N, by = root]
bigramCount <- setorder(bigramCount, -N)
head(bigramCount)
summary(bigramCount$N)
## Let's take just the more frequent token-root: "the"
head(bigram[root=="the"], 10)
## When we predict, we won't predict 88047 words, neither 10; we'll predict just the first three. 
## So, we'll left out all but the three more frequent
bigramPruned <- bigram[, head(.SD, 3), by = root]
head(bigramPruned)
## The efficiency of this reductions can be seen by it self: the proportion of the pruned data frame
## respect to the original one
dim(bigramPruned)[1] / dim(bigram)[1]
## in Mb
format(object.size(bigram), units = "Mb") ## 959 Mb
format(object.size(bigramPruned), units = "Mb") ## 86 Mb
## Drop it down some useless variables
bigram <- bigramPruned[, c(2, 3, 1)]
head(bigram)
saveRDS(bigram, "./bigramRDS")
## Also we'll need to keep the number of times each token-root appears to calculate later the probs.
saveRDS(bigramCount, "./bigramCountRDS")
rm(list = c("t2", "b2", "n2", "bigramPruned", "root"))
#######################################################################################
## Trigrams
t3 <- as.data.table(readRDS("./t3gRDS"))
b3 <- as.data.table(readRDS("./b3gRDS"))
n3 <- as.data.table(readRDS("./n3gRDS"))
## Let's get bind all the tokens and summarise to get the total number of times each token appears.

trigram <- bindup(t3, b3, n3)
ggplot(trigram[1:15, ], aes(x = reorder(terms, -total), y = total)) + geom_bar(stat = "identity")

## Cleaning up
rm(list = c("t3", "b3", "n3"))
gc()
trigram <- trigram[, c(1, 2)]
saveRDS(trigram, "./trigramRDS")
######################################################################################
library(data.table)
setwd("~/Documents/finalCapstone/datasets/train")
trigram <- as.data.table(readRDS("./trigramRDS"))
## 1. split the tokens with strsplit and keep the first part or token-root
trigram$root <- paste(unlist(strsplit(trigram$terms, "_"))[seq(1, dim(trigram)[1] * 3 - 2, 3)], 
                      unlist(strsplit(trigram$terms, "_"))[seq(2, dim(trigram)[1] * 3 - 1, 3)], sep = "_")
## Let's define token-root as the token without its last word. i.e: for "of_the", its token-root
## it's "of", for "of_the_year", it's "of_the".
## When we are looking for predict, we take the phrase and prune it to no more than  two words. 
## Then we look up for this two words, a bigram, the token-root and take the three more frequent  
## trigrams which are conformated by it (the token-root), and extract its last word: they are the 
## predicted words. If the phrase have no more than two words or if the token-root was not found or 
## by any reason didn't give back a predicted word -according to stupid back-off model-, we got to 
## look up a onegram token-root to give back the last word of the three more frequents bigrams which 
## are composed by this onegram token-root.
## For each one of the token-root there are bunches, too many of trigrams which are conformated by 
## them, but I realize that we will use just the three more frequents.
## Let's see how much bigrams there are for each token-root (onegram) 
trigramCount <- trigram[, .N, by = root]
trigramCount <- setorder(trigramCount, -N)
head(trigramCount)
summary(trigramCount$N)
## Let's take just the more frequent token-root: "the"
head(trigram[root=="one_of"], 10)
## When we predict, we won't predict 88047 words, neither 10; we'll predict just the first three. 
## So, we'll left out all but the three more frequent
trigramPruned <- trigram[, head(.SD, 3), by = root]
head(trigramPruned)
## The efficiency of this reductions can be seen by it self: the proportion of the pruned data frame
## respect to the original one
dim(trigramPruned)[1] / dim(trigram)[1]
## in Mb
format(object.size(trigram), units = "Mb") ## 959 Mb
format(object.size(trigramPruned), units = "Mb") ## 86 Mb
## Drop it down some useless variables
trigram <- trigramPruned[, c(2, 3, 1)]
head(trigram)
saveRDS(trigram, "./trigramRDS")
## Also we'll need to keep the number of times each token-root appears to calculate later the probs.
saveRDS(trigramCount, "./trigramCountRDS")
rm(list = c("t2", "b2", "n2", "trigramPruned", "root"))
#######################################################################################