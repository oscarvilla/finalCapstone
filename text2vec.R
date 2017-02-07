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
## Bigrams
setwd("~/Documents/finalCapstone/datasets/train")
t1 <- as.data.table(readRDS("./t1gRDS"))
b1 <- as.data.table(readRDS("./b1gRDS"))
n1 <- as.data.table(readRDS("./n1gRDS"))

sizeObj <- data.frame(obj = c("t1", "b1", "n1"), 
                      ntokens = c(dim(t1)[1], dim(b1)[1], dim(n1)[1]), 
                      size = c(format(object.size(t1), units = "Mb"), 
                               format(object.size(b1), units = "Mb"), 
                               format(object.size(n1), units = "Mb")))
sizeObj
## Featuring the percentage and the cumulative percentage
featuring <- function(x){
        x <- setorder(x, -terms_counts)
        x$perc <- x$terms_counts / sum(x$terms_counts)
        x$cumPerc <- cumsum(x$perc)
        return(x)
}
t1 <- featuring(t1)
b1 <- featuring(b1)
n1 <- featuring(n1)
## Plotting some basics about the separate bigrams
library(ggplot2)
ggplot(t1[1:20, ], aes(x = reorder(terms, -terms_counts), y = terms_counts)) + geom_bar(stat = "identity")
ggplot(b1[1:20, ], aes(x = reorder(terms, -terms_counts), y = terms_counts)) + geom_bar(stat = "identity")
ggplot(n1[1:20, ], aes(x = reorder(terms, -terms_counts), y = terms_counts)) + geom_bar(stat = "identity")
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

bigram <- bindup(t1, b1, n1)
ggplot(bigram[1:20, ], aes(x = reorder(terms, -total), y = total)) + geom_bar(stat = "identity")


## 1. split the tokens with strsplit
## 2. Take the first part 
t1 <- unlist(strsplit(bigram$terms, "_"))[seq(1, dim(bigram)[1] * 2 - 1, 2)]
bigram$t1 <- t1
## 3. 
bigramPruned <- bigram[, head(.SD, 3), by = t1]
bigramPruned <- bigramPruned[order(bigramPruned$terms)]
head(bigramPruned)
dim(bigramPruned)[1] / dim(bigram)[1]
format(object.size(bigram), units = "Mb") ## 959 Mb
format(object.size(bigramPruned), units = "Mb") ## 86 Mb
## Plotting percentages and cumulative oercentages

plot(t1$cumPerc[1:500000])
plot(b1$cumPerc[1:500000])
plot(n1$cumPerc[1:500000])

## More and less frequent tokens
plot(head(setorder(t1, -terms_counts), 20)$cumPerc); head(setorder(b1, -terms_counts), 20); head(setorder(n1, -terms_counts), 20)
tail(setorder(t1, -terms_counts), 20); tail(setorder(b1, -terms_counts), 20); tail(setorder(n1, -terms_counts), 20)

library(ggplot2)






#######################################################################################
t2 <- readRDS("./t2gRDS")
t3 <- readRDS("./t3gRDS")
sizeObj <- data.frame(obj = c("t1", "t2", "t3"), 
                      size = c(format(object.size(t1), units = "Mb"), 
                               format(object.size(b), units = "Mb"), 
                               format(object.size(t3), units = "Mb")))
sizeObj
library(data.table)
head(setorder(t2, -terms_counts))
tail(setorder(t2, -terms_counts))
#######################################################################################
## Shows the first element of each bigram
unlist(strsplit(t1$terms[1:10], "_"))[seq(1, 19, 2)]
## With the first one can make a datatable sd and keep the three more frecuent of each first element
## Shows the second element of each bigram
unlist(strsplit(t1$terms[1:10], "_"))[seq(2, 20, 2)]
#######################################################################################