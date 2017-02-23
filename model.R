library(data.table)
## One gram
onegram <- as.data.table(readRDS("./datasets/train/onegramRDS"))
n <- sum(onegram$total)
## Reduce the sample of onegrams, because we'll use, at the most, just the three more frequent
onegram <- data.table(terms = onegram[1:3], 
                      fre = onegram[1:3]$total / n * 0.4 * 0.4)
onegram <- onegram[, c(1, 3)]
names(onegram) <- c("terms", "fre")
format(object.size(onegram), units = "Mb")
head(onegram)
## Bigram with counter
bigram <- as.data.table(readRDS("./datasets/train/bigramRDS"))
format(object.size(bigram), units = "Mb")
head(bigram)
bigramCount <- as.data.table(readRDS("./datasets/train/bigramCountRDS"))
format(object.size(bigramCount), units = "Mb")
head(bigramCount)
## Trigram with counter
trigram <- as.data.table(readRDS("./datasets/train/trigrams-parts/trigramPruned2RDS"))
format(object.size(trigram), units = "Mb")
head(trigram)
trigram <- trigram[, c(2, 3, 1), with = FALSE]
trigramCount <- as.data.table(readRDS("./datasets/train/trigrams-parts/trigramCountRDS"))
format(object.size(trigramCount), units = "Mb")
head(trigramCount)
##########################################################################################
## Searching algo
candidates <- function(phrase = ""){
        p1 <- phrase
        p2 <- sapply(strsplit(as.character(p1), "_"), tail, 1)
        
        x <- data.table(terms = trigram[root == p1]$terms, 
                        fre = trigram[root == p1]$total / trigramCount[root == p1]$total)
        y <- data.table(terms = bigram[root == p2]$terms, 
                        fre = bigram[root == p2]$total / bigramCount[root == p2]$N * 0.4)
        z <- onegram
        
        df <- rbind(x, y, z)
        df <- setorder(df, -fre)[1:3]
        cand <- sapply(strsplit(as.character(df$terms), "_"), tail, 1)
        return(cand)
}
##########################################################################################
## Splitter algo
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
setwd("~/Documents/finalCapstone/datasets/model")
## Finally save the data cleaned.
dm1 <- cleaner(readLines("./ttm.txt"))
dm2 <- cleaner(readLines("./bbm.txt"))
dm3 <- cleaner(readLines("./nnm.txt"))


sapply(strsplit(as.character(dm1[1:3]), " "), tail, 3)

rootSearch <- sapply(strsplit(as.character(dm1[1:3]), " "), tail, 3)[1:2,]
response <- sapply(strsplit(as.character(dm1[1:3]), " "), tail, 3)[3, ]

dt <- data.table()
for(i in 1:3){
        t <- paste(rootSearch[, 1], "_")
        dt <- rbind
}