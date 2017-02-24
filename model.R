library(data.table)
setwd("~/Documents/finalCapstone")
## One gram
onegram <- as.data.table(readRDS("./datasets/train/onegramRDS"))
n <- sum(onegram$total)
## Reduce the sample of onegrams, because we'll use, at the most, just the three more frequent
onegram <- data.table(terms = onegram[1:3], 
                      fre = onegram[1:3]$total / n * 0.4 * 0.4)
onegram <- onegram[, c(1, 3)]
names(onegram) <- c("terms", "fre")
##format(object.size(onegram), units = "Mb")
##head(onegram)
## Bigram with counter
bigram <- as.data.table(readRDS("./datasets/train/bigramRDS"))
##format(object.size(bigram), units = "Mb")
##head(bigram)
bigramCount <- as.data.table(readRDS("./datasets/train/bigramCountRDS"))
##format(object.size(bigramCount), units = "Mb")
##head(bigramCount)
## Trigram with counter
trigram <- as.data.table(readRDS("./datasets/train/trigrams-parts/trigramPruned2RDS"))
##format(object.size(trigram), units = "Mb")
##head(trigram)
trigram <- trigram[, c(2, 3, 1), with = FALSE]
trigramCount <- as.data.table(readRDS("./datasets/train/trigrams-parts/trigramCountRDS"))
##format(object.size(trigramCount), units = "Mb")
##head(trigramCount)
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