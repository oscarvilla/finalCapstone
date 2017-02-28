## Loading dataframes
setwd("~/Documents/finalCapstone/finalModel")
library(data.table)
onegram <- as.data.table(readRDS("./onegramTidyRDS"))
bigram <- as.data.table(readRDS("./bigramTidyRDS"))
trigram <- as.data.table(readRDS("./trigramTidyRDS"))
bigramCount <- as.data.table(readRDS("./bigramCountRDS"))
trigramCount <- as.data.table(readRDS("./trigramCountRDS"))
##########################################################################
## Match engine algo
candidates <- function(phrase = ""){
        p1 <- phrase
        p2 <- sapply(strsplit(as.character(p1), "_"), tail, 1)
        
        tri <- data.table(terms = trigram[root == p1]$terms, 
                        fre = trigram[root == p1]$total / trigramCount[root == p1]$total)
        
        bi <- data.table(terms = bigram[root == p2]$terms, 
                        fre = bigram[root == p2]$total / bigramCount[root == p2]$N)
        o <- onegram
        
        response <- rbind(tri, bi, o)
        
        return(response$terms)
}