library(data.table)
## One gram
onegram <- as.data.table(readRDS("./datasets/train/onegramRDS"))
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

head(bigramCount)