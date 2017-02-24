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
dm <- as.character(rbind(as.vector(dm1), as.vector(dm2), as.vector(dm3)))

sapply(strsplit(as.character(dm1[1:3]), " "), tail, 3)
saveRDS(dm, "./dmRDS")
##############################################################################
setwd("~/Documents/finalCapstone/datasets/model")
df <- readRDS("./dmRDS")
library(data.table)

x <- df[1:10000]
x <- x[sapply(gregexpr("\\S+", x), length) > 2] ## erase the phrases with less than two words
                                                ## This makes mess me
library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)


r <- parApply(cl = cl, sapply(strsplit(as.character(x), " "), tail, 3)[1:2,], 2, 
           function(x) {y <- paste(x[1], x[2], sep = "_")})
y <- sapply(strsplit(as.character(x), " "), tail, 3)[3, ]

stopCluster(cl)

##########################################################################