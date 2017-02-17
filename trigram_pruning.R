library(data.table)
setwd("~/Documents/finalCapstone/datasets/train")
trigram <- as.data.table(readRDS("./trigramRDS")$terms)

library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

x <- trigram[1:10000000]

m <- nrow(x) * 3 - 1
n <- nrow(x) * 3
x$root <- parLapply(cl, x, function(x) paste(unlist(strsplit(as.character(x), split = "_"))[seq(1, 29999999, 3)], 
                                      unlist(strsplit(as.character(x), split = "_"))[seq(2, 30000000, 3)], 
                                      sep = "_"))

stopCluster(cl)

####################################################################################################
root <- foreach(i=1:8, .combine = rbind, .packages = c("data.table")) %dopar% {
        k <- x[t==i, c("V1"), with = FALSE]
        root <- paste(apply(k, 1, FUN = function(x) strsplit(as.character(x), split = "_")[[1]][1]), 
                      apply(k, 1, FUN = function(x) strsplit(as.character(x), split = "_")[[1]][2]))
}
####################################################################################################