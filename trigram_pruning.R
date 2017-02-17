library(data.table)
setwd("~/Documents/finalCapstone/datasets/train")
trigram <- as.data.table(readRDS("./trigramRDS")$terms)

library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
library(foreach)
library(doParallel)
## Activate 3 clusters for parallelizing the work
cl <- makeCluster(3)
registerDoParallel(cl)
makeForkCluster(nnodes = getOption("mc.cores", 3L))

x <- trigram

m <- nrow(x) * 3 - 1
n <- nrow(x) * 3
x$root <- parLapply(x, function(x) paste(unlist(strsplit(as.character(x), split = "_"))[seq(1, m, 3)], 
                                      unlist(strsplit(as.character(x), split = "_"))[seq(2, m, 3)]))

stopCluster(cl)



root <- foreach(i=1:8, .combine = rbind, .packages = c("data.table")) %dopar% {
        k <- x[t==i, c("V1"), with = FALSE]
        root <- paste(apply(k, 1, FUN = function(x) strsplit(as.character(x), split = "_")[[1]][1]), 
                      apply(k, 1, FUN = function(x) strsplit(as.character(x), split = "_")[[1]][2]))
}


 

#trigram <- as.data.table(readRDS("./trigramRDS"))


## verify cores
getDoParWorkers()


x$root <- as.vector(root)


trigram <- cbind(trigram, root = root)

t2 <- Sys.time()
t2-t1
stopCluster(cl)