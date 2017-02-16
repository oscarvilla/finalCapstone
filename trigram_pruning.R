library(data.table)
setwd("~/Documents/finalCapstone/datasets/train")
library(filehash)
dbCreate("mydb")
x <- dbInit("mydb")
x$terms <- readRDS("./trigramRDS")

library(foreach)
## library(doParallel) ## Looks it's not working. Try with doMC
library(doMC)
registerDoMC(cores = 4)
## Activate 4 clusters for parallelizing the work
## cl <- makeCluster(4)
## registerDoParallel(cl)

l <- nrow(x$terms) / 8
x$terms$t <- rep(seq(1, 8, 1), l)

root <- foreach(i=1:8, .combine = rbind, .packages = c("data.table")) %dopar% {
        
        root <- paste(unlist(strsplit(as.character(x$terms[x$terms$t==i, c("terms"), with = TRUE]), "_"))[seq(1, dim(x$terms[x$terms$t==i, c("terms"), with = TRUE])[1] * 3 - 2, 3)], 
                      unlist(strsplit(x$terms[x$terms$t==i, c("terms"), with = TRUE], "_"))[seq(2, dim(x$terms[x$terms$t==i, c("terms"), with = TRUE])[1] * 3 - 1, 3)], sep = "_")
}

stopCluster(cl)
 

#trigram <- as.data.table(readRDS("./trigramRDS"))


## verify cores
getDoParWorkers()


x$root <- as.vector(root)


trigram <- cbind(trigram, root = root)

t2 <- Sys.time()
t2-t1
stopCluster(cl)