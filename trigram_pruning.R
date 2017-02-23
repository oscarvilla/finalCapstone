library(data.table)
setwd("~/Documents/finalCapstone/datasets/train")
library(data.table)
# 1. Import the data frame
dt <- as.data.table(readRDS("./trigramRDS"))
# 2. Create a factor to split by in chunks
n <- 4 # How many chunks do you want
dt$t <- seq(1, n, 1)
head(dt, 18) 
# 3. Split into chunks
x <- split(dt, dt$t)
# 4. Save the chunks as RDS
# Get in a new dir
setwd("~/Documents/finalCapstone/datasets/train/trigrams-parts")
saveRDS(x[[1]], "./trigram1RDS")
saveRDS(x[[2]], "./trigram2RDS")
saveRDS(x[[3]], "./trigram3RDS")
saveRDS(x[[4]], "./trigram4RDS")
################################################################
library(parallel)
cl <- makeCluster(4)
# here we can star a for loop and iterate by i
root <- data.table()
for(i in 1:n){
        y <- dt$terms
        root <- as.data.table(parLapply(cl, y, function(y) paste(sapply(strsplit(y, split = "_"),'[',1), 
                                                                 sapply(strsplit(y,split= "_"),'[',2), sep = "_")))
        root <- rbind(root, temp)
        
}