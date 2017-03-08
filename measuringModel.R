f <- data.table()
n <- length(x)

library(foreach)
library(parallel)
library(doParallel)
registerDoParallel(cores=detectCores(all.tests=TRUE))

predict <- foreach(i=1:n, .combine = rbind) %dopar% {
        v <- r[i]
        k <- candidates(v)
        t <- cbind(y1 = k[1], y2 = k[2], y3 = k[3], y = y[i])
        f <- rbind(f, t)
}
predict2 <- predict
predict2$r <- (predict2$y1 == predict2$y | predict2$y2 == predict2$y | predict2$y3 == predict2$y) * 1
sum(predict2$r) / n