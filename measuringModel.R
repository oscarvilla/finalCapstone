f <- data.table()
n <- 9495
for(i in 1:n){
        v <- r[i]
        k <- candidates(v)
        t <- cbind(y1 = k[1], y2 = k[2], y3 = k[3], y = y[i])
        f <- rbind(f, t)
}
f$r <- (f$y1 == f$y || f$y2 == f$y || f$y3 == f$y) * 1
sum(f$r) / n




rm(list = c("t", "f", "i", "k", "v"))
