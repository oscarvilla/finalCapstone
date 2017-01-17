library(text2vec)
library(data.table)
library(tm)
mytext <- readLines("/home/oscar/Documents/DSSCapstone/en_US/en_US.twitter.txt", n = 1000000)
## Parallelizing 
N_WORKERS = 3 ## Number of workers or chunks in parallel
splits <- split_into(mytext, N_WORKERS) ## Split the data
jobs <-  lapply(splits, itoken, tolower, word_tokenizer)
t1 = Sys.time()
v1 <- create_vocabulary(jobs, ngram = c(ngram_min = 2L, ngram_max = 2L))
t2 <- Sys.time()
t2 - t1
N_WORKERS = 4 ## Number of workers or chunks in parallel
splits <- split_into(mytext, N_WORKERS) ## Split the data
jobs <-  lapply(splits, itoken, tolower, word_tokenizer)
t1 = Sys.time()
v2 <- create_vocabulary(jobs, ngram = c(ngram_min = 3L, ngram_max = 3L))
t2 <- Sys.time()
t2 - t1

prep_fun = tolower
tok_fun = word_tokenizer
it_train = itoken(mytext, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun,
                  progressbar = TRUE)

t1 = Sys.time()
vocab <- create_vocabulary(it_train, ngram = c(ngram_min = 2L, ngram_max = 2L))
t2 <- Sys.time()
t2 - t1
vocab

head(setorder(vocab$vocab, -terms_counts))
tail(setorder(vocab$vocab, -terms_counts))
head(setorder(vocab$vocab, terms), 100)
tail(setorder(vocab$vocab, terms), 100)
############################################
vectorizer = vocab_vectorizer(vocab)
t1 = Sys.time()
dtm_train = create_dtm(it_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))