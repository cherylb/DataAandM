#  Data Acquisition / Management 0607 02
#  Week Three - Entropy

#  1 entropy function
entropy <- function (d){
  # calculate entropy for data population
  # d = data vector
  d <- as.factor(d)
  x <- table(d)
  n <- length(d)
  p.i <- x / n
  e <- -sum(p.i * log2(p.i))
  return(e)
}

#  2  infogain function
infogain <- function(d,a){
  df <- data.frame(d,a) # put it in data frame
  n <- length(d)  #total n
  a <- as.factor(a)
  x <- table(a) # total for each partition
  p.a <- x/n  #prob of getting a

  calc.entropy <- function(x){
    #x is a data frame with col d
    d <- x$d
    return(entropy(d))
  }
  
  ent.a <- sum(p.a * (by(df, df$a, calc.entropy)))
  gain <- entropy(df$d) - ent.a 
  return(gain)
}


#3 decide function
decide <- function(df, col){
  # df is the entire data frame
  # col is the target col number of df
  
  d <- df[[col]]  # target
  a <- df
  a[[col]] <- NULL  # leaving just attributes
  gain <- sapply(a, infogain, d = d)
  best <- sort(gain)[length(gain)]  
  col.best <- which(colnames(df)==names(best))  # in case more than one best
  z <- list(gain, col.best)
  return(z)
}


library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
fileurl= "https://raw.githubusercontent.com/cherylb/DataAandM/master/entropy-test-file.csv"
data <- getURL(fileurl)
dfmain <- read.csv(text = data)
print("Data looks like: ")
print(head(dfmain,10))

#1
cat("\n#1\n")
target <- dfmain$answer
e <- entropy(target)
cat("entropy of target = answer is: ", e, "\n")

#2
attribute <- dfmain$attr1
cat("\n\n# 2\n")
gain <- infogain(target, attribute)
cat("infogain for attribute 1 is ", gain, "\n")

attribute <- dfmain$attr2
gain <- infogain(target, attribute)
cat("infogain for attribute 2 is ", gain, "\n")

attribute <- dfmain$attr3
gain <- infogain(target, attribute)
cat("infogain for attribute 3 is ", gain, "\n")

#3
cat("\n\n# 3\n")
best <- decide(dfmain, col = 4)
print("infogain for each attribute is: ")
print(best[[1]])
cat("\n column number(s) for attribute with max infogain is: ", best[[2]], "\n")
