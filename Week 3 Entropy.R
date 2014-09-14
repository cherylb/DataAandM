#  Data Acquisition / Management 0607 02
#  Week Three - Entropy

entropy <- function (d){
  # calculate entropy for data population
  # d = data vector
  d <- as.factor(d)
  x <- table(d)
  n <- length(d)
  p.i <- x / n
  e <- -sum(p.i * log2(p.i))
}

infogain <- function(d,a){
  df <- data.frame(d,a) # put it in data frame
  n <- length(d)  #total n
  a <- as.factor(a)
  x <- table(a) # total for each partition
  p.a <- x/n  #prob of getting a

  calc.entropy <- function(x){
    #x is a data frame with col d
    d <- x$d
    print(head(d, 10))
    print(class(d))
    return(entropy(d))
  }
  
  ent.a <- p.a * as.numeric(by(df, df$a, calc.entropy))
  ent.a <- sum(ent.a * log2(ent.a))

  gain <- entropy(df$d) - ent.a

  
  
  
  
  
}


fileurl= "https://raw.githubusercontent.com/cherylb/DataAandM/master/entropy-test-file.csv"
data <- getURL(fileurl)
df.model <- read.csv(text = data)
