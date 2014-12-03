# week 12 quiz

library(microbenchmark)
#simple function

totals <- function(){
  n <- rnorm(10)
  x <- sum(n)
  y <- mean(n)
  results <- c(x,y)
  return(results) 
}

totals()

#time function
m <- 1:10000

#using system.time
system.time(for(i in m) totals())

#using microbenchmark
microbenchmark(totals(),times = 10000)

