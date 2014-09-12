#  Data Acquisition / Management 0607 02
#  Week Three Quiz

#  1  mean of numeric vector
mean.function <- function(num.vec){
  #  input a numeric vector, sums and divides by length
  m <- sum(num.vec)/length(num.vec)
  return(m)
}
x <- c(1, 3, 5)
mean.function(x) == mean(x)  # checking against built in


# 2  mean of vector with missing values (NA's)
mean.function <- function(num.vec){
  num.vec <- num.vec[is.na(num.vec) ==  FALSE]
  cat("Vector excluding missing values: ", num.vec, "\n")
  m <- sum(num.vec)/length(num.vec)
  return(m)
}
x <- c(1, 3, NA, 5, 8)
cat("mean of ", x, " excluding NA's is: " , mean.function(x) , "\n")


#3 find common factor of two numeric values - works for longer lists as well
calcDivsor <- function(input.values){
  x <- abs(as.numeric(unlist(strsplit(input.values,","))))
  test.num <- sort(rep(seq(1,min(x)),length(x))) # all possible integers, repeated for lenght x
  z <- test.num[x %% test.num == 0]
  return(max(z[duplicated(z)])
}
input.values <- c(readline("To find the largest common divsor,enter two numeric 
                           vaues to test seperated by a comma: "))
cat ("Greatest common divsor of (", x, ") is: ", calcDivsor(input.values), "\n")


#4 using Euclids
calcEuclid <- function(input.values) {
  x <- abs(as.numeric(unlist(strsplit(input.values,","))))
  a <- min(x)  # lower number  starting r -2
  b <- max(x)  # higher number starting r -1 
  r <- b %% a
  if (r == 0){
    return(a)  # if one is divisible by other, lower num. is largest factor
  } else {
    q <- b %/% a #starting q.start
    while (r != 0) {
      b <- a
      a <- r
      q <- b %/% a
      old.r = r
      r <- b - a * q
      return(old.r)
    }    
  }
}
input.values <- c(readline("To use Eucilds algorithim, enter two numeric vaues to seperated by a comma: "))
cat("the greatest common factor is: ", calcEuclid(input.values), "\n")
  

# 5 calculate value based on two inputs
calcVal <- function(input.values){
  z <- as.numeric(unlist(strsplit(input.values,",")))
  x <- z[1] 
  y <- z[2]
  return(x^2 * y + 2 * x * y - x*y^2)
}
input.values <- c(readline("Enter two numeric vaues for x, y seperated by a comma: "))
cat("the result is: ", calcVal(input.values), "\n")

}


