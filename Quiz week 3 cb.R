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
  z <- z[duplicated(z)]
  return(max(z))
}
input.values <- c(readline("To find the largest common divsor,enter two numeric 
                           vaues to test seperated by a comma: "))
cat ("Greatest common divsor of (", input.values, ") is: ", calcDivsor(input.values), "\n")


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
    }
    return(old.r)
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
cat("the result of the equation is: ", calcVal(input.values), "\n")


# 6 merge two data files
# Read matrix data from a csv file on github allows changing of data if needed
library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
fileurl= "https://raw.githubusercontent.com/cherylb/DataAandM/master/week-3-price-data.csv"
data <- getURL(fileurl)
df.price <- read.csv(text = data)

fileurl= "https://raw.githubusercontent.com/cherylb/DataAandM/master/week-3-make-model-data.csv"
data <- getURL(fileurl)
df.model <- read.csv(text = data)
df <- merge(df.model, df.price, by= "ModelNumber")
cat("Number of rows in merged data frame is: ", nrow(df), " - that was unexpected...\n")
    # not expected would have expected 28 rows, the same as in df.price

#7 merge same files so all price data and model data appear
df <- merge(df.model, df.price, by= "ModelNumber", all = TRUE)
cat("Number of rows now in merged data frame is: ", nrow(df), "\n")

#8 subset only 2010
df.tens <- subset(df, Year == 2010)
print("Only 2010:")
print(df.tens)

#9 subset only red> $10k
df.red <- subset(df, Price > 10000 & Color == "Red")
print("Only Red and > $10,000")
print(df.red)

#10 No Model num and color
df.trim <- df.red[ ,!names(df.red) %in% c("Color", "ModelNUmber")]
print("Exclude color and model number")
print (df.trim)

#11 create vector to count number of characters in each element of char vector
word.length <- function(input.value){
  x <-strsplit(input.value," ")
  y <- nchar(x[[1]])
  names(y) <- x[[1]]
  return(y)
}
input.values <- c(readline("Enter a character vector of some kind: "))
print("The word lengths in the vector are: ")
print(word.length(input.values))


#12  compare two same length vectors
compare <- function(in.a, in.b){
  x <-unlist(strsplit(in.a, "\\, |\\,| "))  #splits for comma or space
  y <-unlist(strsplit(in.b, "\\, |\\,| "))
  if (length(x) != length(y)){
    return(NULL)
    break
  }
  return(paste(x,y))
}

in.a  <- c(readline("Enter a character vector of some kind: "))
in.b  <- c(readline("Enter another character vector of same length:  "))
print("The combined vector is: ")
if(is.null(compare(in.a,in.b))){
  print("not possible to calculate. Vecors are of unequal length.")
} else {
  print(compare(in.a,in.b))
}


# 13 extract a substring starting with first vowel and 3 letters long
voul <- function(in.a){
  if(length(in.a) > 1) {  # if input has more than one element, combine to single string
    in.a <- paste(in.a,collapse = " ")
  }
  pat <- "[AEIOUaeiou]{1}.."
  i <- str_extract(in.a,pat)
  if (is.na(i)){
    return("No string begining with a vowel and 3 char long was found.")
    break
  }
  return(paste("The first string begining with a vowel and 3 char long is: ",i))
}
in.a  <- c(readline("Enter a character string of some kind: "))
voul(in.a)


#14 create data frame
months <- sample(1:12, 20, replace = T)   #random months
years <- sample(2015:2100, 20, replace = F)
days <- sample(1:28, 20, replace = T)
df.dates <- data.frame(months,years,days)
df.dates$USdate <- as.Date(paste(df.dates$months,df.dates$days,df.dates$years, sep = "-"),"%m-%d-%y")
print("class of columns in df: ")
print(lapply(df.dates,class))
print(df.dates)

#15  convert a string to date
strdate <- "04-23-3002"
strformat <- "%m-%d-%y"
new.date <- as.Date(strdate,strformat)

#16 extract date parts m, d, y - going to borrow new.date from above
month <- format.Date(new.date, "%m")

#17 sequence of dates
dates <- seq(as.Date("2005/1/1"), as.Date("2014/12/31"), "days")
