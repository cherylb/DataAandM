#  Data Acquisition / Management 0607 02
#  Week Three Assignment

# 1 function returns missing values in input
countMissing <- function(input){
   d <- input[is.na(input) != FALSE]
   return(length(d))
}

input <- c(seq(1:5), NA, NA)
r <- countMissing(input)
cat("\n#1\n")
cat("Number of missing values in ,", input, "is ", r, "\n")


# 2 function returns missing values in each col of data frame
countdfMissing <- function(input){
  z<- sapply(input, function(x) countMissing(x))
  return(z)
}
suppressWarnings(num.nas <- as.numeric(c("Not", "a", "number", 33, 44)))  # forceing NA on purpose, don't want warning
char.nas <- c(NA, "A", "BNA", "NA", NA)
boo.nas <- as.logical(c(TRUE, FALSE, TRUE, NA, FALSE))
df.test <- data.frame(num = num.nas, char = char.nas, boo = boo.nas, stringsAsFactors = FALSE)
cat("\n#2\n")
print("data frame is")
print(df.test)
print("missing values in data frame:")
print(countdfMissing(df.test))


# 3  min, max, mean, median, first quartile, third quartile, sd, num missing of numeric vector
someStats <- function(input){
  #sort it
  x <- sort(input)  #this removes any NA values
  l <- length(x)
  
  #  min
  min.stat <- x[1]  #lowest in list
  
  #max
  max.stat <- x[l]
  
  #mean
  if (length(x) != 0){
    mean.stat <- sum(x) / l
  } else {
    mean.stat <- 0
  }
  
  # median
  calc.med <- function(x){
    l <- length(x)
    if ( l %% 2 == 0){  # is even
      (med <- (x[l %/% 2] + x[l %/% 2 +1])/2)
    } else { 
      med <- x[l %/% 2 + 1]
      return(med)
    }
  }
  
  med.stat <- calc.med(x)
 
  # quartiles - m&m method
  l = length(x)
  m = l %/% 2
  if ( l %% 2 == 0){  # is even?, include median
    low <- x[1:m]
    up <- x[(m+1):l]
  } else {            # is odd, exclude median
    low <- x[1:m]
    up <- x[(m + 2):l]
  }
  
  
  #x <- seq(1,20,2)  #test even
  quart.one <- calc.med(low)
  quart.three <- calc.med(up)
  
  
  #standard dev
  
  sd.stat <- sqrt(sum((x - mean.stat)^2)/(length(x) - 1))
  #check
  sd.stat == sd(x)
  
  #  missing data
  missing <- countMissing(input)
  
  the.stats <- list(mean = mean.stat, min = min.stat, max =max.stat, 
                    med = med.stat, Q1 = quart.one, Q3 =quart.three, stand.dev = sd.stat, data.missing = missing)
  return(the.stats)
}
cat("\n#3\n")
x <- c(sample(1:100,20, replace = T), NA, NA, seq(1:3))
stats <- someStats(x)
cat("Stats for example x: ", x, "\n")
print(stats)


# 4  compare character or factor vectors for distinct, common, missing values
aboutChar <- function(input){
  # count distinct 
  if( is.character(input)){
    input <- as.factor(input)  #if it's a character convert to a factor
  }
  num.distinct = length(levels(input))
  
  #most common, and number
  x <- table(input)
  n <- length(x[x == max(x)])  # number of max values
  l <- length(x)
  pos <- which(x == sort(x)[l])  #position of 
  count.common <- as.integer(max(x))  #count of most common item(s)
  most.common <- names(x[pos])  #most common item(s)
  
  z<- list(distinct = num.distinct, common = most.common, num.common = count.common)
  return(z) 
}
cat("\n#4\n")
input <- c("A", "A", "B","B","C", "B", "C","C","D")
r <- aboutChar(input)
cat("info about input: ", input, "\n")
cat("Number of distinct elements: ", r$distinct, "\n")
cat("Most common element(s): ", r$common, "\n")
cat("Number common element(s) repeated: ", r$num.common, "\n")

# 5 logical vector - number true, false, prop true, missing
infoLogical <- function(input){
  x <- sort(input)  # remove missing values
  num.true <- length(x[x == TRUE])
  num.na <- countMissing(input)
  num.false <- length(x[x == FALSE])
  prop.true <- num.true/(num.true + num.false)  # excluding missing values
  z <- list(number.true = num.true, number.false = num.false, propor.true = prop.true, num.missing = num.na)
  return(z)
}
input4 <- c(rep(c(TRUE, FALSE, NA),3.5), TRUE, TRUE)
r <- infoLogical(input)
cat("\n#5\n")
cat("info about logical input: ", input, "\n")
cat("Number of TRUE: ", r$number.true, "\n")
cat("Number of FALSE: ", r$number.false, "\n")
cat("Proportion of TRUE to FALSE: ", r$propor.true, "\n")
cat("Number of missing values: ", r$num.missing, "\n")


#  6 info about data frame using char, num,logical vectors
getClass <- function(dfcol){
  if (is.numeric(dfcol)){
    info <- someStats(dfcol)
  } else {
    if(is.character(dfcol) | is.factor(dfcol)){
      info <- aboutChar(dfcol) 
      } else {
        if(is.logical(dfcol)) {
          info <- infoLogical(dfcol)
          }
        }
      }
  csummary <- list(info)
  return(csummary)
}

infoDf <- function(input){
  missing <- countdfMissing(df)
  
  z<- sapply(input, getClass)
  z <- lapply(z, data.frame)
  names(z) = colnames(df)
  return(z)
}

# data frame
num <- sample(1:15,10, replace = T)
char <- sample(LETTERS[1:5], 10, replace = T)
x <- rep(c(TRUE, FALSE, TRUE, NA), 10)
boo <- sample(x, 10, replace = T)
df <- data.frame(num, char, boo)
cat("\n#1\n")
x <- infoDf(df)
print("data: ")
print(df)
print("missing values in data frame:")
print(countdfMissing(df))
print("summary of data for each column: ")
print(x)

