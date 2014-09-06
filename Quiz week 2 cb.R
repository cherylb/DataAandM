#Data Acquisition / Management 0607 02
#Week Two Assignment


# 1 A vector with 20 numbers
odd.vector = c(seq(1,30,2),seq(1,10,2))
cat("A numeric vector: ", odd.vector)

# 2 convert to a char vector
char.vector = as.character(odd.vector)
cat("As a character vector: ", char.vector)

# 3 convert to factor
fact.vector = as.factor(char.vector)
cat("As a factor vector: ", fact.vector)

# 4 show number of levels in factor vector
f.levels = nlevels(fact.vector)
cat("with ", f.levels, " levels")

# 5  perform 3x^2 - 4x +1 on numeric vector
quad.vector = 3 * odd.vector^2 - 4 * odd.vector +1
cat("Numeric vector transfored by 3x^2 - 4x +1")
print(quad.vector)

# 6 matrix least squares regression 
# Read matrix data from a csv file on github allows changing of data if needed
library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
fileurl= "https://raw.githubusercontent.com/cherylb/DataAandM/master/dataQ2N6.csv"
data <- getURL(fileurl)
df <- read.csv(text = data)
y.matrix <- as.matrix(df[4])
x.matrix <- as.matrix(df[1:3])
# 6  regression using matrix operations
b.vector <- solve(t(x.matrix)%*%x.matrix) %*% t(x.matrix) %*% y.matrix  # creats B-hat
cat("B-hat values are", b.vector)
# 6  or - regression using lm function
print("lm function excluding the intercept produces almost the exact same values")
lm(y.matrix ~ x.matrix -1)   #excluding intercept
b2.vector <- coef(lm(y.matrix ~ x.matrix -1))
#compare two methods results
b.compare <- b.vector - b2.vector
cat("Differences between methods are essentially zero: ", b.compare)

#7 Create a list with named elements
a.list <- c(zero = c("mark", "it"), not = 0, eight = (c("rules")))
print("A list with 3 named elements: mark, not, and eight")
print(a.list)

#8 create a data frame with char, factor, numeric, date columns
one.char <- letters[seq(1:10)]
toe.factor <- as.factor(c(rep("calmer",5), rep("than",3), rep("youare", 2)))  #with 3 levels
three.number <- c(rep(0,5),rep(8,5))
four.date <- seq(as.Date("1998/6/3"), as.Date("2007/6/3"), "years")
silly.df <- data.frame(one.char, toe.factor, three.number, four.date, stringsAsFactors = FALSE)
print("An absurd data frame with characters, facrtors, numbers and dates: ")
print(silly.df)

#9 add a row with new factor value
new.row <- c(as.character("k"), "toe", as.numeric(0), "2008-06-03")
levels(silly.df$toe.factor) <-  c(levels(silly.df$toe.factor),"toe")
silly.df <- rbind(silly.df, new.row)
print("Adding a new row with a new factor 'toe' in toe.factor")
print(silly.df)
print(
factor(silly.df$toe.factor))

#  10  read a csv file named temperatures from working directory
temp.df <- read.csv("temperatures.csv")

#  11  read a tsv file from working directory
file.path <- "C:/Users/Cheryl/Desktop/measurements.txt"
meas.df <- read.table(file.path)

#  12 read from website with "|" seperator
fileurl= "https://raw.githubusercontent.com/cherylb/DataAandM/master/pipes.txt"
df <- read.table(fileurl, sep = "|")

#  13 create a loop to calculate 12!
num <- 12 #this could be any positive number, here it is 12
x <- seq(2,num,1)
y <- 1
for (i in 1:length(x)){
    y = x[i]* y
    print(y)
}
cat("12 factorial is ", y, "\n")

#14 loop to calculate balance after 6 years
balance <- 1500  # intial balance
rate <- .0324  # interest
years <- 6  # years
periods <- years * 12  # compounded monthly
n = 0
while ( n < periods){
  balance = balance * (1 + rate)
  n = n+1
}
cat("Balance after 6 years:", round(balance,digits = 2), "\n")

#  15 sum every 3rd value of numeric vector length 20
x <- seq(-15,42,3)  # can change sequence
i <- seq(3,length(x),3)
calc <- sum(x[i])
cat("Original sequence: ", x, "\n")
cat("Sum of every third: ", calc, "\n")

#  16 calculate sum x^i for i is 1 to 10
x <- 2
i <- 1
y <- 0
for (i in 1:10){
  y <- c(y, x^i)
  i <- i + 1
}
cat("sum of x^i for i = 1 to 10 is: " , sum(y) , "\n")


#  17 calculate #16 using while loop
x <- 2
i <- 1
y <- 0
while (i <= 10){
  y <- c(y, x^i)
  i <- i + 1
}
cat("sum of x^i for i = 1 to 10 is: " , sum(y) , "\n")

#  18 calcultate #16 without loops
result <- sum(2^seq(1:10))
cat("sum of x^i for i = 1 to 10 is: " , result , "\n")

#  19 numeric vector by 5's 20 to 50
fives.vector <- seq(20,50,5)

#  20 char vector len 10 , 'example'
word <= "example"[]
example.vector <- rep(word, 10)
cat("Recursive example of examples with length: ", length(example.vector), "\n")
print(example.vector)

#  21 input a, b, c and quadratic equation in

input.values <- c(readline("Enter numeric values for a, b, c seperated by a comma: "))  #tested ok 1,3,-4

x <- as.numeric(unlist(strsplit(input.values,",")))
a <- x[1]
b <- x[2]
c <- x[3]

root1 <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
root2 <- (-b - sqrt(b^2 - 4*a*c))/(2*a)

cat("root one is: ", root1, "\n")
cat("root two is: ", root2, "\n")

  
