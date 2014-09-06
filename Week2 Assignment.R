#  Data Acquisition / Management 0607 02
#  Week Two Assignment


#  1 Five customers James Mary Steve Alex Patricia

#  a make queue
queue <- c("James", "Mary", "Steve", "Alex", "Patricia")
cat("\n#1\n")
cat ("People in line to start are: ", queue, "\n")

#  b Harlod shows up
queue <- append(queue, "Harold")  # back of the line, Harlod
cat ("Harold makes his entrance: ", queue, "\n")

#  c James checks out
queue <- queue[2:6]
cat ("James checks out: ", queue, "\n")

#  d Pam is slick
queue <- append(queue, "Pam", 1)
cat ("Fast-talking Pam arrives: ", queue, "\n")

#  e Harlod has to go
queue <- queue[2:6]
cat ("Harlod takes off: ", queue, "\n")

#  f Alex has a meeting with Harlod
queue <- queue[queue != "Alex"]
cat ("Alex leaves also: ", queue, "\n")

#  g where's Patricia
cat("Patricia is in the ", match("Patricia", queue), "position. \n")

#  h how many are still waiting
cat("There are now ", length(queue), " people in queue. \n")


#  2 quiz #21 revisited

cat("\n#2\n")
input.values <- c(readline("Enter numeric values for a, b, c seperated by a comma: "))  #tested ok 1,3,-4

x <- as.numeric(unlist(strsplit(input.values,",")))
a <- x[1]
b <- x[2]
c <- x[3]

equation <- sprintf("%+1.f * x^2 %+1.f * x %+1.f", a, b, c)

disc <- b^2 - 4*a*c

if (disc == 0){
  root1 = -b/2*a
  cat("The equation ", equation, " has one root of ", root1, "\n")
} else if(disc > 0){
  root1 <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
  root2 <- (-b - sqrt(b^2 - 4*a*c))/(2*a)
  cat("The equation ", equation, " has two roots. \n")
  cat("root one is: ", root1, "\n")
  cat("root two is: ", root2, "\n")
} else {
  root1 <- (-b + sqrt(as.complex(b^2 - 4*a*c)))/(2*a)
  root2 <- (-b - sqrt(as.complex(b^2 - 4*a*c)))/(2*a)
  cat("The equation ", equation, " returns complex roots \n")
  cat("root one is: ", root1, "\n")
  cat("root two is: ", root2, "\n")
}


#  3  numbers 1 - 1000 not by 3,7,11
cat("\n#3\n")
x <- seq(1,1000)  # 1 - 1000
z <- c(3, 7, 11)  # not divisible by these numbers

y <- x[x%%z[1] * x%%z[2] * x%%z[3] != 0]  # x mod 3, 7 ,11 are all not zero
print("The numbers from 1 to 1000 that are not divisible by 3, 7, or 11:")
print(y)


#  4 test 3 numbers for Pythagorean triple 
cat("\n#1\n")
input.values <- c(readline("Enter three numeric vaues to test seperated by a comma: "))

x <- as.numeric(unlist(strsplit(input.values,",")))
x <- sort(x)

if(x[3]^2 == (x[1]^2 + x[2]^2)){
  cat("The numbers: ", input.values, " form a Pythagorean triple. \n")
} else {
  cat("The numbers: ", input.values, " do not form a Pythagorean triple. \n")
}

