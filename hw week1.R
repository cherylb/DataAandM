#Data Aquisition / Managment 0607 02
#Week One Assignment

# 1. What versions of R and RStudio do you have installed?
  # R version 3.1.1 (2014-07-10) -- "Sock it to Me"
  # R Studio Version 0.98.507 

# 2. What version of PostgreSQL do you have installed?
  # PostgreSQL version 9.3

# 3. Install and load the R package DMwR. 
#    Load the data set sales and determine the number of observations contained in the data set.

library(DMwR)
# number of observations
print(paste("Number of observations: ", nrow(sales)))
# data head and summary
print("First rows:")
print(head(sales))
print("Summary")
print(summary(sales))
