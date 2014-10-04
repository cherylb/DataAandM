# week6quiz.R
# [For your convenience], here is the provided code from Jared Lander's R for Everyone, 
# 6.7 Extract Data from Web Sites

install.packages("XML")
require(XML)
theURL <- "http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool/"
bowlPool <- readHTMLTable(theURL, which = 1, header = FALSE, stringsAsFactors = FALSE)
bowlPool

# 1. What type of data structure is bowlpool? 
# A. bowlPool is a data.frame

# 2. Suppose instead you call readHTMLTable() with just the URL argument,
# against the provided URL, as shown below

theURL <- "http://www.w3schools.com/html/html_tables.asp"
hvalues <- readHTMLTable(theURL)

# What is the type of variable returned in hvalues?
# hvalues returns a list, with data.frame and Null values

# 3. Write R code that shows how many HTML tables are represented in hvalues
(num.dfs <- length(hvalues[which(lapply(hvalues,class) == "data.frame")]))


# 4. Modify the readHTMLTable code so that just the table with Number, 
# FirstName, LastName, # and Points is returned into a dataframe
#  easy way if table position in known:
hvalues <- readHTMLTable(theURL, which = 1)
#  hard way... is marvelous but too large to fit in this margin

# 5. Modify the returned data frame so only the Last Name and Points columns are shown.
(hvalues <- select(hvalues, `Last Name`, Points))

# 6 Identify another interesting page on the web with HTML table values.  
new.url <- "http://spectrumhealthblogs.org/you-me-and-pt/2013/08/27/hula-hooping-fad-legitimate-exercise-device/"
(hula.tables <- readHTMLTable(new.url))

# 7 How many HTML tables does that page contain?
(num.dfs <- length(hula.tables[which(lapply(hula.tables,class) == "data.frame")]))

# 8 Identify your web browser, and describe (in one or two sentences) 
# how you view HTML page source in your web browser.

# I am currently using firefox browser, I can look at the HTML page source by going to developer tools and then
# using the 'inspector' tool. This is especialy nice because it splits the screen with the top showing the web page and the bottom
# showing the HTML, and as you move around the page it highlights the code below. If I want to just see all the source code I can
# select the developer --> page source (ctrl+U) option, which is handy but not as easier for a beginner to read through. 