#  Week 4 Assignment
#  using rvest to pull data from a web page. In this case rvest is looking at itself

#-------------------------------------------------------------------------------------------

library(rvest)

url <- html("https://github.com/hadley/rvest")

#  description

desc <- html_nodes(url, ".repository-description p")
print("Short Description:")
print(html_text(desc))


totalcom <- as.numeric(html_text(html_nodes(url, ".commits .text-emphasized")))
print("Total number of commits:")
print(totalcom)


#summary readme
print("Summary:")
(readme <- html_text(html_nodes(url,"p +p")))

#pulling the tables
tables <- html_nodes(rvest.url,"table")
print ("Contents Available:")
count.tables = length(tables)
print(contents <- html_table(tables[[1]])[2:4])
