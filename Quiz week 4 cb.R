#  Data Acquisition / Management 0607 02
#  Week four - quiz

#  get data in
library(dplyr)
library(reshape2)

(df.movies <- tbl_df(read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="")))

#  1. total movies per decade
by.decade <- data.frame(dec = round(df.movies$year, -1))
(by.decade <- summarise(group_by(counts, dec), count = n())
 
avg.rating <- select(df.movies, title, year, )


# avg for all years
genras = seq(length(df.movies)-6,length(df.movies))
names(genras) = tail(colnames(df.movies), 7)
by.rate <- select(df.movies, year, rating, genras)
m.rate <- melt(by.rate, id = c("year", "rating"))
gp.m.rate <- filter(group_by(m.rate, variable), value != 0)
(sum.rate <- summarise(gp.m.rate, num.rate = sum(value), avg.rate = sum(rating)/sum(value)))


# recasting

dc <- dcast(m.rate, year ~ variable, value.var = "rating", mean)
(dc <- tbl_df(replace(dc, is.nan(dc), 0)))

# make it long again 
x <- melt(dc, id = "year")
x <- filter(x, value != 0)
ggplot(data = x, aes(x = year, y = value, group = variable, colour = variable))+geom_line()

ggplot(data = m.rate, aes(x = year, y = rating, group = variable, colour = variable))
  
#  2.  avg user rating for genra, over time
#
# 3.  length of movie & rating - relationship?
# 4.  genra and rating - relationship?
# 5. other variable preict total numbers of votes