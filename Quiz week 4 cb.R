#  Data Acquisition / Management 0607 02
#  Week four - quiz

#  get data in
library(dplyr)
library(reshape2)

(df.movies <- tbl_df(read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="")))

#  1. total movies per decade
by.decade <- data.frame(dec = round(df.movies$year, -1))
(by.decade <- summarise(group_by(by.decade, dec), count = n()))
 

#  2.  avg user rating for genra, over time

# avg for all years
genras = seq(length(df.movies)-6,length(df.movies))
names(genras) = tail(colnames(df.movies), 7)
by.rate <- select(df.movies, year, rating, genras)
m.rate <- melt(by.rate, id = c("year", "rating"))
gp.m.rate <- filter(group_by(m.rate, variable), value != 0)
(sum.rate <- summarise(gp.m.rate, num.rate = sum(value), avg.rate = sum(rating)/sum(value)))


# recasting by year by genra

dc <- dcast(m.rate, year ~ variable, value.var = "rating", mean)
(dc <- tbl_df(replace(dc, is.nan(dc), 0)))

# make it long again for some plots
x <- melt(dc, id = "year")
x <- filter(x, value != 0)
e <- ggplot(data = x, aes(x=year, y= value))
e + geom_point(aes(colour = variable, size = 3))+geom_smooth(method = lm)+ labs(x="Year", y = "Rating")
qplot(data = x, x = year, y = value, color = variable)
head(x)
summary(lm ( x$value ~ x$year)
print("The average rating for all genras does not signficantly vary over time.\n")


qplot(data = x, x = year, y = value, colour = variable, facets = ~ variable)
cat("The indivdual genras do show changes over time.\n")
cat("Action is decreasing rapidly over time \n"
    "Romance ratings are decreasing slightly \n"
    "Documentary and Short are both increasing \n"
    "Comedy has a more complex relationship, that could best be described by either a\n"
    "moving averge of two levels, a sine curve, or a 3rd order ploynomial \n"
    "Animation films are holding fairly level"
p2 <- qplot(data = z, x = year, y = value, colour = variable, facets = ~ variable)
p2 +stat_smooth(method = "lm")  
                

#
# 3.  length of movie & rating - relationship?
# 4.  genra and rating - relationship?
# 5. other variable preict total numbers of votes