#  Data Acquisition / Management 0607 02
#  Week four - quiz

#  get data in
library(dplyr)
library(reshape2)

(df.movies <- tbl_df(read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="")))
#-------------------------------------------------------------------------------------------
#  1. total movies per decade
#-------------------------------------------------------------------------------------------
print("#1 Total Movies Per Decade")
by.decade <- data.frame(dec = round(df.movies$year, -1))
(by.decade <- summarise(group_by(by.decade, dec), count = n()))
 
#-------------------------------------------------------------------------------------------
#  2.  avg user rating for genre, over time
#-------------------------------------------------------------------------------------------

# avg for all years
genres = seq(length(df.movies)-6,length(df.movies))
names(genres) = tail(colnames(df.movies), 7)
by.rate <- select(df.movies, year, rating, genres)
m.rate <- melt(by.rate, id = c("year", "rating"))
gp.m.rate <- filter(group_by(m.rate, variable), value != 0)
cat("\n\n#2a  Average rating by genre for all years\n")
(sum.rate <- summarise(gp.m.rate, num.rate = sum(value), avg.rate = sum(rating)/sum(value)))

dc <- dcast(gp.m.rate, year ~ variable, value.var = "rating", mean)
cat("\n\n#2b  Average rating per genre for each year\n")
(dc <- tbl_df(replace(dc, is.nan(dc), 0)))
# make it long again for some plots
x <- melt(dc, id = "year")
x <- filter(x, value != 0)
cat("When viewed togehter the average rating for all genres does not signficantly vary over time.\n")
e <- ggplot(data = x, aes(x=year, y= value))
(e + geom_point(aes(colour = variable))+geom_smooth(method = lm)+ labs(x="Year", y = "Rating"))
print("Summary")
(summary(lm ( x$value ~ x$year)))
        
cat("\nThe indivdual genres do show changes over time.\n",
    "Action is decreasing rapidly over time \n",
    "Romance ratings are decreasing slightly \n",
    "Documentary and Short are both increasing \n",
    "Comedy has a more complex relationship, that could best be described by either a\n",
    "moving averge of two levels, a sine curve, or a 3rd order ploynomial \n",
    "Animation films are holding fairly level\n")
p2 <- qplot(data = x, x = year, y = value, colour = variable, facets = ~ variable)
(p2 +stat_smooth(method = "lm"))

#-------------------------------------------------------------------------------------------
# 3.  length of movie & rating - relationship?
#     removing the 9 movies length > 500 (less than 0.01% of the population )
#-------------------------------------------------------------------------------------------
cat("\n\n#3  There is no sigificante rlationship between the length of the movies and the average rating\n")
(p3 <- qplot(data = filter(df.movies, length < 500), x = length,  y = rating ))
aov.out <- aov(rating ~ length, data = filter(df.movies, length < 500))
summary.lm(aov.out)
summary(lm(rating ~ length, data = filter(df.movies, length  < 500)))



#-------------------------------------------------------------------------------------------
# 4.  genre and length - relationship?
#-------------------------------------------------------------------------------------------
cat("\n\n#4  The length is related to genre, each genre has a different average length\n")
cat("It is very unlikely the means are the same across genres\n")
m.len <- filter(melt(select(df.movies, length, genres), id = "length"), value != 0, length < 500)
aov.out <- aov(value ~ variable, data = m.len)
summary.lm(aov.out)
s.len <- summarise(group_by(m.len, variable), avg.len = mean(length))
ggplot(data = s.len, aes(x = variable, y = avg.len)) + geom_bar(stat = "identity") + 
  geom_hline(yintercept = mean(s.len$avg.len))


#-------------------------------------------------------------------------------------------
# 5. other variable preict total numbers of votes
#-------------------------------------------------------------------------------------------
cat("\n\n#5  The year is the best predictor of the number of votes\n")
print("summary")
sum.votebyyear <- summarise(group_by(m.vote, year), yearct = sum(value), avg.votes = sum(votes)/sum(value))
(summary(sum.votebyyear))
(qplot(data=sum.votebyyear, x = year, y = log(avg.votes)) + stat_smooth(method = "lm"))
(summary(lm(data = sum.votebyyear, log(avg.votes)~ year)))
cat("\n\n Layering in the genre could also be helpful for some genres\n")
sum.votebyyear <- summarise(group_by(m.vote, year, variable), yearct = sum(value), avg.votes = sum(votes)/sum(value))
print("summary")
summary(sum.votebyyear)
p2 <- qplot(data = v, x = year, y = log(avg.votes), colour = variable, facets = ~ variable)
(p2 +stat_smooth(method = "lm"))
