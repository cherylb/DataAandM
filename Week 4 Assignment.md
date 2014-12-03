Week 4 Assignment - Using Movies data set
Which year were the best popular movies made?

========================================================
Definitions:
'Best' movies are defined here as movies with the highest average rating within each genre. It is important to define what is best for each genre because each genre has a distinct mean, and a 'best' average rating in one particular genre may not be a 'best' rating in another.

'Popular' movies are defined here as movies receiving a high number of votes, inferring that movies with a large number of votes were seen by a corresponding large number of people, and can be considered popular. 
Both the average rating and the number of votes are normally distributed. 


### Import Data

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Source: local data frame [58,788 x 24]
## 
##                       title year length budget rating votes   r1   r2  r3   r4   r5   r6   r7   r8   r9  r10 mpaa
## 1                         $ 1971    121     NA    6.4   348  4.5  4.5 4.5  4.5 14.5 24.5 24.5 14.5  4.5  4.5     
## 2         $1000 a Touchdown 1939     71     NA    6.0    20  0.0 14.5 4.5 24.5 14.5 14.5 14.5  4.5  4.5 14.5     
## 3    $21 a Day Once a Month 1941      7     NA    8.2     5  0.0  0.0 0.0  0.0  0.0 24.5  0.0 44.5 24.5 24.5     
## 4                   $40,000 1996     70     NA    8.2     6 14.5  0.0 0.0  0.0  0.0  0.0  0.0  0.0 34.5 45.5     
## 5  $50,000 Climax Show, The 1975     71     NA    3.4    17 24.5  4.5 0.0 14.5 14.5  4.5  0.0  0.0  0.0 24.5     
## 6                     $pent 2000     91     NA    4.3    45  4.5  4.5 4.5 14.5 14.5 14.5  4.5  4.5 14.5 14.5     
## 7                   $windle 2002     93     NA    5.3   200  4.5  0.0 4.5  4.5 24.5 24.5 14.5  4.5  4.5 14.5    R
## 8                      '15' 2002     25     NA    6.7    24  4.5  4.5 4.5  4.5  4.5 14.5 14.5 14.5  4.5 14.5     
## 9                       '38 1987     97     NA    6.6    18  4.5  4.5 4.5  0.0  0.0  0.0 34.5 14.5  4.5 24.5     
## 10                  '49-'17 1917     61     NA    6.0    51  4.5  0.0 4.5  4.5  4.5 44.5 14.5  4.5  4.5  4.5     
## ..                      ...  ...    ...    ...    ...   ...  ...  ... ...  ...  ...  ...  ...  ...  ...  ...  ...
## Variables not shown: Action (int), Animation (int), Comedy (int), Drama (int), Documentary (int), Romance (int), Short
##   (int)
```
### Each genre has a different distribution of ratings and votes, so it is important to consider them seperatly

Votes - comparison of means
The anova results in rejection of the hypothesis that the theoretical mean is the same across genres
A simple bar chart illustrates the variability between genres

```r
genres = seq(length(df.movies)-6,length(df.movies))
names(genres) = tail(colnames(df.movies), 7)

m.vote <- filter(group_by(melt(select(df.movies,votes, rating, genres),
                               id =  c("rating", "votes")),variable),value!=0)
m.vote  <- filter(melt(select(df.movies, votes, genres), id = "votes"), value != 0)
aov.out <- aov(value ~ variable, data = m.vote)
summary.lm(aov.out)
```

```
## 
## Call:
## aov(formula = value ~ variable, data = m.vote)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -2.65e-10  0.00e+00  0.00e+00  0.00e+00  6.00e-14 
## 
## Coefficients:
##                     Estimate Std. Error  t value Pr(>|t|)    
## (Intercept)         1.00e+00   1.52e-14 6.58e+13  < 2e-16 ***
## variableAnimation   5.66e-14   2.29e-14 2.47e+00  0.01337 *  
## variableComedy      5.66e-14   1.71e-14 3.31e+00  0.00095 ***
## variableDrama       5.66e-14   1.67e-14 3.38e+00  0.00072 ***
## variableDocumentary 5.66e-14   2.33e-14 2.43e+00  0.01504 *  
## variableRomance     5.66e-14   2.14e-14 2.64e+00  0.00820 ** 
## variableShort       5.66e-14   1.86e-14 3.05e+00  0.00230 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.04e-12 on 65127 degrees of freedom
## Multiple R-squared:   0.5,	Adjusted R-squared:   0.5 
## F-statistic: 1.09e+04 on 6 and 65127 DF,  p-value: <2e-16
```

```r
s.len <- summarise(group_by(m.vote, variable), avg.vote = mean(votes))
c <- ggplot(data = s.len, aes(x = variable, y = avg.vote, fill = variable)) 
c + geom_bar(stat = "identity") + geom_hline(aes(yintercept = mean(s.len$avg.vote)),colour = "red")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

Ratings - comparison of means
The anova results in rejection of the hypothesis that the theoretical mean is the same across genres
A simple bar chart illustrates the variability between genres

```r
m.rate  <- filter(melt(select(df.movies, rating, genres), id = "rating"), value != 0)
aov.out <- aov(value ~ variable, data = m.rate)
summary.lm(aov.out)
```

```
## 
## Call:
## aov(formula = value ~ variable, data = m.rate)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -2.65e-10  0.00e+00  0.00e+00  0.00e+00  6.00e-14 
## 
## Coefficients:
##                     Estimate Std. Error  t value Pr(>|t|)    
## (Intercept)         1.00e+00   1.52e-14 6.58e+13  < 2e-16 ***
## variableAnimation   5.66e-14   2.29e-14 2.47e+00  0.01337 *  
## variableComedy      5.66e-14   1.71e-14 3.31e+00  0.00095 ***
## variableDrama       5.66e-14   1.67e-14 3.38e+00  0.00072 ***
## variableDocumentary 5.66e-14   2.33e-14 2.43e+00  0.01504 *  
## variableRomance     5.66e-14   2.14e-14 2.64e+00  0.00820 ** 
## variableShort       5.66e-14   1.86e-14 3.05e+00  0.00230 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.04e-12 on 65127 degrees of freedom
## Multiple R-squared:   0.5,	Adjusted R-squared:   0.5 
## F-statistic: 1.09e+04 on 6 and 65127 DF,  p-value: <2e-16
```

```r
s.len <- summarise(group_by(m.rate, variable), avg.rate = mean(rating))
ggplot(data = s.len, aes(x = variable, y = avg.rate, fill = variable)) + geom_bar(stat = "identity" ) + geom_hline(yintercept = mean(s.len$avg.rate), colour = "red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

### Selecting and plotting a subset of data to include just those movies that meet the folowing criteria:

Number of votes is in the upper 1%  for the genre
Rating is in the upper 1% for the genre

# Calculate the cutoff for the upper 95% of votes for each genre

```r
#Dist. of votes, only intersted in uppper tail
summary(df.movies$votes)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       5      11      30     632     112  158000
```

```r
m.vote <- filter(group_by(melt(select(df.movies,votes, genres), id =  "votes"), 
                       variable),value!=0)

# looking at top 95% of the votes

print("Table of 95 percentile of votes by genre")
```

```
## [1] "Table of 95 percentile of votes by genre"
```

```r
(vote.95 <- summarise(m.vote, cutoff.v = quantile(votes, .95)))
```

```
## Source: local data frame [7 x 2]
## 
##      variable cutoff.v
## 1      Action  10800.9
## 2   Animation    447.7
## 3      Comedy   3536.0
## 4       Drama   3549.0
## 5 Documentary    264.9
## 6     Romance   7087.9
## 7       Short     95.0
```

```r
cuts.v <- vote.95$cutoff.v
names(cuts.v) = vote.95$variable
```

### 95% percentile rating for each genre

```r
#summary of all rating, only interested in upper tail
summary(df.movies$rating)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    5.00    6.10    5.93    7.00   10.00
```

```r
m.rate <- filter(group_by(melt(select(df.movies, rating, genres), id =  "rating"), 
                          variable),value!=0)
print("Table of 95 percentile of ratings by genre")
```

```
## [1] "Table of 95 percentile of ratings by genre"
```

```r
(rate.95 <- summarise(m.rate, cutoff.r = quantile(rating, .95)))
```

```
## Source: local data frame [7 x 2]
## 
##      variable cutoff.r
## 1      Action      7.7
## 2   Animation      8.3
## 3      Comedy      8.1
## 4       Drama      8.2
## 5 Documentary      9.0
## 6     Romance      8.1
## 7       Short      8.8
```

```r
cuts <- rate.95$cutoff
names(cuts) = rate.95$variable
```

Select the 'best' movies in each genre


```r
# year + title identifies the movie
m <- select(df.movies, title, year, genres, votes, rating)
mtop <- filter(melt(m, id =  c("rating", "votes", "year", "title")), value != 0)
mtop  <- merge(mtop, rate.95,  by.mtop = "variable", by.rate.95 = "variable")
mtop  <- filter(merge(mtop, vote.95,  by.mtop = "variable", by.vote.95 = "variable"), rating >= cutoff.r, votes >= cutoff.v)

# organizaing DAta

best.movies <- cbind(select(mtop, variable, title, year, rating, votes), score = mtop$rating * mtop$votes/1000, titleyear = paste(mtop$title, " - ", mtop$year))
colnames(best.movies)[1] = 'genre'
xx <- best.movies[order(best.movies$titleyear, -best.movies$score),]
xx <- xx[!duplicated(xx$titleyear),]
```
### The list of best popular movies, sorted by genre and then score
There is no distinct film in the genre Documentary that meets the criteria of being in the 95th percentile for both rating and votes
Listing the top five in each

```r
list.of.movies <- select(xx[order(xx$genre, -xx$score),], genre, title, year, rating, votes, score)

# top five in each
(by(data = list.of.movies, INDICES = list.of.movies$genre, FUN = function(x) head(x, 5)))
```

```
## list.of.movies$genre: Action
##     genre                                              title year rating  votes  score
## 26 Action Lord of the Rings: The Fellowship of the Ring, The 2001    8.8 157608 1387.0
## 29 Action                                        Matrix, The 1999    8.5 143853 1222.8
## 42 Action                                          Star Wars 1977    8.8 134640 1184.8
## 28 Action             Lord of the Rings: The Two Towers, The 2002    8.8 114797 1010.2
## 27 Action     Lord of the Rings: The Return of the King, The 2003    9.0 103631  932.7
## ------------------------------------------------------------------------------------------ 
## list.of.movies$genre: Animation
##        genre                                title year rating votes  score
## 53 Animation                         Finding Nemo 2003    8.3 41846 347.32
## 59 Animation        Sen to Chihiro no kamikakushi 2001    8.6 24253 208.58
## 60 Animation Wallace & Gromit: The Wrong Trousers 1993    8.4 14976 125.80
## 54 Animation                       Hotaru no haka 1988    8.3  6886  57.15
## 61 Animation                   What's Opera, Doc? 1957    8.3  2781  23.08
## ------------------------------------------------------------------------------------------ 
## list.of.movies$genre: Comedy
##     genre                                                                title year rating votes score
## 76 Comedy                                                         Forrest Gump 1994    8.2 89722 735.7
## 72 Comedy Dr. Strangelove or: How I Learned to Stop Worrying and Love the Bomb 1964    8.7 63471 552.2
## 89 Comedy                                      Monty Python and the Holy Grail 1975    8.4 60565 508.7
## 93 Comedy                                                  Princess Bride, The 1987    8.2 53946 442.4
## 74 Comedy                                Eternal Sunshine of the Spotless Mind 2004    8.6 46240 397.7
## ------------------------------------------------------------------------------------------ 
## list.of.movies$genre: Drama
##     genre                     title year rating  votes  score
## 186 Drama Shawshank Redemption, The 1994    9.1 149494 1360.4
## 175 Drama              Pulp Fiction 1994    8.8 132745 1168.2
## 140 Drama            Godfather, The 1972    9.1 122755 1117.1
## 136 Drama                Fight Club 1999    8.5 112092  952.8
## 107 Drama           American Beauty 1999    8.5 109991  934.9
## ------------------------------------------------------------------------------------------ 
## list.of.movies$genre: Documentary
## NULL
## ------------------------------------------------------------------------------------------ 
## list.of.movies$genre: Romance
##       genre                         title year rating votes  score
## 216 Romance            Gone with the Wind 1939    8.1 28836 233.57
## 223 Romance                     Notorious 1946    8.3 10637  88.29
## 218 Romance                    In America 2002    8.1  8371  67.81
## 204 Romance Adventures of Robin Hood, The 1938    8.2  7359  60.34
## ------------------------------------------------------------------------------------------ 
## list.of.movies$genre: Short
##     genre                             title year rating votes score
## 236 Short                One Froggy Evening 1955    8.9   405 3.604
## 235 Short                              More 1998    8.8   408 3.590
## 234 Short                   Ilha das Flores 1989    8.9   328 2.919
## 233 Short Homme qui plantait des arbres, L' 1987    9.0   312 2.808
```

```r
print("Count by genre, Documentary has no distinct film that meets both criteria")
```

```
## [1] "Count by genre, Documentary has no distinct film that meets both criteria"
```

```r
print(table(list.of.movies$genre))
```

```
## 
##      Action   Animation      Comedy       Drama Documentary     Romance       Short 
##          51           8          37          75           0           4           4
```

```r
print("Summary Stats")
```

```
## [1] "Summary Stats"
```

```r
print(summarise(group_by(list.of.movies, genre), total.titles = length(titleyear), max.rate = max(rating), max.votes= max(votes), max.total = max(score)))
```

```
## Error: object 'titleyear' not found
```

