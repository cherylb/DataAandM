#  Week 4 Assignment
#  mythical questions about mythical poll data

#-------------------------------------------------------------------------------------------
#  1. Three questions about the data
#
#     1.a Is there a difference between the percentages of 'Yes' based on city?
#     1.b Is there a difference between the percentages of 'Yes' based on age group?
#     1.c Is there a difference between how many of each group were polled in the city?
#-------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------
#  2.  Create data frame in R
#-------------------------------------------------------------------------------------------
c1 <- c("Yes", 80100, 143000, 99400, 150400)
c2 <- c("No", 35900, 214800, 43000, 207000)
df <- data.frame(rbind(c1,c2))
head = c("obs", "EA16.24" , "EA25.up", "GA16.24", "GA25.up")
colnames(df) = head
print(df)


#-------------------------------------------------------------------------------------------
#  3.  Tidy the data using tidyr
#-------------------------------------------------------------------------------------------
#  variables are:  City, Age Group, number yes, number no, Total
library(tidyr)
library(dplyr)
tidy.df <- df %>% gather(cityage, freq, -obs) %>% separate(cityage, c("City", "Age"),1) %>% 
  spread(obs, freq) %>% mutate(Yes = as.numeric(Yes), No = as.numeric(No), Total = Yes + No)
print(tidy.df)


#-------------------------------------------------------------------------------------------
#  4. Answer questions in #1 using plyr
#-------------------------------------------------------------------------------------------
#  1.a Is there a difference between the percentages of 'Yes' based on city?
(df.city <- (summarise(group_by(tidy.df, City), percent.yes = sum(Yes)/sum(Total))))
(dif.by.city <- select(filter(df.city, City == "E"),percent.yes) - 
  select(filter(df.city, City == "G"),percent.yes))
#  There a small difference in the percentage of 'Yes' by city, we can conclude
#  that the slight preference for Parten bree may be slightly more pronounced in
#  Glasgow

# 1.b Is there a difference between the percentages of 'Yes' based on age group?
(df.age <- (summarise(group_by(tidy.df, Age ), percent.yes = sum(Yes)/sum(Total))))
(dif.age <- select(filter(df.age, Age == "A16.24"),percent.yes) - 
  select(filter(df.age, Age == "A25.up"),percent.yes))
#  There is a large difference between the overall age group, with a clear
#  preference among the younger 16 to 24 group for Partan bree, and a clear
#  preference amount the 25 + group for Cullen skink

#  1.c Is there a difference between how many of each group were polled in each city?
(df.grp <- tidy.df %>% select(City, Age, Total) %>% spread(Age, Total) %>%
  mutate(n = A16.24 + A25.up, per16to24 = A16.24/n, per25plus = A25.up/n))
(dif.grps <- df.grp %>% filter(City == "E") %>% select(per16to24) - 
  df.grp %>% filter(City == "G") %>% select(per16to24))
#  There is a small difference in the amount of the different age groups
#  selected between the two cities.

#-------------------------------------------------------------------------------------------
#  5. Having gone through this process, the only thing I would change for the
#  first two questions is I would eleminate the 'No' column as that value is 
#  implicit in the 'total' and 'yes' columns. For the third question, I don't
#  think I actually needed to reshape the data I could have pulled the result
#  using an alternate group/summarise
#-------------------------------------------------------------------------------------------

