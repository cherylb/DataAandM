library(ggplot2)

# sample data
lat <- c(40.0,37, 35.2, 37.0, 36.0, 36.5, 40.0, 35.1)
long <- c(-120.5, -120, -121.5, -122.5, -120., -123.0, -120.0, -128.0)

# just scale layer, no warning
qplot( x = long, y = lat) +scale_y_continuous(limits= c(35,40))

# just smooth layer, no warning
qplot(x = long, y = lat) + stat_smooth(method = "lm")

#combined, returns a graph and a warning
qplot(x = long, y = lat) + stat_smooth(method = "lm") + scale_y_continuous(limits= c(35,40))

#changing the last value in lat to 35.5, and no warning
lat <- c(40.0,37, 35.2, 37.0, 36.0, 36.5, 40.0, 35.5)
qplot(x = long, y = lat) + stat_smooth(method = "lm") + scale_y_continuous(limits= c(35,40))
