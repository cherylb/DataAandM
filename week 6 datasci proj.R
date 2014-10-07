

library(RCurl)
library(jsonlite)

base <- "http://comcat.cr.usgs.gov/fdsnws/event/1/"   # [METHOD[?PARAMETERS]]
method <- "query"
frm.time <- "2014-01-01"
to.time <- "2014-09-28"
min.lat <- 30
max.lat <- 34
min.long <- 120
max.long <- 130
events <- "earthquake"
min.mag <- 2.0


u <- paste(base, method, "?starttime=", frm.time,"&maxlatitude=", max.lat,"&minlatitude=", min.lat,
      "&maxlongitude", max.long, "&minlongitude=", min.long, "&minmagnitude=", min.mag,
      "&eventtype=", events, "&format=geojson", "&endtime=", to.time, sep = "")
url <- URLencode(u)

data <- getURL(u)
meta.data <- jsonlite :: fromJSON(data)[[2]]
df.props <- fromJSON(data)[[3]]

head(df.props)
