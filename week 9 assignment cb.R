library(rmongodb)
mongo <- mongo.create()

print("mongo connection")
print(mongo.is.connected(mongo))



print(mongo.get.databases(mongo))

mongo.get.database.collections(mongo, "unitedstates")

ns <- "unitedstates.uniteddata"

# using fields unique for each data set 

qry <- '{"territory":{"$exists":1}}'
cursor <- mongo.find(mongo, ns, query=qry)
df.territory <- mongo.cursor.to.data.frame(cursor) # ok no nested data
(str(df.territory))

qry <- '{"federal_district":{"$exists":1}}'
cursor <- mongo.find(mongo, ns, query=qry)
df.district <- mongo.cursor.to.data.frame(cursor) # ok no nested data
(str(df.district))

qry <- '{"state":{"$exists":1}}'
cursor <- mongo.find(mongo, ns, query=qry)
df.state <- mongo.cursor.to.data.frame(cursor) # ok no nested data
(str(df.state))


# cleanup from factors to values and dates
df.district$establishment_date<- 
  as.Date(as.character(df.district$establishment_date), format = "%m/%d/%Y")
str(df.district)

df.state$statehood_date<- 
  as.Date(as.character(df.state$statehood_date), format = "%m/%d/%Y")
str(df.state)
