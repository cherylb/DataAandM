
library(datasets)
library(dplyr)
library(stats)
library(reshape2)
library(RNeo4j)  # devtools::install_github("nicolewhite/RNeo4j")


df <- data.frame(melt(as.matrix(eurodist), varnames = c("City1", "City2")))

#list of cities (nodes)

cities <- df %>%select(City1) %>% group_by(City1)%>% summarise(count = n())

# remove duplicates get distances (relationships)

distance <- unique(df %>%filter(value != 0))

graph = startGraph("http://localhost:7474/db/data/")
graph$version
clear(graph)

#import cities as nodes into Neo4j

n <- nrow(cities)

for (i in 1:n){
  createNode(graph, "City", name = cities[i,1], continent = "Europe")
}


# create relationships
n <- nrow(distance)
for (i in 1:n){
  c1 <- distance[i,1]
  c2 <- distance[i,2]
  d <- distance[i,3]
  # get c1 node
  qry1 <- "MATCH(:City {name:{x1}})-[r:Distance]-(:City {name:{x2}}) RETURN r"
  r1 <- NULL
  r1 <- getSingleRel(graph,qry1,x1 = c2, x2 = c1)
  
  if(is.null(r1)){
    
    qry <- "MATCH (c:City) WHERE c.name = {x} RETURN c"
    n1 <- getNodes(graph,qry, x = c1)
    n2 <- getNodes(graph,qry, x = c2)
    createRel(n1[[1]], "Distance", n2[[1]], km = d)
  }
  
}

# Query the results
# find the distance between Brussels and Athens
c1 = "Brussels"
c2 = "Athens"
r <- getSingleRel(graph,qry1,x2 = c1, x1 = c2)

print("Distance: ")
print(r[1])


