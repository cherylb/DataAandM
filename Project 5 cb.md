Project 5 
========================================================
Data Source:  R datasets package, using the eurodist data
Procedure:
1.  Load libraries for datasets, dplyr, stats, reshape2, and RNeo4j 
2.  import eurodist object
3.  use stats and reshape2 to read and reshape the eurodist 'dist' object into a data frame
4.  create a unique list of cities from the data, these will be the nodes
5.  remove duplicates from the distance data
6.  use RNeo4j package to connect to default graph
7.  loop through the cities to create a node for each
8.  loop through the distance data frame, create a relationship with the property of 'km' set to the distance, if one does not already exist



```r
library(datasets)
library(dplyr)
```

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

```r
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
```

```
## [1] "2.1.5"
```

```r
clear(graph)
```

```
## You are about to delete all nodes, relationships, indexes, and constraints from the graph database. Are you sure? Y/N
```

```
## Error: argument is of length zero
```

```r
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
  qry1 <- "MATCH(:City {name:{x1}})-[r:Distance]-(:City {name:{x2}})
         RETURN r"
  r1 <- NULL
  r1 <- getSingleRel(graph,qry1,x1 = c2, x2 = c1)
  
  if(is.null(r1)){
    
    qry2 <- "MATCH (c:City) WHERE c.name = {x} RETURN c"
    n1 <- getNodes(graph,qry2, x = c1)
    n2 <- getNodes(graph,qry2, x = c2)
    createRel(n1[[1]], "Distance", n2[[1]], km = d)
  }
  
}
```

The resulting graph shows all of the cities with a relationship between all of them. 





