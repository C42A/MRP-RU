#######################################
## Cassandra Postma
## DS MRP - Graph Analysis
## Due June 17th
#######################################
install.packages("igraph")
install.packages("compare")
library("igraph")
library("compare")


##Read and clean data, convert to bipartite undirected graph
table <- read.table("C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\FullTrain.csv", sep=",", header=TRUE)

##Map movie IDs to 1 to 213
idConvert<- data.frame(MovieID = numeric(213), ConvertID = numeric(213))
idConvert$ConvertID<-c(1:213)

movies<-table$MovieID
nUnique_Movie<-unique(movies)
idConvert$MovieID<-nUnique_Movie

idConvert
fullTable<-merge(table, idConvert, on='MovieID')

##Map customer IDs to 1 to 4287
idCust<- data.frame(CustomerID = numeric(4287), cust = numeric(4287))
idCust$cust<-c(1:4287)

customers1<-table$CustomerID
nUnique_cust<-unique(customers1)
idCust$CustomerID<-nUnique_cust

idCust
fullTable<-merge(fullTable, idCust, on='CustomerID')


##Create bipartite graph of movie id and customer id with rating as weight
m<-data.matrix(fullTable)
m_rate<-m[,c(9,8,6)]

bg<-graph.edgelist(m_rate[,c(1,2)])
E(bg)$weight=as.numeric(m_rate[,3])
bg<-as.undirected(bg,mode="collapse")
bg

##Adjacency Matrix
adj=get.adjacency(bg,attr='weight') 
nrow(adj)
ncol(adj)
adj<-adj[1:213,1:4287]

test<-adj[1:10,1:20]
test

testG<-graph.incidence(test, weighted = T)
testG<-as.undirected(testG,mode="collapse")
V(testG)$color <- V(testG)$type
V(testG)$color=gsub("FALSE","red",V(testG)$color)
V(testG)$color=gsub("TRUE","blue",V(testG)$color)
plot(testG, edge.color="gray30",edge.width=E(testG)$weight, layout=layout_as_bipartite)


bg<-graph.incidence(adj, weighted = T)
bg<-as.undirected(bg,mode="collapse")


##Get degree distribution graph
plot(degree.distribution(bg), xlab="Degree", ylab="Frequency", log="xy", main= "Log Degree Distribution of MovieID paired with CustomerID")
max(degree(bg))
mean(degree(bg))
summary(degree(bg))

df_deg <- as.data.frame(table(degree(bg)))
colnames(df_deg) <- c('degree','count')
plot(df_deg)

##Get number of edges and verticies
gsize(bg)
length(E(bg))

length(V(testG))
length(V(bg))
##Plot bg
V(bg)$color <- V(bg)$type
V(bg)$color=gsub("FALSE","red",V(bg)$color)
V(bg)$color=gsub("TRUE","blue",V(bg)$color)
plot(bg, edge.color="gray30",edge.width=E(bg)$weight, layout=layout_as_bipartite)

##Export Graph for other uses
write_graph(bg, "C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\GraphRep", format = "gml")
write.csv(fullTable,"C:\\Users\\MrW\\Desktop\\Cassy's School Stuff\\MRP\\GraphTrain.csv")
