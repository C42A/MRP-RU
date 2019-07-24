
#######################################
## Cassandra Postma
## DS MRP - R Results on Graph Data
## Link Prediction Analysis
## Due July 29th
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
adj<-get.adjacency(bg,attr='weight') 
adjG<-adj
nrow(adjG)
adj
test<-adj[1:10,1:20]
test

testG<-graph.incidence(test, weighted = T)
testG<-as.undirected(testG,mode="collapse")
V(testG)$color <- V(testG)$type
V(testG)$color=gsub("FALSE","red",V(testG)$color)
V(testG)$color=gsub("TRUE","blue",V(testG)$color)
plot(testG, edge.color="gray30",edge.width=E(testG)$weight, layout=layout_as_bipartite)


bg<-graph.incidence(adjG, weighted = T)
bg<-as.undirected(bg,mode="collapse")



bg
edgeG<-as_edgelist(bg)
G<-bg
adjG<-as_adjacency_matrix(bg)


##############################################################
##Get Trainging and Test sets
##############################################################
##Get testing set randomly from Edge list
testSet<-function(e,k){
  ##testgSet: takes in graph
  ##Args: e: graph given as edge list
  ##
  ##return: random set of k edges indecies for training
  size<- length(e[,1])
  ##Get trainingsize random ints from 1 to size of edge list
  testEdges <- sample(1:size, k, replace=F)
  ##Subset edge list given the random edges
  return(cbind(e[testEdges,],testEdges))
}

##Get training by deleteing testSet
getTraining<-function(g,testset){
  ##getTraining: takes in graph and subset of edges
  ##Args: g: graph
  ##      subset: vector of edge indicies
  ##
  ##return: graph without edges given
  return(delete.edges(g,testset))
}


##testing sets for analysis
test1000<-testSet(edgeG,1000)
G1000<-getTraining(G,test1000[,3])
test1000<-test1000[,1:2]

test2000<-testSet(edgeG,2000)
G2000<-getTraining(G,test2000[,3])
test2000<-test2000[,1:2]

test5000<-testSet(edgeG,5000)
G5000<-getTraining(G,test5000[,3])
test5000<-test5000[,1:2]

test10000<-testSet(edgeG,10000)
G10000<-getTraining(G,test10000[,3])
test10000<-test10000[,1:2]

test15000<-testSet(edgeG,15000)
G15000<-getTraining(G,test15000[,3])
test15000<-test15000[,1:2]

test20000<-testSet(edgeG,20000)
G20000<-getTraining(G,test20000[,3])
test20000<-test20000[,1:2]

##############################################################
##Apply Node Similarities
##############################################################


##############################
## CommonNeighbours : Bipartite
##############################


## Find number in common
commonNeighbours<-function(g,x,y){
  ##commonNeighbours: takes in graph and 2 nodes
  ##Args: g: graph given as adjacency matrix
  ##      x,y: nodes in graph g
  ##
  ##return: number of nodes adjacent to both neighbours of x and y
  xNeighbours<-ego(g,order=2,x,mode = "all",2)
  yNeighbours<-ego(g,order=1,y,mode = "all",1)
  xyNeighbours<-intersect(xNeighbours, yNeighbours)
  return(length(xyNeighbours))
}

CommN<-function(g){
  ##commN: takes in graph
  ##Args: g: graph 
  ##
  ##return: adjacncy matrix where weights are the number of common neighbours
  vert<-length(V(g))
  mappedG<-matrix(rep(0,(vert*vert)),nrow=vert)
  for(i in c(1:vert)){
    for(j in c(1:vert)){
      mappedG[i,j]<-commonNeighbours(g,i,j)
    }
  }
  return(mappedG)
}



CommonN<-function(G,k){
  ##CommonN: takes in graph and the number of edges we want to predict
  ##Args: G: graph
  ##      k: int for number of edges
  ##
  ##return: set of K edges with highest number of common neighbours
  start_time <- Sys.time()
  cSimilarity<-CommN(G)
  end_time <- Sys.time()
  cTime<-end_time-start_time
  cTime
  
  cSG<-graph_from_adjacency_matrix(cSimilarity, mode = "undirected", weighted=TRUE)
  cEdge<-cbind(get.edgelist(cSG), round(E(cSG)$weight,3))
  cEdge<-cEdge[order(cEdge[,3],decreasing=TRUE),]
  return (cEdge[1:k,1:2])
}


##############################
## Jaccard : Bipartite
##############################
jaccard<-function(g,x,y){
  ##jaccard: takes in graph and 2 nodes
  ##Args: g: graph given as adjacency matrix
  ##      x,y: nodes in graph g
  ##
  ##return: number of nodes adjacent to both x and y
  xNeighbours<-ego(g,order=2,x,mode = "all",2)
  yNeighbours<-ego(g,order=1,y,mode = "all",1)
  xyNeighbours<-union(xNeighbours, yNeighbours)
  return(length(xyNeighbours))
}

JC<-function(g){
  ##JC: takes in graph
  ##Args: g: graph 
  ##
  ##return: ajacency matrix where weights are the jaccard coeff. value
  vert<-length(V(g))
  mappedG<-matrix(rep(0,(vert*vert)),nrow=vert)
  for(i in c(1:vert)){
    for(j in c(1:vert)){
      mappedG[i,j]<-jaccard(g,i,j)
    }
  }
  return(mappedG)
}



jaccardCoeff<-function(G,k){
  ##jaccardCoeff: takes in graph and the number of edges we want to predict
  ##Args: G: graph
  ##      k: int for number of edges
  ##
  ##return: set of K edges with highest number Jaccard Coeff.
  start_time <- Sys.time()
  jSimilarity<-JC(G)
  end_time <- Sys.time()
  jTime<-end_time-start_time
  jTime
  
  jSG<-graph_from_adjacency_matrix(jSimilarity, mode = "undirected", weighted=TRUE)
  jEdge<-jbind(get.edgelist(jSG), round(E(jSG)$weight,3))
  jEdge<-jEdge[order(jEdge[,3],decreasing=TRUE),]
  return (jEdge[1:k,1:2])
}


##############################
## Dice (Discarded)
##############################
start_time <- Sys.time()
DSimilarity<- similarity(G, method = "dice")
nrow(DSimilarity)
ncol(DSimilarity)

nrow(adjG)
ncol(adjG)


end_time <- Sys.time()
dTime<-end_time-start_time
dTime
DSimilarity<- DSimilarity - adjG - diag(8574)
DSimilarity[DSimilarity<0]<-0
DSimilarity[1:10,1:10]

DSG<-graph_from_adjacency_matrix(DSimilarity, mode = "undirected", weighted=TRUE)
DEdge<-cbind(get.edgelist(DSG), round(E(DSG)$weight,3))
DEdge<-DEdge[order(DEdge[,3],decreasing=TRUE),]

##############################
## Inverse Log Weighted (Discarded)
##############################
start_time <- Sys.time()
ISimilarity<- similarity(G, method = "invlogweighted")
end_time <- Sys.time()
iTime<-end_time-start_time
iTime
m<-max(ISimilarity)
ISimilarity<- ISimilarity - (m*adjG)
ISimilarity[ISimilarity<0]<-0

ISG<-graph_from_adjacency_matrix(ISimilarity, mode = "undirected", weighted=TRUE)
IEdge<-cbind(get.edgelist(ISG), round(E(ISG)$weight,3))
IEdge<-IEdge[order(IEdge[,3],decreasing=TRUE),]


##############################
## Preferential Attachment Index
##############################
PrefAtt<-function(G,k){
  ##PRefAtt: takes in graph and the number of edges we want to predict
  ##Args: G: graph
  ##      k: int for number of edges
  ##
  ##return: set of K edges with highest number Preferential Attachment
  degG<-degree(G)
  
  start_time <- Sys.time()
  degSimilarity<-degG%*%t(degG)
  rownames(degSimilarity)<-colnames(degSimilarity)
  end_time <- Sys.time()
  degTime<-end_time-start_time
  degTime
  m<-max(degSimilarity)
  degSimilarity<- degSimilarity - (m*adjG)
  diag(degSimilarity)<-0
  degSimilarity[degSimilarity<0]<-0
  degSimilarity<-data.matrix(degSimilarity)
  degSimilarity[1:10,1:10]
  
  degSG<-graph_from_adjacency_matrix(degSimilarity, mode = "undirected", weighted=TRUE)
  degEdge<-cbind(get.edgelist(degSG), round(E(degSG)$weight,3))
  degEdge<-degEdge[order(degEdge[,3],decreasing=TRUE),]
  return (degEdge[1:k,1:2])
}


##############################
## Inverse Shortest Path
##############################

inverseSP<-function(m){
  ##inverseSP: takes in a matrix and inverts each item
  ##Args: m: square matrix
  ##
  ##return: matrix where all values are 1/value
  rM<-m
  for(r in c(1:nrow(m))){
    for(c in c(1:nrow(m))){
      rM[r,c]<-1/(rM[r,c])
      if(is.nan(rM[r,c])){
        rM[r,c]<-0
      }
    }
  }
  return (rM)
}

invShortest<-function(G,k){
  ##invShortest: takes in graph and the number of edges we want to predict
  ##Args: G: graph
  ##      k: int for number of edges
  ##
  ##return: set of K edges with highest inverse shortest path
  distM<-shortest.paths(G,v=V(G),to=V(G))
  distM<-inverseSP(distM)
  
  start_time <- Sys.time()
  spSimilarity<-distM
  end_time <- Sys.time()
  spTime<-end_time-start_time
  spTime
  #m<-max(spSimilarity)
  #spSimilarity<- spSimilarity - (m*adjG)
  diag(spSimilarity)<-0
  adjG<-as_adj(G)
  spSimilarity<- spSimilarity - adjG
  spSimilarity<-data.matrix(spSimilarity)
  spSimilarity[1:10,1:10]
  
  
  spSG<-graph_from_adjacency_matrix(spSimilarity, mode = "undirected", weighted=TRUE)
  spEdge<-cbind(get.edgelist(spSG), round(E(spSG)$weight,3))
  spEdge<-spEdge[order(spEdge[,3],decreasing=TRUE),]
  return (spEdge[1:k,1:2])
}


##############################
## Random
##############################
randomSet<-function(G,k){
  edges<-get.edgelist(G)
  ##randomSet: takes in graph
  ##Args: g: graph
  ##
  ##return: random set of k edges indecies for training
  size<- length(edges[,1])
  ##Get trainingsize random ints from 1 to size of edge list
  testEdges <- sample(1:size, k, replace=F)
  ##Subset edge list given the random edges
  return(edges[testEdges,])
}

############################################################
##Anaylsis
############################################################

## compare two edge lists
compareEdge<-function(a,b){
  ##CompareEdge: takes in 2 graphs and compares counts the number of edges that are the same
  ##Args: a: graph as edge list
  ##      b: graph as edge list
  ##
  ##return: int of number of common edges
  count<-0
  for(i in c(1:nrow(a))){
    for(j in c(1:nrow(a))){
      if((a[i,1]==b[j,1] && a[i,2]==b[j,2]) || (a[i,1]==b[j,2] && a[i,2]==b[j,1])){
        count<-count+1
      }
    }
  }
  return(count)
}

##Tests for analysis graph

##k=1000
deg1000Trained<-PrefAtt(G1000, 1000)
sp1000Trained<-invShortest(G1000, 1000)
rand1000Trained<-randomSet(G, 1000)
j1000Trained<-jaccardCoeff(G1000, 1000)
c1000Trained<-CommonN(G1000, 1000)

jc1000<-compareEdge(test1000,j1000Trained)
comm1000<-compareEdge(test1000, c1000Trained)
sp1000<-compareEdge(test1000, sp1000Trained)
deg1000<-compareEdge(test1000,deg1000Trained)
rand1000<-compareEdge(test1000, rand1000Trained)

##k=2000
deg2000Trained<-PrefAtt(G2000, 2000)
sp2000Trained<-invShortest(G2000, 2000)
rand2000Trained<-randomSet(G, 2000)
j2000Trained<-jaccardCoeff(G2000, 2000)
c2000Trained<-CommonN(G2000, 2000)

jc2000<-compareEdge(test2000,j2000Trained)
comm2000<-compareEdge(test2000, c2000Trained)
sp2000<-compareEdge(test2000, sp2000Trained)
deg2000<-compareEdge(test2000,deg2000Trained)
rand2000<-compareEdge(test2000, rand2000Trained)


##k=5000
deg5000Trained<-PrefAtt(G5000, 5000)
sp5000Trained<-invShortest(G5000, 5000)
rand5000Trained<-randomSet(G, 5000)
j5000Trained<-jaccardCoeff(G5000, 5000)
c5000Trained<-commonN(G5000, 5000)

jc5000<-compareEdge(test5000,j5000Trained)
comm5000<-compareEdge(test5000, c5000Trained)
sp5000<-compareEdge(test5000, sp5000Trained)
deg5000<-compareEdge(test5000,deg5000Trained)
rand5000<-compareEdge(test5000, rand5000Trained)

##k=10000
deg10000Trained<-PrefAtt(G10000, 10000)
sp10000Trained<-invShortest(G10000, 10000)
rand10000Trained<-randomSet(G, 10000)
j10000Trained<-jaccardCoeff(G10000, 10000)
c10000Trained<-CommonN(G10000, 10000)

jc10000<-compareEdge(test10000,j10000Trained)
comm10000<-compareEdge(test10000, c10000Trained)
sp10000<-compareEdge(test10000, sp10000Trained)
deg10000<-compareEdge(test10000,deg10000Trained)
rand10000<-compareEdge(test10000, rand10000Trained)

##k=15000
deg15000Trained<-PrefAtt(G10000, 15000)
sp15000Trained<-invShortest(G10000, 15000)
rand15000Trained<-randomSet(G, 15000)
j15000Trained<-jaccardCoeff(G10000, 15000)
c15000Trained<-CommonN(G10000, 15000)

jc15000<-compareEdge(test15000,j15000Trained)
comm15000<-compareEdge(test15000, c15000Trained)
sp15000<-compareEdge(test15000, sp15000Trained)
deg15000<-compareEdge(test15000,deg15000Trained)
rand15000<-compareEdge(test15000, rand15000Trained)

##20000

test20000<-testSet(edgeG,20000)
G20000<-getTraining(G,test20000[,3])
test20000<-test20000[,1:2]

deg20000Trained<-PrefAtt(G20000, 20000)
sp20000Trained<-invShortest(G20000, 20000)
rand20000Trained<-randomSet(G, 20000)
j20000Trained<-jaccardCoeff(G20000, 20000)
c20000Trained<-CommonN(G20000, 20000)

jc20000<-compareEdge(test20000,j20000Trained)
comm20000<-compareEdge(test20000, c20000Trained)
nsp20000<-compareEdge(test20000, sp20000Trained)
deg20000<-compareEdge(test20000,deg20000Trained)
rand20000<-compareEdge(test20000, rand20000Trained)


##Graph resuts
k<-c(0,1000,2000,5000,10000,15000,20000)
comm<-c(0, comm1000, comm2000, comm5000, comm10000, comm15000, comm20000)
rand<-c(0,rand1000,rand2000,rand5000,rand10000,rand15000,rand20000)
jc<-c(0,jc1000,jc2000,jc5000,jc10000,jc15000,jc20000)
isp<-c(0,sp1000,sp2000,sp5000,sp10000,sp15000,sp20000)
pre<-c(0,deg1000,deg2000,deg5000,deg10000,deg15000,deg20000)

plot(k, comm, type="b", pch=19, col="black", xlab="k",ylab="Correct Predictions", lty=2, ylim=c(-2, 20000), xlim=c(0,20000))
lines(k, rand, pch=18, col="red", type="b", lty=2)
lines(k, jc, pch=18, col="green", type="b", lty=2)
lines(k, isp, pch=18, col="blue", type="b", lty=2)
lines(k, pre, pch=18, col="Lightblue", type="b", lty=2)

# Add a legend
legend(1,20000,legend=c("Jaccard","Random","Common Neighbours","Shortest Path","Preferential Attachment"),col=c("green","red","black","Blue","Lightblue"),lty=5:8,cex=0.8)

