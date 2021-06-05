
### Hierarchical Clustering ###
# Distance matrix
#This function computes and returns the distance matrix
d <- dist(normalized_data, method = "euclidean")
#Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.
fit <- hclust(d, method = "complete")
# Display dendrogram
plot(fit) 
#Cut a Tree into Groups of Data
groups <- cutree(fit, k = 3) # Cut tree into 3 clusters
#converting into matrix to added it to main data
membership <- as.matrix(groups)
#adding it to main data
final <- data.frame(membership, mydata)


## non-Hierarchical Clustering ###
# Elbow curve to decide the k value
twss <- NULL    #null variable is defined
for (i in 2:8) {
  twss <- c(twss, kmeans(df_final, centers = i)$tot.withinss)
}
twss
plot(2:8, twss, type = "b")
# 3 Cluster Solution
fit <- kmeans(df_final, 3)  # taken 3 clusters are good from graph
final <- data.frame(fit$cluster, df)
#aggregateing data with clusters by mean using aggregate function.
mean_cluster<-aggregate(df_numeric, by = list(fit$cluster), FUN = mean)


##### PCA  ######
pcaObj <- princomp(df_norm, cor = TRUE, scores = TRUE, covmat = NULL)
#plots
plot(pcaObj) # graph showing importance of principal components 
#score = table data * object
pcaObj$scores
#only first eight columns are selected because it has 85% of data.
final<-data.frame( pcaObj$scores[, 1:8])


#Association Rules
library(arules)
library(arulesViz)
tData <- as (my_movies1, "transactions") 
# Training Apriori on the dataset
arules <- apriori(tData, parameter = list(support = 0.01, confidence = 0.25, minlen = 2))
inspect(arules[1:5])
rules_lift <- sort (arules, by="lift", decreasing=TRUE)
plot(arules)


#Recomendation_system
library(reshape2)
library(reshape)
## covert to matrix format
ratings_matrix <- as.matrix(acast(ratings_list, ï..Id~Titles, value.var = "Reviews", na.rm=FALSE))
## recommendarlab realRatingMatrix format
R <- as(ratings_matrix, "realRatingMatrix")
R = normalize(R)
rec_mod = Recommender(R, method = "POPULAR", param=list(method="Cosine",nn=10)) 
Top_5_pred = predict(rec_mod, R["4172"], n=5)
Top_5_List = as(Top_5_pred, "list")


##Network_Analytics
#library("igraph")
AirlineNW <- graph.edgelist(as.matrix(airline_routes[, c(3,5)]), directed = F)
indegree <- degree(AirlineNW, mode = "in") #calculating numbers of in for each airport
max(indegree) #maximum 
index <- which(indegree == max(indegree)) #to get respective airport
indegree[index] #airport name and in flights
which(airports$IATA_FAA == "ATL") #in airport dataset respective ATL airport index
airports[3584, ] #all columns of index (ATL)
outdegree <- degree(AirlineNW, mode = "out")
# Which airport is close to most of the airports (in terms of number of flights)
closeness_in <- closeness(AirlineNW, mode = "in", normalized = TRUE)
# Which airport comes in between most of the routes and hence is an important international hub?
btwn <- betweenness(AirlineNW, normalized = TRUE)
#Eigenvector centrality (influential)
eigenv <- eigen_centrality(AirlineNW, directed = TRUE, scale = FALSE, weights = NULL)
eigenv$vector
max(eigenv$vector)
index <- which(eigenv$vector == max(eigenv$vector))
eigenv$vector[index]
which(airports$IATA_FAA == "ATL")
airports[3584, ]
pg_rank <- page_rank(AirlineNW, damping = 0.999)


