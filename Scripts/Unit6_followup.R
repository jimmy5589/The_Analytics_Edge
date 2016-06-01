
movies = read.table("MovieLens.txt", header=FALSE, sep="|", quote="\"")
str(movies)
colnames(movies) = c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","unknown",
                     "Action","Adventure","Animation","Childrens","Comedy","Crime",
                     "Documentary","Drama","Fantasy","FilmNoir","Horror","Musical",
                     "Mistery","Romance","SciFi","Thriller","War","Western")
str(movies)

# remove the variable ID ...
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# remove duplicates
movies = unique(movies)

str(movies)

table(movies$Comedy)
table(movies$Western)
table(movies$Romance & movies$Drama)

distances = dist(movies[2:20], method="euclidean")
clusterMovies = hclust(distances, method="ward.D")
plot(clusterMovies)

clusterGroups = cutree(clusterMovies, k = 10)

tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

subset(movies, movies$Title == "Men in Black (1997)")
clusterGroups[257]
# Movie 257 is in cluster 2

cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title[1:10]

# consider k=2
clusterGroups = cutree(clusterMovies, k = 2)
str(clusterGroups)
table(clusterGroups)

cluster2=subset(movies, clusterGroups == 2)
cluster2[1:10,]
# or,
colMeans(subset(movies[2:20], clusterGroups == 2))

######
install.packages("flexclust")
