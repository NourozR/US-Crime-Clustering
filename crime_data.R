library(clValid)
crime_data_raw <- crime_data
crime_data_raw$crime.cluster <- NULL
View(crime_data_raw)
crime_data_raw <- scale(crime_data_raw[,2:5])
plot(crime_data$Murder, crime_data$Assault, col = crime_usa_km$cluster)

# PCA analysis
crime_data_pca <- prcomp(x = crime_data_raw, scale. = TRUE, center = TRUE)
summary(crime_data_pca)

#K-means Clustering
crime_usa_km <- kmeans(x = crime_data_raw, centers = 4, nstart = 20)
dunn_km <- dunn(clusters = crime_usa_km$cluster, Data = crime_data_raw)
plot(crime_data$Murder, crime_data$Assault, col = crime_usa_km$cluster)

# Hierarchical clustering

dist_matrix <- dist(crime_data_raw, method = "euclidean") # euclidian distance is taken, but other options can be used.
crime_hc <- hclust(dist_matrix, method = "single") # single linkage method has been used.
plot(crime_hc) # to plot the dendogram
cluster_hc <- cutree(crime_hc, k = 4 ) 
dunn_hc <- dunn(clusters = cluster_hc, Data = crime_data_raw)
dunn_hc # prints out dunn index

# Plot all clusters against states to see the clusters
plot(crime_data$X, crime_usa_km$cluster)
plot(crime_data$X, cluster_hc)









