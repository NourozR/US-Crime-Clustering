# US-Crime-Clustering
Clustering US States Based on Criminal Activity &amp; Population Using K-means &amp;  Hierarchical Clustering in Rstudio


The data contains crimes committed like: assault, murder, and rape in arrests per 100,000 residents in each of the 50 US states in 1973. Along with analyzing the data you will also learn about:

1. Principal component analysis to see importance of components
2. Clustering using K-means algorithm
3. Clustering using single-linkage hierarchical clustering
4. Compare results of two clustering algorithm based on Dunn's index.

Dunn Index: The Dunn index is a metric for evaluating clustering algorithms.The aim is to identify sets of clusters that are compact, with a small variance between members of the cluster, and well separated, where the means of different clusters are sufficiently far apart, as compared to the within cluster variance. 

For a given assignment of clusters, a higher Dunn index indicates better clustering. 

In this project, 
 
Dunn Index for Kmeans Clustering: 0.1604403 &
Dunn Index for Hierarchical clustering: 0.2438734


For Kmeans Algorithm:

![kmeans](https://cloud.githubusercontent.com/assets/24511419/25100699/eb0780e4-23d2-11e7-935e-a2a3408c45dc.png)


For hierarchical clustering:

![hc](https://cloud.githubusercontent.com/assets/24511419/25100847/914cff88-23d3-11e7-81f2-040f92422e69.png)


If the plots are not clear, it is recommended to run the crime_data.R file to get details of the clustering results!
