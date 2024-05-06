library(readxl)
library(cluster)
library(NbClust)

# Specify the file path
file_location <- "C:\\Users\\dlahi\\OneDrive\\Desktop\\ML\\ML\\ML\\CW\\Whitewine_v6.xlsx"

# Read the Excel file
wine_dataset <- read_excel(file_location)

# View the first few rows of the imported data
head(wine_dataset)

# Assuming your dataset is named 'wine_dataset'
# Remove the 'Quality' column
wine_dataset <- wine_dataset[, -which(names(wine_dataset) == "quality")]

# perfomr outlier removal
# Assuming your dataset is named 'wine_dataset' without the 'Quality' column
# Scale the data
scaled_dataset <- scale(wine_dataset)

# Assuming your scaled dataset is named 'scaled_dataset'
# Define a function to detect outliers using the IQR method
identify_outliers <- function(x, threshold = 1.5) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - threshold * iqr
  upper_bound <- q3 + threshold * iqr
  return(x < lower_bound | x > upper_bound)
}

# Apply the outlier detection function to each column
outlier_indices <- apply(scaled_dataset, 2, identify_outliers)

# Remove outliers
cleaned_dataset <- scaled_dataset[!apply(outlier_indices, 1, any), ]

# Assuming your scaled and cleaned data is stored in 'cleaned_dataset'
library(NbClust)

# Determine the number of clusters using NBclust
num_clusters <- NbClust(cleaned_dataset, min.nc = 2, max.nc = 10, method = "kmeans")
num_clusters$Best.nc


wcss_values <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(cleaned_dataset, centers = i)
  wcss_values[i] <- kmeans_model$tot.withinss
}
plot(1:10, wcss_values, type = "b", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares (WCSS)", main = "Elbow Method")


library(cluster)
gap_statistics <- clusGap(cleaned_dataset, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
plot(gap_statistics, main = "Gap Statistics")

silhouette_score_func <- function(k){
  km <- kmeans(cleaned_dataset, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(cleaned_dataset))
  mean(ss[, 3])
}
k_values <- 2:10
avg_silhouette_scores <- sapply(k_values, silhouette_score_func)
plot(k_values, type='b', avg_silhouette_scores, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


#Based on the above results
optimal_num_clusters <- 2
# Perform k-means clustering with the optimal number of clusters
kmeans_model <- kmeans(cleaned_dataset, centers = optimal_num_clusters)

# Print kmeans model output
print(kmeans_model)

# Calculate within-cluster sum of squares (WSS)
within_cluster_ss <- sum(kmeans_model$withinss)

# Calculate between-cluster sum of squares (BSS)
between_cluster_ss <- sum(kmeans_model$betweenss)

# Calculate total sum of squares (TSS)
total_ss <- within_cluster_ss + between_cluster_ss

# Calculate ratio of BSS over TSS
bss_tss_ratio <- between_cluster_ss / total_ss

# Print relevant metrics
cat("Within-cluster sum of squares (WSS):", within_cluster_ss, "\n")
cat("Between-cluster sum of squares (BSS):", between_cluster_ss, "\n")
cat("Total sum of squares (TSS):", total_ss, "\n")
cat("Ratio of BSS over TSS:", bss_tss_ratio, "\n")

# Visualize cluster centers
cluster_centroids <- kmeans_model$centers
print(cluster_centroids)

# Visualize cluster assignments
cluster_assignments <- kmeans_model$cluster
table(cluster_assignments)