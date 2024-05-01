# Load the readxl package
library(readxl)
library(cluster)
library(NbClust)

# Specify the file path
file_path <- "C:\\Users\\dlahi\\OneDrive\\Desktop\\ML\\ML\\ML\\CW\\Whitewine_v6.xlsx"

# Read the Excel file
wine_data <- read_excel(file_path)

# View the first few rows of the imported data
head(wine_data)

# Assuming your dataset is named 'wine_data'
# Remove the 'Quality' column
wine_data <- wine_data[, -which(names(wine_data) == "quality")]

# perfomr outlier removal
# Assuming your dataset is named 'wine_data' without the 'Quality' column
# Scale the data
scaled_data <- scale(wine_data)

# Assuming your scaled dataset is named 'scaled_data'
# Define a function to detect outliers using the IQR method
detect_outliers <- function(x, threshold = 1.5) {
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - threshold * iqr
  upper_bound <- q3 + threshold * iqr
  return(x < lower_bound | x > upper_bound)
}

# Apply the outlier detection function to each column
outlier_indices <- apply(scaled_data, 2, detect_outliers)

# Remove outliers
cleaned_data <- scaled_data[!apply(outlier_indices, 1, any), ]

# Assuming your scaled and cleaned data is stored in 'cleaned_data'
library(NbClust)

# Determine the number of clusters using NBclust
nb_clusters <- NbClust(cleaned_data, min.nc = 2, max.nc = 10, method = "kmeans")
nb_clusters$Best.nc


wcss <- numeric(10)
for (i in 1:10) {
  kmeans_model <- kmeans(cleaned_data, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}
plot(1:10, wcss, type = "b", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares (WCSS)", main = "Elbow Method")


library(cluster)
gap_stat <- clusGap(cleaned_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
plot(gap_stat, main = "Gap Statistics")

silhouette_score <- function(k){
  km <- kmeans(cleaned_data, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(cleaned_data))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


#Based on the above results
optimal_k <- 2
# Perform k-means clustering with the optimal number of clusters
kmeans_model <- kmeans(cleaned_data, centers = optimal_k)

# Print kmeans model output
print(kmeans_model)

# Calculate within-cluster sum of squares (WSS)
wss <- sum(kmeans_model$withinss)

# Calculate between-cluster sum of squares (BSS)
bss <- sum(kmeans_model$betweenss)

# Calculate total sum of squares (TSS)
tss <- wss + bss

# Calculate ratio of BSS over TSS
bss_tss_ratio <- bss / tss

# Print relevant metrics
cat("Within-cluster sum of squares (WSS):", wss, "\n")
cat("Between-cluster sum of squares (BSS):", bss, "\n")
cat("Total sum of squares (TSS):", tss, "\n")
cat("Ratio of BSS over TSS:", bss_tss_ratio, "\n")

# Visualize cluster centers
cluster_centers <- kmeans_model$centers
print(cluster_centers)

# Visualize cluster assignments
cluster_assignments <- kmeans_model$cluster
table(cluster_assignments)
