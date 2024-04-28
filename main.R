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


