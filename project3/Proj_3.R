# ============= STEP 1: LOAD LIBRARIES =============
cat("\nLoading required libraries...")
library(tidyverse)
library(stats)
library(cluster)
library(corrplot)
library(ggplot2)
library(Amelia)
cat("\nLibraries loaded successfully!")

# ============= STEP 2: READ AND PREPARE DATA =============
cat("\n\nReading data file...")
# Read CSV file with specified delimiter
data <- read.csv("~/Lessons/EM/Project3/ds_cancer.csv", sep=",")  # Modify separator as needed
cat("\nData loaded successfully!")
cat("\nInitial data dimensions:", dim(data)[1], "rows x", dim(data)[2], "columns")

# Remove any ID or non-numeric columns (adjust as needed)
X <- data[, 2:(ncol(data)-1)]

#X <- X[1:100, ]
cat("\nSelected numeric columns for analysis")

# Scale the data
X_scaled <- scale(X)
cat("\nData scaled successfully")

# Print dimensions
n <- nrow(X_scaled)
p <- ncol(X_scaled)
cat("\n\nFinal Analysis Matrix:")
cat("\nNumber of observations (n):", n)
cat("\nNumber of variables (p):", p)
cat("\nCase type:", ifelse(p < n, "p < n (more observations than variables)", "p > n (more variables than observations)"))
cat("\np/n ratio:", round(p/n, 4))

# ============= STEP 3: CORRELATION ANALYSIS =============
#cat("\n\nCalculating correlations...")
#cor_matrix <- cor(X_scaled)
#cat("\nCorrelation matrix calculated")
#corrplot(cor_matrix,
#         method = "color",
#         type = "upper",
#         tl.col = "black",
#         tl.srt = 45,
#         title = paste("Correlation Matrix (", p, "variables )"))

# ============= STEP 4: CREATE MISSING DATA FOR TESTING =============
cat("\n\nCreating test dataset with missing values...")
set.seed(123155 * (126784 -123177 ))
missing_count <- floor(n * p * 0.2)  # 20% missing values
missing_indices <- sample(1:(n*p), missing_count)
X_missing <- X_scaled
X_missing[missing_indices] <- NA
cat("\nCreated", missing_count, "missing values (20% of data)")

# ============= STEP 5: IMPUTATION METHODS =============
cat("\n\nPerforming different imputation methods...")

cat("\nMean imputation...")
X_mean <- X_missing
for(j in 1:ncol(X_mean)) {
  X_mean[is.na(X_mean[,j]), j] <- mean(X_mean[,j], na.rm = TRUE)
}

cat("\nMedian imputation...")
X_median <- X_missing
for(j in 1:ncol(X_median)) {
  X_median[is.na(X_median[,j]), j] <- median(X_median[,j], na.rm = TRUE)
}

cat("\nEM algorithm imputation...")
X_em <- amelia(X_missing, m=1)$imputations[[1]]

cat("\nComplete cases (filtered data)...")
X_filtered <- na.omit(X_missing)
cat("\nFiltered data dimensions:", dim(X_filtered)[1], "rows x", dim(X_filtered)[2], "columns")

# ============= STEP 6: APPLY CDPCA =============
cat("\n\nApplying CDPCA with different imputation methods...")
# Set number of clusters and components
P <- 3  # Adjust based on your needs
Q <- 2  # Adjust based on your needs
cat("\nUsing P =", P, "clusters and Q =", Q, "components")

foo = CDpca(X_mean, P=P, Q=Q, tol=1e-5, maxit=100, r=5)

cdpc_results <- list(
  mean = foo,
  median = foo
  #original = CDpca(X_scaled, P=P, Q=Q, tol=1e-5, maxit=100, r=1),
  #mean = CDpca(X_mean, P=P, Q=Q, tol=1e-5, maxit=100, r=5),
  #median = CDpca(X_median, P=P, Q=Q, tol=1e-5, maxit=100, r=5)
  #em = CDpca(X_em, P=P, Q=Q, tol=1e-5, maxit=100, r=1)
)

# ============= STEP 7: COMPARE RESULTS =============
cat("\n\nComparing results across methods...")
metrics <- data.frame(
  #Method = c("Original", "Mean", "Median", "EM", "Filtered"),
  Method = c("Mean", "Median"),
  bcdev = sapply(cdpc_results, function(x) x$bcdev),
  Enorm = sapply(cdpc_results, function(x) x$Enorm)
)

cat("\n\nResults Summary:")
print(metrics)

# ============= STEP 8: CALCULATE SILHOUETTE SCORES =============
cat("\n\nCalculating silhouette scores...")

calculate_silhouette <- function(X, cdpca_result) {
  clusters <- cdpca_result$tableclass$"CDPCA Class"
  cat("\nNumber of clusters found:", length(unique(clusters)))
  if(length(unique(clusters)) < 2) {
    cat("\nWarning: Less than 2 clusters found")
    return(NA)
  }
  sil <- silhouette(clusters, dist(X))
  return(mean(sil[, "sil_width"]))
}

silhouette_scores <- data.frame(
  #Method = c("Original", "Mean", "Median", "EM", "Filtered"),
  Method = c("Mean", "Median"),
  Score = c(
    #calculate_silhouette(X_scaled, cdpc_results$original),
    calculate_silhouette(X_mean, cdpc_results$mean),
    calculate_silhouette(X_median, cdpc_results$median)
    #calculate_silhouette(X_em, cdpc_results$em),
    #calculate_silhouette(X_filtered, cdpc_results$filtered)
  )
)

cat("\n\nSilhouette Scores Summary:")
print(silhouette_scores)

# ============= STEP 9: CREATE VISUALIZATIONS =============
cat("\n\nCreating visualizations...")

# Between Cluster Deviance Plot
bcdev_plot <- ggplot(metrics, aes(x=Method, y=bcdev)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(title="Between Cluster Deviance by Method", 
       y="Between Cluster Deviance (%)") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
print(bcdev_plot)

# Error Norm Plot
error_plot <- ggplot(metrics, aes(x=Method, y=Enorm)) +
  geom_bar(stat="identity", fill="coral") +
  theme_minimal() +
  labs(title="Error Norm by Method", 
       y="Error Norm") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
print(error_plot)

# Silhouette Score Plot
silhouette_plot <- ggplot(silhouette_scores, aes(x = Method, y = Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Silhouette Scores by Method",
       y = "Silhouette Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(silhouette_plot)

cat("\n\nAnalysis complete!")
