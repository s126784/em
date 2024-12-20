options(rgl.useNULL=TRUE)
if(!require('matlib')) {
  install.packages('matlib')
  library('matlib')
}

# ============= STEP 1: LOAD LIBRARIES =============
library(quantmod)
library(tidyverse)
library(stats)
library(cluster)
library(corrplot)
library(ggplot2)
library(corrplot)
library(Amelia)
library(ggplot2)
library(mi)
library("sda")

# ============= STEP 2: DEFINE STOCK SYMBOLS =============
tech_stocks <- c("AAPL", "MSFT", "GOOGL")
finance_stocks <- c("JPM", "V", "MA")
consumer_stocks <- c("KO", "PG", "WMT")
all_stocks <- c(tech_stocks, finance_stocks, consumer_stocks)

# Print initial setup
cat("\nInitial Setup:")
cat("\nNumber of stocks:", length(all_stocks))
cat("\nNumber of sectors:", 3)

# ============= STEP 3: GET STOCK DATA =============
start_date <- "2022-01-01"
end_date <- "2023-12-31"

get_stock_data <- function(symbol) {
  cat("\nGetting data for", symbol)
  tryCatch({
    stock <- getSymbols(symbol,
                        from = start_date,
                        to = end_date,
                        auto.assign = FALSE)

    stock_df <- data.frame(
      Date = index(stock),
      Open = as.numeric(stock[, paste0(symbol, ".Open")]),
      High = as.numeric(stock[, paste0(symbol, ".High")]),
      Low = as.numeric(stock[, paste0(symbol, ".Low")]),
      Close = as.numeric(stock[, paste0(symbol, ".Close")]),
      Volume = as.numeric(stock[, paste0(symbol, ".Volume")]),
      Adjusted = as.numeric(stock[, paste0(symbol, ".Adjusted")]),
      Symbol = symbol
    )
    cat(" - Success:", nrow(stock_df), "observations")
    return(stock_df)
  },
  error = function(e) {
    cat(" - Error:", e$message, "\n")
    return(NULL)
  })
}

# Get data for all stocks
stock_data_list <- lapply(all_stocks, get_stock_data)
names(stock_data_list) <- all_stocks

# ============= STEP 4: DATA PROCESSING =============
# Remove failed downloads
stock_data_list <- lapply(all_stocks, get_stock_data)
names(stock_data_list) <- all_stocks
stock_data_list <- stock_data_list[!sapply(stock_data_list, is.null)]
cat("\n\nProcessed Data:")
cat("\nNumber of successfully downloaded stocks:", length(stock_data_list))

# Combine all data
combined_data <- do.call(rbind, stock_data_list)
cat("\nInitial combined data dimensions:", dim(combined_data)[1], "rows x", dim(combined_data)[2], "columns")

# Clean missing values
combined_data <- na.omit(combined_data)
cat("\nAfter cleaning missing values:", dim(combined_data)[1], "rows x", dim(combined_data)[2], "columns")


# ============= STEP 5: PREPARE ANALYSIS MATRIX =============

set.seed(123155 * (126784 -123177 ))
#set.seed(123155+126784+123177)

# Select numeric variables for analysis
price_vars <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
cat("\n\nAnalysis Variables:")
cat("\nNumber of price variables:", length(price_vars))

# Create and scale analysis matrix
X <- as.matrix(combined_data[, price_vars])
X_scaled <- scale(X)

# Print final matrix dimensions
n <- nrow(X_scaled)  # number of observations
p <- ncol(X_scaled)  # number of variables

cat("\n\nFinal Analysis Matrix:")
cat("\nNumber of observations (n):", n)
cat("\nNumber of variables (p):", p)
# cat("\nCase type:", ifelse(p < n, "p < n (more observations than variables)", "p > n (more variables than observations)"))
# cat("\np/n ratio:", round(p/n, 4))


missing_count <- floor(n * p * 0.2)
missing_indices <- sample(1:(n*p), missing_count)
X_missing <- X_scaled
X_missing[missing_indices] <- NA

# ============= STEP 6: CORRELATION ANALYSIS =============
cat("\n\nCalculating Correlations...")
cor_matrix <- cor(X_scaled)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         title = paste("Correlation Matrix (", p, "variables )"))

# ============= STEP 7: BASIC STATISTICS =============
cat("\n\nBasic Statistics:")
print(summary(X_scaled))

# ============= STEP 8: VISUALIZATION =============
cat("\nCreating Visualizations...")
plot_data <- combined_data %>%
  gather(Variable, Value, all_of(price_vars)) %>%
  mutate(Date = as.Date(Date))

# Time series plot
time_series_plot <- ggplot(plot_data, aes(x = Date, y = Value, color = Symbol)) +
  geom_line() +
  facet_wrap(~Variable, scales = "free_y") +
  theme_minimal() +
  labs(title = paste("Financial Variables by Stock (n =", n, ", p =", p, ")"),
       x = "Date",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(time_series_plot)

# ============= STEP 9: DIFFERENT IMPUTATION METHODS =============
# 9.1. Mean Imputation
X_mean <- X_missing
for(j in 1:ncol(X_mean)) {
  X_mean[is.na(X_mean[,j]), j] <- mean(X_mean[,j], na.rm = TRUE)
}

# 9.2. Median Imputation
X_median <- X_missing
for(j in 1:ncol(X_median)) {
  X_median[is.na(X_median[,j]), j] <- median(X_median[,j], na.rm = TRUE)
}

# 9.3. KNN Imputation
#X_knn <- as.matrix(VIM::kNN(X_missing)[,1:ncol(X_missing)])

# 9.4. EM Algorithm
X_em_expectation_maximization <- amelia(X_missing, m=1)$imputations[[1]]

# 9.5. Data filtering
X_filtered <- na.omit(X_missing)
print(dim(X_filtered))

# ============= STEP 10: APPLY CDPCA TO ALL VERSIONS =============

P <- 3  # Number of clusters
Q <- 2  # Number of components

cdpc_results <- list(
  original = CDpca(X_scaled, P=P, Q=Q, tol=1e-5, maxit=100, r=10),
  mean = CDpca(X_mean, P=P, Q=Q, tol=1e-5, maxit=100, r=10),
  median = CDpca(X_median, P=P, Q=Q, tol=1e-5, maxit=100, r=10),
  #knn = CDpca(X_knn, P=P, Q=Q, tol=1e-5, maxit=100, r=10),
  em_expectation_maximization = CDpca(X_em_expectation_maximization, P=P, Q=Q, tol=1e-5, maxit=100, r=10),
  filtered = CDpca(X_filtered, P=P, Q=Q, tol=1e-5, maxit=100, r=10)
)

# ============= STEP 11: COMPARE RESULTS =============
metrics <- data.frame(
  Method = c("Original", "Mean", "Median", "Emax", "Filtered"),
  bcdev = sapply(cdpc_results, function(x) x$bcdev),
  Enorm = sapply(cdpc_results, function(x) x$Enorm)
)

# ============= STEP 12: VISUALIZE RESULTS =============
# Function to get y-axis range with small margins
get_range <- function(x) {
  margin <- (max(x) - min(x)) * 0.1
  c(min(x) - margin, max(x) + margin)
}

# Cluster Deviance Plot with tight y-axis
ggplot(metrics, aes(x=Method, y=bcdev)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(title="Cluster Deviance by Method", y="Cluster Deviance (%)") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  coord_cartesian(ylim = get_range(metrics$bcdev))

# Error Norm Plot with tight y-axis
ggplot(metrics, aes(x=Method, y=Enorm)) +
  geom_bar(stat="identity", fill="coral") +
  theme_minimal() +
  labs(title="Error Norm by Method", y="Error Norm") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  coord_cartesian(ylim = get_range(metrics$Enorm))

# ============= STEP 13: CALCULATE SILHOUETTE SCORES FOR EACH METHOD =============

# Function to calculate silhouette score
calculate_silhouette <- function(X, cdpca_result) {
  # Extract cluster assignments from tableclass
  clusters <- cdpca_result$tableclass$"CDPCA Class"

  # Print cluster information for debugging
  cat("\nNumber of observations:", length(clusters))
  cat("\nUnique clusters:", length(unique(clusters)))
  cat("\nCluster distribution:")
  print(table(clusters))

  # Check if we have at least 2 clusters
  if(length(unique(clusters)) < 2) {
    cat("\nWarning: Less than 2 clusters found")
    return(NA)
  }

  # Calculate silhouette score
  sil <- silhouette(clusters, dist(X))
  return(mean(sil[, "sil_width"]))
}

# Calculate silhouette scores
cat("\n=== Silhouette Scores ===\n")

cat("\nOriginal data:")
sil_original <- calculate_silhouette(X_scaled, cdpc_results$original)
cat("\nScore:", sil_original)

cat("\n\nMean imputation:")
sil_mean <- calculate_silhouette(X_mean, cdpc_results$mean)
cat("\nScore:", sil_mean)

cat("\n\nMedian imputation:")
sil_median <- calculate_silhouette(X_median, cdpc_results$median)
cat("\nScore:", sil_median)

#cat("\n\nKNN imputation:")
#sil_knn <- calculate_silhouette(X_knn, cdpc_results$knn)
#cat("\nScore:", sil_knn)

cat("\n\nEM imputation:")
sil_em <- calculate_silhouette(X_em_expectation_maximization, cdpc_results$em_expectation_maximization)
cat("\nScore:", sil_em)

cat("\n\nFiltered data:")
sil_filtered <- calculate_silhouette(X_filtered, cdpc_results$filtered)
cat("\nScore:", sil_filtered)

# Create summary dataframe
silhouette_scores <- data.frame(
  Method = c("Original", "Mean", "Median",  "EM", "Filtered"),
  Score = c(sil_original, sil_mean, sil_median, sil_em, sil_filtered)
)

# Create visualization
silhouette_plot <- ggplot(silhouette_scores, aes(x = Method, y = Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Silhouette Scores by Method",
       y = "Silhouette Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print results
cat("\n\n=== Summary of Silhouette Scores ===\n")
print(silhouette_scores)
print(silhouette_plot)
