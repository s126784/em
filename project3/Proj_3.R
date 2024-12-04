# Load required libraries
library(tidyverse)

# Read the CSV file
data <- read.csv("~/Lessons/EM/Project3/pbmc68k_reduced.csv", row.names=1)  # row.names=1 if first column is cell IDs
data

# Print initial data summary to check for issues
print("Initial data summary:")
print(summary(data))

# Separate the cell type labels and expression data
cell_types <- data$cell_type
expression_data <- data %>% select(-cell_type)

# Clean the data
cleaned_data <- expression_data %>%
  mutate_all(function(x) {
    x[!is.finite(x)] <- NA
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
  })

# Scale the data
scaled_data <- scale(cleaned_data)

# Apply PCA
cdpca_result= CDpca(scaled_data, P=11, Q=2, tol=1e-5, maxit=100, r=10)