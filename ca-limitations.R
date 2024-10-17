# Load necessary libraries
library(FactoMineR)
library(factoextra)
library(ggplot2)

# 1. Limitation 1: Focus on Categorical Data
# ----------------------------------------------------
# Generate a dataset with categorical and continuous variables
set.seed(42)
Category1 <- sample(c("A", "B", "C", "D"), 100, replace = TRUE)
Category2 <- sample(c("W", "X", "Y", "Z"), 100, replace = TRUE)
Continuous1 <- rnorm(100)
Continuous2 <- rnorm(100)

# Create a data frame
data <- data.frame(Category1, Category2, Continuous1, Continuous2)

# Correspondence Analysis works only with categorical data.
# Let's create a contingency table from the categorical variables.
cat_data <- table(Category1, Category2)

# Perform Correspondence Analysis on categorical data
ca_result <- CA(cat_data, graph = FALSE)

# Visualize the Correspondence Analysis result for categorical data
fviz_ca_biplot(ca_result, repel = TRUE) +
  ggtitle("Correspondence Analysis - Categorical Data") +
  theme_minimal()

# Note: Continuous variables Continuous1 and Continuous2 are ignored, 
# highlighting CA's limitation on continuous data.


# 2. Limitation 2: Scalability Issues
# ----------------------------------------------------
# Create a larger dataset with more categories and larger sample size
set.seed(42)
Category1_large <- sample(paste("Cat", 1:20, sep=""), 1000, replace = TRUE)
Category2_large <- sample(paste("Group", 1:20, sep=""), 1000, replace = TRUE)

# Create a contingency table from the larger dataset
cat_data_large <- table(Category1_large, Category2_large)

# Perform Correspondence Analysis on the larger dataset
ca_large_result <- CA(cat_data_large, graph = FALSE)

# Visualize the Correspondence Analysis result for large dataset
fviz_ca_biplot(ca_large_result, repel = TRUE) +
  ggtitle("Correspondence Analysis - Large Dataset") +
  theme_minimal()

# Scalability issues become apparent when dealing with many categories,
# as the biplot can become cluttered and difficult to interpret.