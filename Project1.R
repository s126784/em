library(ggplot2)

set.seed(123)

#read csv
data <- read.csv("data.csv")
targets <- read.csv("targets.csv")

targets <- as.matrix(targets)
head(data, 5)

# get indexes of incomplete rows
incomplete_rows <- which(apply(data, 1, function(x) any(is.na(x))))
incomplete_rows

data_complete <- data[-incomplete_rows, ]
targets_complete <- targets[-incomplete_rows, ]

data_scaled <- scale(data_complete)

# split data into two components using kmeans
kmeans_result <- kmeans(data_scaled, centers = 2)

#calculate pca loading
pca <- prcomp(data_complete)
pca_loading <- pca$rotation

# Apply CDPCA on scaled data
result <- CDpca_simple(data_scaled, P = 2, Q = 2, maxit= 15, r = 15, cdpcaplot = TRUE)
cdpca_loading = result$A

#show first two columns
pca_loading[, 1:2]
cdpca_loading


# result
