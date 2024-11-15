# ============= STEP 1: LOAD LIBRARIES =============
library(quantmod)
library(tidyverse)
library(stats)
library(cluster)
library(corrplot)
library(ggplot2)

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

# ============= STEP 9: SAVE RESULTS =============
results <- list(
  data = X_scaled,
  symbols = all_stocks,
  dates = combined_data$Date,
  dimensions = list(
    n = n,
    p = p,
    p_n_ratio = p/n
  )
)
saveRDS(results, "processed_financial_data.rds")

# Print final summary
cat("\n\nFinal Summary:")
cat("\nTotal observations (n):", n)
cat("\nTotal variables (p):", p)
cat("\nStocks analyzed:", length(unique(combined_data$Symbol)))
cat("\nDate range:", min(combined_data$Date), "to", max(combined_data$Date))

# ============= STEP 10: APPLY CDPCA =============
# Choosing parameters for CDPCA
P <- 3  # Number of clusters for stocks
Q <- 2  # Number of clusters for variables

# Call the CDpca function
cdpc_result <- CDpca_simple(data = X_scaled, class = NULL, P = P, Q = Q,
                            tol = 1e-5, maxit = 100, r = 10, cdpcaplot = TRUE)

# Review results from CDPCA
cat("\nCDPCA Analysis Results:\n")
print(cdpc_result)

# =========== STEP 11: Visualise ======
Y <- cdpc_result[["Y"]]  # Get component score matrix
ggplot(as.data.frame(Y), aes(x=V1, y=V2)) +
  geom_point(aes(color=combined_data$Symbol)) +   # Color by stock symbol
  labs(title="Stock Data in CDPCA Space",
       x="Principal Component 1",
       y="Principal Component 2") +
  theme_minimal()

