# ============================================================================
# Netflix Prize Dataset - Collaborative Filtering Analysis
# ============================================================================
# Description:
# This script performs collaborative filtering on the Netflix Prize dataset.
# It applies three recommendation methods: User-Based CF (UBCF), 
# Item-Based CF (IBCF), and Singular Value Decomposition (SVD).
#
# The analysis is conducted on two different samples:
# 1. A randomly sampled subset of 30K ratings.
# 2. The top 30% most active users' ratings.
#
# Performance is evaluated using RMSE, and results are visualized.
# ============================================================================

# Load necessary libraries
library(tidyverse)       # Data wrangling
library(data.table)      # Fast data reading
library(recommenderlab)  # Collaborative filtering
library(ggplot2)         # Data visualization

# ----------------------------------------------------------------------------
# Step 1: Load and Sample Datasets
# ----------------------------------------------------------------------------

# Load 30K random sample dataset
netflix_30k <- fread("random_30k_sample.csv", select = c("MovieID", "CustomerID", "Rate"))

# Load full Netflix dataset to extract top 30% most active users
netflix_full <- fread("combined_data_merged.csv", select = c("MovieID", "CustomerID", "Rate"))

# ----------------------------------------------------------------------------
# Step 2: Prepare User-Item Matrix for 30K Sample
# ----------------------------------------------------------------------------

# Aggregate ratings by computing the mean for duplicates
data_aggregated_30k <- netflix_30k %>%
  group_by(CustomerID, MovieID) %>%
  summarise(mean_rate = mean(Rate), .groups = "drop")

# Convert to User-Item matrix
user_item_matrix_30k <- data_aggregated_30k %>%
  pivot_wider(names_from = MovieID, values_from = mean_rate) %>%
  replace(is.na(.), 0) %>%
  column_to_rownames("CustomerID") %>%
  as.matrix()

# Convert to realRatingMatrix format
rating_matrix_30k <- as(user_item_matrix_30k, "realRatingMatrix")

# ----------------------------------------------------------------------------
# Step 3: Extract Top 30% Most Active Users
# ----------------------------------------------------------------------------

# Count number of ratings per user
customer_review_counts <- netflix_full %>%
  group_by(CustomerID) %>%
  summarise(review_count = n(), .groups = "drop")

# Determine the threshold for top 30% most active users
top_30_threshold <- quantile(customer_review_counts$review_count, 0.7)

# Select only the most active users
top_30_customers <- customer_review_counts %>%
  filter(review_count >= top_30_threshold) %>%
  slice_sample(n = 5000)  # Limit to 5,000 users for efficiency

# Extract only ratings from the top 30% users
top_30_data <- netflix_full %>%
  filter(CustomerID %in% top_30_customers$CustomerID)

# Identify top 500 most-rated movies
top_movies <- netflix_full %>%
  group_by(MovieID) %>%
  summarise(movie_count = n(), .groups = "drop") %>%
  arrange(desc(movie_count)) %>%
  slice_head(n = 500)  # Retain top 500 movies

# Filter dataset to include only ratings for these top movies
top_30_data <- top_30_data %>%
  filter(MovieID %in% top_movies$MovieID)

# ----------------------------------------------------------------------------
# Step 4: Prepare Sparse User-Item Matrix for Top 30% Users
# ----------------------------------------------------------------------------

# Function to create a realRatingMatrix
prepare_user_item_matrix <- function(data) {
  data_aggregated <- data %>%
    group_by(CustomerID, MovieID) %>%
    summarise(mean_rate = mean(Rate), .groups = "drop")
  
  user_item_matrix <- data_aggregated %>%
    pivot_wider(names_from = MovieID, values_from = mean_rate) %>%
    replace(is.na(.), 0) %>%
    column_to_rownames("CustomerID") %>%
    as.matrix()
  
  return(as(as(user_item_matrix, "dgCMatrix"), "realRatingMatrix"))  # Convert to sparse matrix
}

# Create rating matrix for top 30% users
rating_matrix_top30 <- prepare_user_item_matrix(top_30_data)

# ----------------------------------------------------------------------------
# Step 5: Train & Evaluate Models
# ----------------------------------------------------------------------------

# Function to train & evaluate a recommendation model
train_evaluate_model <- function(rating_matrix, method, param_list) {
  evaluation_scheme <- evaluationScheme(rating_matrix, method = "split", train = 0.8, given = -1, goodRating = 3)
  model <- Recommender(getData(evaluation_scheme, "train"), method = method, param = param_list)
  predictions <- predict(model, getData(evaluation_scheme, "known"), type = "ratings")
  return(calcPredictionAccuracy(predictions, getData(evaluation_scheme, "unknown")))
}

# Train & evaluate models on 30K dataset
ubcf_30k_results <- train_evaluate_model(rating_matrix_30k, "UBCF", list(method = "Cosine", nn = 15))
ibcf_30k_results <- train_evaluate_model(rating_matrix_30k, "IBCF", list(method = "Pearson", k = 30))
svd_30k_results <- train_evaluate_model(rating_matrix_30k, "SVDF", list(k = 5))

# Train & evaluate models on Top 30% dataset
ubcf_top30_results <- train_evaluate_model(rating_matrix_top30, "UBCF", list(method = "Cosine", nn = 15))
ibcf_top30_results <- train_evaluate_model(rating_matrix_top30, "IBCF", list(method = "Pearson", k = 30))
svd_top30_results <- train_evaluate_model(rating_matrix_top30, "SVDF", list(k = 5))

# ----------------------------------------------------------------------------
# Step 6: Compare Performance Metrics
# ----------------------------------------------------------------------------

# Store results in a data frame
evaluation_results <- data.frame(
  Model = c("UBCF 30K", "UBCF Top 30%", "IBCF 30K", "IBCF Top 30%", "SVD 30K", "SVD Top 30%"),
  RMSE = c(ubcf_30k_results["RMSE"], ubcf_top30_results["RMSE"],
           ibcf_30k_results["RMSE"], ibcf_top30_results["RMSE"],
           svd_30k_results["RMSE"], svd_top30_results["RMSE"])
)

# Print results
cat("Performance Comparison of Different Recommendation Methods:\n")
print(evaluation_results)

# ----------------------------------------------------------------------------
# Step 7: Visualization of Performance Metrics
# ----------------------------------------------------------------------------

# Reshape results for ggplot2
evaluation_results_long <- evaluation_results %>%
  pivot_longer(cols = c(RMSE), names_to = "Metric", values_to = "Value")

# Plot RMSE for each model
ggplot(evaluation_results_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance Comparison (30K vs Top 30%)",
       x = "Model",
       y = "RMSE Value") +
  theme_minimal()