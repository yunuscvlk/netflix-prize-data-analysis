# Load necessary libraries
library(tidyverse)
library(data.table)
library(recommenderlab)
library(ggplot2)

# Step 1: Load and Sample the Dataset
netflix_data <- fread("combined_data_merged.csv", select = c("MovieID", "CustomerID", "Rate"))
set.seed(123)
sampled_data <- netflix_data[sample(.N, 10000)]

# Step 2: Create a User-Item Matrix
data_aggregated <- sampled_data %>%
  group_by(CustomerID, MovieID) %>%
  summarise(Mean_Rate = mean(Rate), .groups = "drop")

user_item_matrix <- data_aggregated %>%
  pivot_wider(names_from = MovieID, values_from = Mean_Rate) %>%
  replace(is.na(.), 0) %>%
  column_to_rownames("CustomerID") %>%
  as.matrix()

rating_matrix <- as(user_item_matrix, "realRatingMatrix")

# Step 3: Set Up Evaluation Scheme
set.seed(123)
evaluation_scheme <- evaluationScheme(rating_matrix, method = "split", train = 0.9, given = -1, goodRating = 3)

# Step 4: Build Recommendation Models with Different Parameters

# 1. User-Based Collaborative Filtering (UBCF) with Cosine Similarity
optimized_ubcf <- Recommender(getData(evaluation_scheme, "train"), method = "UBCF", 
                              param = list(
                                method = "Cosine", 
                                normalize = "Z-score",  # Z-score normalization
                                nn = 15                 # Fine-tuned k (number of neighbors)
                              ))

# 2. Item-Based Collaborative Filtering (IBCF) with Pearson Similarity
optimized_ibcf <- Recommender(getData(evaluation_scheme, "train"), method = "IBCF", 
                              param = list(
                                method = "Pearson",
                                k = 30  # Tuning for item-based CF with 30 neighbors
                              ))

# 3. Singular Value Decomposition (SVD) Model
optimized_svd <- Recommender(getData(evaluation_scheme, "train"), method = "SVDF", param = list(k = 5))

# Step 5: Generate Predictions
ubcf_predictions <- predict(optimized_ubcf, getData(evaluation_scheme, "known"), type = "ratings")
ibcf_predictions <- predict(optimized_ibcf, getData(evaluation_scheme, "known"), type = "ratings")
svd_predictions <- predict(optimized_svd, getData(evaluation_scheme, "known"), type = "ratings")

# Step 6: Calculate RMSE, MSE, and MAE for Each Model
ubcf_error <- calcPredictionAccuracy(ubcf_predictions, getData(evaluation_scheme, "unknown"))
ibcf_error <- calcPredictionAccuracy(ibcf_predictions, getData(evaluation_scheme, "unknown"))
svd_error <- calcPredictionAccuracy(svd_predictions, getData(evaluation_scheme, "unknown"))

# Step 7: Combine Results into a Data Frame
error_metrics <- rbind(
  UBCF = ubcf_error,
  IBCF = ibcf_error,
  SVD = svd_error
) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Model")

# Step 8: Plot the Error Metrics for Comparison
# Reshape for easier plotting with ggplot2
error_metrics_long <- error_metrics %>%
  pivot_longer(cols = c(RMSE, MSE, MAE), names_to = "Metric", values_to = "Value")

# Plot RMSE, MSE, and MAE for each model
ggplot(error_metrics_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Error Metrics Comparison of Recommendation Models",
       x = "Model",
       y = "Error Value") +
  scale_fill_manual(values = c("RMSE" = "#E69F00", "MSE" = "#56B4E9", "MAE" = "#009E73")) +
  theme_minimal() +
  theme(legend.title = element_blank())