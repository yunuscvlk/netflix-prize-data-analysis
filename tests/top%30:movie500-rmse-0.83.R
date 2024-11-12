# Load necessary libraries
library(tidyverse)
library(data.table)
library(recommenderlab)

# Load the Dataset
netflix_data <- fread("combined_data_merged.csv", select = c("MovieID", "CustomerID", "Rate"))

# Step 1: Define thresholds and sample size limits
customer_review_counts <- netflix_data %>%
  group_by(CustomerID) %>%
  summarise(review_count = n()) %>%
  ungroup()

top_30_threshold <- quantile(customer_review_counts$review_count, 0.7)
lower_threshold <- quantile(customer_review_counts$review_count, 0.25)
upper_threshold <- quantile(customer_review_counts$review_count, 0.75)

# Step 2: Select two subsets of data based on customer activity
top_30_customers <- customer_review_counts %>%
  filter(review_count >= top_30_threshold) %>%
  select(CustomerID) %>%
  slice_sample(n = 5000)  # Limit to 5,000 users for memory efficiency

middle_customers <- customer_review_counts %>%
  filter(review_count >= lower_threshold & review_count <= upper_threshold) %>%
  select(CustomerID) %>%
  slice_sample(n = 5000)

top_30_data <- netflix_data %>% filter(CustomerID %in% top_30_customers$CustomerID)
middle_data <- netflix_data %>% filter(CustomerID %in% middle_customers$CustomerID)

# Step 3: Filter for Top Movies to Reduce Columns
top_movies <- netflix_data %>%
  group_by(MovieID) %>%
  summarise(movie_count = n()) %>%
  arrange(desc(movie_count)) %>%
  head(300) %>%  # Use only top 300 movies for further memory reduction
  select(MovieID)

top_30_data <- top_30_data %>% filter(MovieID %in% top_movies$MovieID)
middle_data <- middle_data %>% filter(MovieID %in% top_movies$MovieID)

# Step 4: Prepare Sparse User-Item Matrices
prepare_user_item_matrix <- function(data) {
  data_aggregated <- data %>%
    group_by(CustomerID, MovieID) %>%
    summarise(Mean_Rate = mean(Rate), .groups = "drop")
  
  user_item_matrix <- data_aggregated %>%
    pivot_wider(names_from = MovieID, values_from = Mean_Rate) %>%
    replace(is.na(.), 0) %>%
    column_to_rownames("CustomerID") %>%
    as.matrix()
  
  as(as(user_item_matrix, "dgCMatrix"), "realRatingMatrix")  # Convert to sparse realRatingMatrix
}

top_30_matrix <- prepare_user_item_matrix(top_30_data)
middle_matrix <- prepare_user_item_matrix(middle_data)

# Step 5: Define Function to Train and Evaluate Models
train_evaluate_model <- function(rating_matrix, method, k_neighbors = NULL, k_features = NULL) {
  eval_scheme <- evaluationScheme(rating_matrix, method = "split", train = 0.8, given = -1, goodRating = 3)
  
  if (method == "UBCF") {
    model <- Recommender(getData(eval_scheme, "train"), method = "UBCF", param = list(method = "Cosine", nn = k_neighbors))
  } else if (method == "SVD") {
    model <- Recommender(getData(eval_scheme, "train"), method = "SVDF", param = list(k = k_features))
  }
  
  predictions <- predict(model, getData(eval_scheme, "known"), type = "ratings")
  calcPredictionAccuracy(predictions, getData(eval_scheme, "unknown"))
}

# Step 6: Train and Evaluate Both Models on Both Samples
# Model on Top 30% sample
ubcf_top30_results <- train_evaluate_model(top_30_matrix, method = "UBCF", k_neighbors = 15)
svd_top30_results <- train_evaluate_model(top_30_matrix, method = "SVD", k_features = 30)

# Model on 25th-75th Percentile sample
ubcf_middle_results <- train_evaluate_model(middle_matrix, method = "UBCF", k_neighbors = 15)
svd_middle_results <- train_evaluate_model(middle_matrix, method = "SVD", k_features = 30)

# Step 7: Print Evaluation Results
cat("UBCF Top 30% RMSE:", ubcf_top30_results["RMSE"], "\n")
cat("SVD Top 30% RMSE:", svd_top30_results["RMSE"], "\n")
cat("UBCF Middle 25-75% RMSE:", ubcf_middle_results["RMSE"], "\n")
cat("SVD Middle 25-75% RMSE:", svd_middle_results["RMSE"], "\n")