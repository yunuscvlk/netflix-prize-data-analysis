# Load necessary libraries
library(tidyverse)
library(data.table)
library(recommenderlab)
library(ggplot2)

# Step 1: Load and Sample the Dataset
# Assuming your Netflix data is stored in a CSV file with columns "MovieID", "CustomerID", "Rate"
netflix_data <- fread("combined_data_merged.csv", select = c("MovieID", "CustomerID", "Rate"))

# Sample 10,000 entries randomly from the dataset
set.seed(123)
sampled_data <- netflix_data[sample(.N, 30000)]

# Step 2: Create a User-Item Matrix for collaborative filtering
# Aggregate by CustomerID and MovieID to get the mean rating (in case of duplicate entries)
data_aggregated <- sampled_data %>%
  group_by(CustomerID, MovieID) %>%
  summarise(Mean_Rate = mean(Rate), .groups = "drop")

# Convert to a wide format where rows represent users and columns represent movies
user_item_matrix <- data_aggregated %>%
  pivot_wider(names_from = MovieID, values_from = Mean_Rate) %>%
  replace(is.na(.), 0) %>%  # Replace NAs with 0 for missing ratings
  column_to_rownames("CustomerID") %>%
  as.matrix()

# Convert the matrix to a recommenderlab realRatingMatrix
rating_matrix <- as(user_item_matrix, "realRatingMatrix")

# Step 3: Define an Evaluation Scheme
set.seed(123)
evaluation_scheme <- evaluationScheme(rating_matrix, method = "split", train = 0.8, given = -1, goodRating = 3)

# Step 4: Build and Evaluate Multiple Collaborative Filtering Models
# Define a function to train and evaluate a model using specified method
train_evaluate_model <- function(method, param_list) {
  model <- Recommender(getData(evaluation_scheme, "train"), method = method, param = param_list)
  predictions <- predict(model, getData(evaluation_scheme, "known"), type = "ratings")
  calcPredictionAccuracy(predictions, getData(evaluation_scheme, "unknown"))
}

# Evaluate UBCF with Pearson similarity
ubcf_pearson_results <- train_evaluate_model("UBCF", list(normalize = "center", method = "Pearson", nn = 30))
cat("UBCF with Pearson Similarity:\n")
print(ubcf_pearson_results)

# Evaluate UBCF with Cosine similarity
ubcf_cosine_results <- train_evaluate_model("UBCF", list(normalize = "center", method = "Cosine", nn = 30))
cat("UBCF with Cosine Similarity:\n")
print(ubcf_cosine_results)

# Evaluate IBCF with Pearson similarity
ibcf_pearson_results <- train_evaluate_model("IBCF", list(normalize = "center", method = "Pearson", k = 30))
cat("IBCF with Pearson Similarity:\n")
print(ibcf_pearson_results)

# Evaluate IBCF with Cosine similarity
ibcf_cosine_results <- train_evaluate_model("IBCF", list(normalize = "center", method = "Cosine", k = 30))
cat("IBCF with Cosine Similarity:\n")
print(ibcf_cosine_results)



# Step 5: Summary of Results
# Store results in a data frame for easier comparison
evaluation_results <- data.frame(
  Model = c("UBCF Pearson", "UBCF Cosine", "IBCF Pearson", "IBCF Cosine"),
  RMSE = c(ubcf_pearson_results["RMSE"], ubcf_cosine_results["RMSE"], ibcf_pearson_results["RMSE"], 
           ibcf_cosine_results["RMSE"]),
  MSE = c(ubcf_pearson_results["MSE"], ubcf_cosine_results["MSE"], ibcf_pearson_results["MSE"], 
          ibcf_cosine_results["MSE"]),
  MAE = c(ubcf_pearson_results["MAE"], ubcf_cosine_results["MAE"], ibcf_pearson_results["MAE"], 
          ibcf_cosine_results["MAE"])
)

cat("Comparison of Different Recommendation Methods:\n")
print(evaluation_results)
# Step 6: Visualize the Results
# Reshape the data frame for easier plotting with ggplot2
evaluation_results_long <- evaluation_results %>%
  pivot_longer(cols = c(RMSE, MSE, MAE), names_to = "Metric", values_to = "Value")

# Plot RMSE, MSE, and MAE as a grouped bar plot for each model
ggplot(evaluation_results_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance Comparison of Collaborative Filtering Models",
       x = "Model Type",
       y = "Metric Value") +
  scale_fill_manual(values = c("RMSE" = "#E69F00", "MSE" = "#56B4E9", "MAE" = "#009E73")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Analyze which model performed the best based on RMSE, MSE, and MAE