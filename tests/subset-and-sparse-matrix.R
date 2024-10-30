# Load necessary libraries
if (!require("data.table")) install.packages("data.table")
if (!require("dplyr")) install.packages("dplyr")
if (!require("recommenderlab")) install.packages("recommenderlab")

library(data.table)
library(dplyr)
library(recommenderlab)

# Step 1: Efficiently read the dataset using fread
# This loads the main dataset, with column names: MovieID, CustomerID, Rate, Date
data <- fread("combined_data_merged.csv", header = FALSE, col.names = c("MovieID", "CustomerID", "Rate", "Date"))

# Step 2: Select a subset of popular customers and movies for memory efficiency
# We select the top 1000 customers and top 500 movies by frequency of ratings

# Find top 1000 customers who rated the most movies
top_customers <- data %>%
  group_by(CustomerID) %>%
  summarise(n_ratings = n()) %>%
  arrange(desc(n_ratings)) %>%
  head(1000)

# Find top 500 movies with the most ratings
top_movies <- data %>%
  group_by(MovieID) %>%
  summarise(n_ratings = n()) %>%
  arrange(desc(n_ratings)) %>%
  head(500)

# Step 3: Filter the main dataset to include only the top customers and movies
# This reduces the dataset size for faster processing
subset_data <- data %>%
  filter(CustomerID %in% top_customers$CustomerID & MovieID %in% top_movies$MovieID)

# Convert 'Rate' column to numeric to avoid any potential issues with data types
subset_data[, Rate := as.numeric(Rate)]

# Step 4: Calculate the average rating per CustomerID and MovieID
# We take the mean rating if a customer rated the same movie multiple times
data_aggregated <- subset_data[, .(Mean_Rate = mean(Rate, na.rm = TRUE)), by = .(CustomerID, MovieID)]

# Step 5: Create a User-Movie matrix, with customers as rows and movies as columns
# Initialize an empty matrix with unique CustomerID and MovieID
customers <- unique(data_aggregated$CustomerID)
movies <- unique(data_aggregated$MovieID)
ratings_matrix <- matrix(NA, nrow = length(customers), ncol = length(movies),
                         dimnames = list(customers, movies))

# Fill the matrix with ratings
for (i in 1:nrow(data_aggregated)) {
  customer <- as.character(data_aggregated$CustomerID[i])
  movie <- as.character(data_aggregated$MovieID[i])
  rating <- data_aggregated$Mean_Rate[i]
  
  ratings_matrix[customer, movie] <- rating
}

# Step 6: Convert the matrix to a sparse matrix format for recommenderlab
# This optimizes memory usage and speeds up collaborative filtering
ratings_matrix <- as(ratings_matrix, "realRatingMatrix")


# Step 7: Create a collaborative filtering model using User-Based Collaborative Filtering (UBCF)
# This recommends movies based on similarities between users
recommender_model <- Recommender(ratings_matrix, method = "UBCF")

# Step 8: Generate top-N recommendations for each user
top_n <- 20
recommendations <- predict(recommender_model, ratings_matrix, n = top_n)

# Step 9: Convert recommendations into a list format for easier viewing
# Each customer has a list of recommended movies
recommendations_list <- as(recommendations, "list")

# Step 10: Format the recommendations with CustomerID and MovieID
# This creates a data table with each CustomerID and their recommended MovieID(s)
recommendations_output <- data.table(CustomerID = rep(customers, each = top_n), 
                                     Recommended_MovieID = unlist(recommendations_list))

# Remove any rows with NA in recommended movies
recommendations_output <- recommendations_output[!is.na(Recommended_MovieID)]

# Print a sample of the recommendations to verify output
print(head(recommendations_output))

# Optional: Save the full recommendations to a CSV file
#fwrite(recommendations_output, "recommendations_output.csv")

# Step 11: Load and filter the probe dataset
# Probe dataset contains actual movies watched by users, used to validate our recommendations
probe_data <- fread("probe.csv", header = TRUE, col.names = c("MovieID", "CustomerID"))

# Filter probe data to include only the top 1000 customers and top 500 movies
probe_filtered <- probe_data %>%
  filter(CustomerID %in% top_customers$CustomerID & MovieID %in% top_movies$MovieID)

# Display filtered probe data and save to a CSV if needed
print(probe_filtered)
#fwrite(probe_filtered, "filtered_probe.csv")

# Convert both CustomerID and MovieID in recommendations_output to integer for compatibility
recommendations_output <- recommendations_output %>%
  mutate(CustomerID = as.integer(CustomerID),
         MovieID = as.integer(Recommended_MovieID)) %>%
  select(-Recommended_MovieID) # Remove the original column if no longer needed

# Step 12: Match recommendations with actual recommendations in the filtered probe data
# Use semi_join to keep only rows where CustomerID and MovieID match in both datasets
matched_recommendations <- recommendations_output %>%
  semi_join(probe_filtered, by = c("CustomerID", "MovieID"))

# Calculate accuracy as before
total_recommendations <- nrow(recommendations_output)
total_matches <- nrow(matched_recommendations)
accuracy_percentage <- (total_matches / total_recommendations) * 100

# Display the overall accuracy
cat("Overall Recommendation Accuracy:", round(accuracy_percentage, 2), "%\n")
print(head(matched_recommendations, 10))
