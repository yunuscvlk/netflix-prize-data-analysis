# Load necessary packages
if (!require("data.table")) install.packages("data.table")
if (!require("dplyr")) install.packages("dplyr")

library(data.table)
library(dplyr)

#"combined_data_merged.csv" R projeniz ile aynı klasörde bulunmalı.

# Step 1: Efficiently read a subset of the dataset using fread
data <- fread("combined_data_merged.csv", header = FALSE, col.names = c("MovieID", "CustomerID", "Rate", "Date"))

# Step 3: Take a subset of unique CustomerIDs and MovieIDs
# Limit the number of unique customers and movies to reduce matrix size
top_customers <- data %>%
  group_by(CustomerID) %>%
  summarise(n_ratings = n()) %>%
  arrange(desc(n_ratings)) %>%
  head(1000)  # Select top 1000 customers

top_movies <- data %>%
  group_by(MovieID) %>%
  summarise(n_ratings = n()) %>%
  arrange(desc(n_ratings)) %>%
  head(500)  # Select top 500 movies

# Step 4: Filter data to include only the top customers and movies
subset_data <- data %>%
  filter(CustomerID %in% top_customers$CustomerID & MovieID %in% top_movies$MovieID)

# Step 5: Aggregate the ratings by CustomerID and MovieID
data_aggregated <- subset_data[, .(Mean_Rate = mean(Rate)), by = .(CustomerID, MovieID)]

# Step 6: Create a User-Movie matrix manually
customers <- unique(data_aggregated$CustomerID)
movies <- unique(data_aggregated$MovieID)


# Initialize an empty matrix with customers as rows and movies as columns
ratings_matrix <- matrix(NA, nrow = length(customers), ncol = length(movies),
                         dimnames = list(customers, movies))


# Step 7: Fill the matrix with aggregated ratings
for (i in 1:nrow(data_aggregated)) {
  customer <- data_aggregated$CustomerID[i]
  movie <- data_aggregated$MovieID[i]
  rate <- data_aggregated$Mean_Rate[i]
  
  # Assign the rating to the corresponding cell in the matrix
  ratings_matrix[as.character(customer), as.character(movie)] <- rate
}

# Convert the matrix to a data frame if needed
ratings_df <- as.data.frame(ratings_matrix)

# Print the first few rows of the matrix to ensure it's correct
print(head(ratings_df))
rm(data)
# Now you can proceed with collaborative filtering on this subset