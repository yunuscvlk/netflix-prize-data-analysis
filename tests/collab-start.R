# Install and load necessary packages
if (!require("recosystem")) install.packages("recosystem")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")
if (!require("dplyr")) install.packages("dplyr")

library(recosystem)
library(ggplot2)
library(reshape2)
library(dplyr)

# Step 1: Create a small dataset (UserID, MovieID, Rating)
dataset <- data.frame(
  UserID = c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4),
  MovieID = c(1, 2, 3, 1, 3, 1, 2, 4, 2, 3, 4),
  Rating = c(5, 3, 4, 4, 2, 5, 4, 1, 2, 4, 3)
)


# Step 2: Create User-Movie matrix

#Hocanın istediği matrix
#The dcast function reshapes the data to create a matrix showing each user’s rating for each movie.

ratings_matrix <- dcast(dataset, UserID ~ MovieID, value.var = "Rating")
print("User-Movie Matrix:")
print(ratings_matrix)

# Step 3: Write data to a file and train a collaborative filtering model using recosystem
write.table(dataset, file = "train_data.txt", sep = " ", row.names = FALSE, col.names = FALSE)

r <- Reco()
train_data <- data_file("train_data.txt")
r$train(train_data, opts = list(dim = 10, lrate = 0.1, niter = 20))

# Step 4: Make predictions for User 1 on movies 1 to 4
user_id <- 1
items <- 1:4
test_data <- data_memory(user_index = rep(user_id, length(items)), item_index = items)
predicted_ratings <- r$predict(test_data, out_memory())

# Step 5: Create a dataframe for predicted ratings
predicted_df <- data.frame(
  MovieID = items,
  PredictedRating = predicted_ratings
)

# Show predicted ratings for User 1
print("Predicted Ratings for User 1:")
print(predicted_df)

# Step 6: Visualization of predicted ratings for User 1
ggplot(predicted_df, aes(x = factor(MovieID), y = PredictedRating)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(x = "Movie ID", y = "Predicted Rating", title = "Predicted Ratings for User 1") +
  theme_minimal()

# Step 7: Heatmap of the User-Movie Matrix (visualizing known ratings)
melted_matrix <- melt(ratings_matrix, id.vars = "UserID")
ggplot(melted_matrix, aes(x = factor(UserID), y = factor(variable), fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue", na.value = "grey") +
  labs(x = "User ID", y = "Movie ID", fill = "Rating", title = "User-Movie Ratings Heatmap") +
  theme_minimal()