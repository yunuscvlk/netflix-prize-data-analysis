if (!require(data.table)) install.packages("data.table")
if (!require(recosystem)) install.packages("recosystem")
if (!require(tictoc))     install.packages("tictoc")

# Load required libraries
library(data.table) # For efficient data handling
library(recosystem)
library(tictoc)     # For matrix factorization

# ------------------------- STEP 1: Load Data ---------------------------------
train_data <- fread("../dist/combined_data_merged.csv")  # Read training data

# Rename columns to standard names
colnames(train_data) <- c("CustomerID", "MovieID", "Rating", "Date")

# Check for missing values
any(is.na(train_data))

# ------------------------- STEP 2: Split Data ---------------------------------
set.seed(42)  # For reproducibility
train_indices <- sample(1:nrow(train_data), size = 0.8 * nrow(train_data))
train_set <- train_data[train_indices]
validation_set <- train_data[-train_indices]

# ------------------ STEP 3: Train Recommender Model --------------------------
tic()
reco <- Reco()  # Initialize the recommender model

# Write training data to a temporary file
train_temp_file <- tempfile()
write.table(
  train_set[, .(CustomerID, MovieID, Rating)], 
  file = train_temp_file, row.names = FALSE, col.names = FALSE
)

reco$train(train_data = data_file(train_temp_file), opts = list(
  dim = 300,          # Number of latent factors
  lrate = 0.05,       # Learning rate
  costp_l2 = 0.01,    # Regularization term
  nthread = 4,        # Number of threads
  niter = 20          # Number of iterations
))

# ------------------ STEP 4: Validate the Model --------------------------------
# Write validation data to a temporary file
validation_temp_file <- tempfile()
write.table(
  validation_set[, .(CustomerID, MovieID, Rating)], 
  file = validation_temp_file, row.names = FALSE, col.names = FALSE
)

# Make predictions on the validation set
validation_predictions <- reco$predict(test_data = data_file(validation_temp_file), out_memory())

# Compute RMSE for validation set
rmse <- sqrt(mean((validation_set$Rating - validation_predictions)^2, na.rm = TRUE))
cat(sprintf("Validation RMSE: %.4f\n", rmse))
toc()

# ------------------ STEP 5: Predict Ratings for Qualifying Data ---------------
qualifying_data <- fread("../dist/qualifying.csv") # Read qualifying data

# Rename columns to match the input data structure
colnames(qualifying_data) <- c("MovieID", "CustomerID", "Date")

# Remove unnecessary columns (e.g., Date is not needed for predictions)
qualifying_data <- qualifying_data[, .(CustomerID, MovieID)]

# Write qualifying data to a temporary file
qualifying_temp_file <- tempfile()
write.table(
  qualifying_data,
  file = qualifying_temp_file, row.names = FALSE, col.names = FALSE
)

qualifying_predictions <- reco$predict(test_data = data_file(qualifying_temp_file), out_memory())

# Add predictions to the qualifying dataset
qualifying_data[, PredictedRating := qualifying_predictions]

# Save predictions
output_file <- "../dist/predictions.csv"  
fwrite(qualifying_data, file = output_file)

cat("Predictions saved to:", output_file, "\n")
