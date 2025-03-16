if (!require(dplyr))      install.packages("dplyr")
if (!require(ggplot2))    install.packages("ggplot2")
if (!require(lubridate))  install.packages("lubridate")
if (!require(naniar))     install.packages("naniar")
if (!require(corrplot))   install.packages("corrplot")
if (!require(data.table)) install.packages("data.table")

library(dplyr)
library(ggplot2)
library(lubridate)
library(naniar)
library(corrplot)
library(data.table)

df <- fread("../dist/combined_data_merged.csv")

# Total number of unique movies
unique_movies <- n_distinct(df$MovieID)
cat("Total unique movies:", unique_movies, "\n")

# Number of ratings per movie
movie_counts <- count(df, MovieID)

# Visualizing missing data
gg_miss_var(df)

# Total number of unique customers
unique_customers <- n_distinct(df$CustomerID)  
cat("Total unique customers:", unique_customers, "\n")

# Total number of unique movies (repeated)
unique_movies <- n_distinct(df$MovieID)  
cat("Total unique movies:", unique_movies, "\n")

# Distribution of ratings
rate_distribution <- count(df, Rate) |> mutate(percentage = n / sum(n) * 100)

# Visualization: Percentage distribution of ratings
ggplot(rate_distribution, aes(x = factor(Rate), y = percentage)) +
geom_bar(stat = "identity", fill = "lightblue", color = "black") +
theme_minimal() +
labs(title = "Percentage Distribution of Ratings", x = "Rating Values", y = "Percentage (%)") +
geom_text(aes(label = n), vjust = -0.5) +  
geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = 1.5)  

# Extracting year and month information
df_date <- mutate(df, Year = format(Date, "%Y"), Month = format(Date, "%m"))

# Calculating number of ratings per year and month
monthly_votes <- df_date |> group_by(Year, Month) |> summarise(TotalVotes = n()) |> arrange(Year, Month)

# Visualization: Monthly rating count by year
ggplot(monthly_votes, aes(x = Month, y = Year, fill = TotalVotes)) +
geom_tile(aes(width = 0.8, height = 0.8), color = "white") +  
geom_text(aes(label = TotalVotes), color = "black", size = 4) +  
scale_fill_gradient(low = "lightblue", high = "midnightblue") +  
theme_minimal() +
labs(title = "Monthly Rating Count by Year", x = "Months", y = "Years") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Calculating average rating per year
yearly_avg_rating <- summarise(group_by(df, Year = format(Date, "%Y")), AvgRating = mean(Rate))
yearly_avg_rating

# Visualization: Yearly average rating distribution
ggplot(yearly_avg_rating, aes(x = Year, y = AvgRating)) +
geom_line(group = 1, color = "red", size = 1.2) +
geom_point(size = 3, color = "darkred") +
theme_minimal() +
labs(title = "Yearly Average Rating", x = "Year", y = "Average Rating")

# Identifying low-rating users
lowraters <- customer_movie_count[customer_movie_count$TotalMoviesRated <= 2,]

# Does the rating increase as the number of votes increases?
average_ratings <- summarise(
  group_by(df, MovieID),
  AvgRating = mean(Rate),
  VoteCount = n()
) |> arrange(desc(AvgRating))
average_ratings

# Visualization (Scatter plot): Relationship between VoteCount and AvgRating
ggplot(average_ratings, aes(x = VoteCount, y = AvgRating)) +
geom_point(color = "blue", alpha = 0.5) +  
geom_smooth(method = "lm", color = "red", se = FALSE) +  
theme_minimal() +
labs(
  title = "Relationship Between Number of Votes and Average Rating",
  x = "Number of Votes",
  y = "Average Rating"
) +
theme(
  plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
  axis.title.x = element_text(size = 14, face = "bold"),  
  axis.title.y = element_text(size = 14, face = "bold")   
)

# Statistical significance test
# (H0): Correlation coefficient (r) = 0, meaning there is no statistically significant relationship between
# the number of votes and the average rating.
# (H1): Correlation coefficient (r) ??? 0, meaning there is a statistically significant relationship between 
# the number of votes and the average rating.

# Pearson correlation test
cor_test_result <- cor.test(average_ratings$VoteCount, average_ratings$AvgRating)
print(cor_test_result)

# H0 hypothesis is rejected, H1 cannot be rejected.
# There is a statistically significant positive correlation between the number of votes and the average rating.
# However, since the correlation coefficient is 0.23, this relationship is weak.
# Therefore, even though there is a relationship between the number of votes and ratings, it is difficult to say
# that the number of votes has a significant impact on ratings.

# Average ratings given by customers
customer_avg_ratings <- summarise(group_by(df, CustomerID), TotalVotes = n(), AvgRating = mean(Rate))
customer_avg_ratings
summary(customer_avg_ratings)

# Histogram of movie rating counts
hist(movie_counts$n)

# Removing users who gave a rating of 1-2. Why?
# Outliers will be removed, 5% trimming will be applied.
# Experiment: Identifying days with no or very few movie views.
# Suggesting movies watched by the season's top viewers to those who haven't watched them.
boxplot(customer_movie_count$TotalMoviesRated)

# Filtering ratings where at least one rating is 3 or above
filtered_df <- df |> group_by(Rate) |> filter(any(Rate >= 3)) |> ungroup() 

# Counting daily votes
daily_votes <- summarise(group_by(df, Date = as.Date(Date)), TotalVotes = n()) |> ungroup()

# Identifying days with no or very few votes
low_votes_days <- filter(daily_votes, TotalVotes < 20)
low_votes_days

# Analyzing votes on a specific date
specific_date <- as.Date("2004-12-30")
votes_on_specific_date <- summarise(filter(df, as.Date(Date) == specific_date), TotalVotes = n())