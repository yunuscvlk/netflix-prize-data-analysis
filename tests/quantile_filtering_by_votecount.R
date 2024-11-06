library(dplyr)
# Kullanicilarin verdigi toplam oy sayisi
user_ratings <- df %>%
  group_by(CustomerID) %>%
  summarise(vote_count = n())  

# 1. ve 4. ceyrekler
quantiles <- quantile(user_ratings$vote_count, probs = c(0.25, 0.75))
Q1 <- quantiles[1]  
Q4 <- quantiles[2]  

filtered_users <- user_ratings %>%
  filter(vote_count > Q1 & vote_count < Q4)


filtered_df <- df %>%
  filter(CustomerID %in% filtered_users$CustomerID)
