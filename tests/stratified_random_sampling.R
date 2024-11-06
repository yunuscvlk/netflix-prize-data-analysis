library(dplyr)
# Her kullanicinin verdigi toplam oy sayisi
user_ratings <- df %>%
  group_by(CustomerID) %>%
  summarise(vote_count = n())  


quantiles <- quantile(user_ratings$vote_count, probs = c(0.25, 0.75))
Q1 <- quantiles[1]  
Q3 <- quantiles[2]  

#  Dusuk, orta ve yuksek oy verenler
user_ratings <- user_ratings %>%
  mutate(
    group = case_when(
      vote_count <= Q1 ~ "low",
      vote_count > Q1 & vote_count <= Q3 ~ "medium",
      vote_count > Q3 ~ "high"
    )
  )

#Her gruptan %30 secilmesi
sample_percentage <- 0.3


sampled_users <- user_ratings %>%
  group_by(group) %>%
  sample_frac(sample_percentage)
sampled_data <- df %>%
  filter(CustomerID %in% sampled_users$CustomerID)
