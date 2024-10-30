library(dplyr)
library(ggplot2)
library(lubridate)
library(naniar)
library(corrplot)
df<-combined_data_merged
unique_movies <- n_distinct(df$MovieID)
cat("Toplam farkli film sayisi:", unique_movies, "\n")
# Her bir filmin oy sayisi
movie_counts <- df %>%
  count(MovieID)

gg_miss_var(df)



unique_customers <- n_distinct(df$CustomerID)  
cat("Toplam farklimusteri sayisi:", unique_customers, "\n")

unique_movies <- n_distinct(df$MovieID)  
cat("Toplam farkli film sayisi:", unique_movies, "\n")


rate_distribution <- df %>%
  count(Rate) %>%
  mutate(percentage = n / sum(n) * 100)  

# Gorsellestirme: Rate degerlerinin yuzdesel daglimi

ggplot(rate_distribution, aes(x = factor(Rate), y = percentage)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Oylarin Yuzdesel Dagilimi", x = "Rate Degerleri", y = "Yuzde (%)") +
  geom_text(aes(label = n), vjust = -0.5) +  
  
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = 1.5)  




# Yil ve ay bilgisi
df_date <- df %>%
  mutate(Year = format(Date, "%Y"),
         Month = format(Date, "%m"))

# Yil ve ay bazinda oy sayisini hesaplayalim
monthly_votes <- df_date %>%
  group_by(Year, Month) %>%
  summarise(TotalVotes = n()) %>%
  arrange(Year, Month)  

# Gorsellestirme2: Yillara gore aylik oy sayisi
ggplot(monthly_votes, aes(x = Month, y = Year, fill = TotalVotes)) +
  geom_tile(aes(width = 0.8, height = 0.8), color = "white") +  
  geom_text(aes(label = TotalVotes), color = "black", size = 4) +  
  scale_fill_gradient(low = "lightblue", high = "midnightblue") +  
  theme_minimal() +
  labs(title = "Yillara Gore Aylik Oy Verme Sayisi", x = "Aylar", y = "Yillar") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Her yil icin ortalama oy hesaplama
yearly_avg_rating <- df %>%
  group_by(Year = format(Date, "%Y")) %>%
  summarise(AvgRating = mean(Rate))
yearly_avg_rating


# Gorsellestirme3: Yillik ortalama oy dagilimi
ggplot(yearly_avg_rating, aes(x = Year, y = AvgRating)) +
  geom_line(group = 1, color = "red", size = 1.2) +
  geom_point(size = 3, color = "darkred") +
  theme_minimal() +
  labs(title = "Yillik Ortalama Oy", x = "Yil", y = "Oy ortalamasi")


lowraters<-customer_movie_count[customer_movie_count$TotalMoviesRated<=2,]

# Oy sayisi arttikca filmlerin aldigi rating yukseliyor mu ?

average_ratings <- df %>%
  group_by(MovieID) %>%
  summarise(
    AvgRating = mean(Rate),  
    VoteCount = n()          
  ) %>%
  arrange(desc(AvgRating))
average_ratings

# Gorsellestirme4 (Scatter plot): VoteCount ve AvgRating arasindaki iliskiyi gorsellestirme
ggplot(average_ratings, aes(x = VoteCount, y = AvgRating)) +
  geom_point(color = "blue", alpha = 0.5) +  
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  theme_minimal() +
  labs(title = "Oy Sayisi ile Ortalama Reyting Arasindaki iliski", 
       x = "Oy Sayisi", 
       y = "Ortalama Reyting") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold")   
  )






#Istatistiksel olarak anlamli mi ?
#(H0): Korelasyon katsayisi (r) = 0, yani oy sayisi ile ortalama reyting arasinda  istatistiksel olarak anlamli bir  iliski yoktur.
#(H1): Korelasyon katsayisi (r)  0, yani oy sayisi ile ortalama reyting arasinda istatistiksel olarak anlamli bir iliski vardir.

# Pearson korelasyon testi
cor_test_result <- cor.test(average_ratings$VoteCount, average_ratings$AvgRating)


print(cor_test_result)

#H0 hipotezi red, H1 reddilemez. 
#Oy sayisi ile ortalama reyting arasinda istatistiksel olarak anlamli bir pozitif iliski var. Bu, daha fazla oy alan filmlerin 
#ortalama olarak biraz daha yuksek reyting aldigini gosterir ancak korelasyon katsayisi 0,23 oldugundan
#bu iliski zayif bir iliskidir.
# Sonuc olarak  oy sayisi ile reyting arasinda bir iliski olmasina ragmen, oy sayisinin reytingler uzerinde cok buyuk bir etkisi oldugunu soylemek zor.





#: Musterilerin verdigi oylarin ortalamasi
customer_avg_ratings <- df %>%
  group_by(CustomerID) %>%
  summarise(TotalVotes = n(),                       
AvgRating = mean(Rate))


customer_avg_ratings

summary(customer_avg_ratings)
hist(movie_counts$n)


  
 
#1-2 veren kullanicilar o filmden atilacak. Neden?
#Aykiri degerler atilacak, %5 kirpilacak
#Deneme: hic film izlenmeyen veya az izlenen gunler
# Mevsimin en cok izlenen filmini izleyen veya izlemeyene onermek
  
  boxplot(customer_movie_count$TotalMoviesRated)
  
  
  filtered_df <- df %>%
    group_by(Rate) %>%
    filter(any(Rate >= 3)) %>%  
    ungroup()  
?filter()
  
  
  daily_votes <- df %>%
    group_by(Date = as.Date(Date)) %>%  
    summarise(TotalVotes = n()) %>%      
    ungroup()
  
  #  Hic oy verilmeyen veya az oy verilen gunler
  
  low_votes_days <- daily_votes %>%
    filter(TotalVotes < 20)  
  
  
  low_votes_days
  
  specific_date <- as.Date("2004-12-30")
  votes_on_specific_date <- df %>%
    filter(as.Date(Date) == specific_date) %>%  
    summarise(TotalVotes = n())








