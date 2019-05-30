if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1) # if using R 3.6.0: set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

## Naive RMSE 
mu_hat <- mean(edx$rating)
naive_rmse <- RMSE(validation$rating,mu_hat)
rmse_results <- data_frame(method = "Just the Average", RMSE = naive_rmse)

## compute b_i movieID
mu<- mean(edx$rating)
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarise(b_i = mean(rating - mu))

## predicted_rating by movie effect 
predicted_rating <- mu + validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_rating, validation$rating)
rmse_results <- bind_rows(rmse_results,data_frame(method = "Movie Effect Model", RMSE = model_1_rmse))

## user_avgs
user_avgs <- edx %>%
  left_join(movie_avgs,by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

## predicted_rating by user and movie effect
predicted_rating <- validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred= mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_rating, validation$rating)
rmse_results <- bind_rows(rmse_results,data_frame(method = "Movie & User Effect Model", RMSE = model_2_rmse))

## regurlization 
lambdas <- seq(0,10,0.25)
rmses <- sapply(lambdas,function(l){
  mu<- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n() +l))
              
  b_u <- edx %>%
    left_join(b_i, by= 'movieId') %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i -mu)/(n() +l))
  
  predicted_ratings <- validation %>%
    left_join(b_i, by ='movieId') %>%
    left_join(b_u, by ='userId') %>%
    
    mutate(pred = mu+b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
 
})
qplot(lambdas, rmses) 
lambdas[which.min(rmses)]

lambda <- 5.25

  mu<- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu)/(n() +lambda))
  
  b_u <- edx %>%
    left_join(b_i, by= 'movieId') %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i -mu)/(n() +lambda))
  
  predicted_ratings <- validation %>%
    left_join(b_i, by ='movieId') %>%
    left_join(b_u, by ='userId') %>%
    
    mutate(pred = mu+b_i + b_u) %>%
    .$pred
  model_3_rmse <- RMSE(predicted_ratings, validation$rating)
  
rmse_results <- bind_rows(rmse_results,data_frame(method = "regularization with lambda Model", RMSE = model_3_rmse))
print(rmse_results)

#final RMSE result.
rmse_results %>% knitr::kable()






