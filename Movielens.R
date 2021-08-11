##########################################################
# HarvardX (PH125.9x) - Movielens Capstone Project - HarvardX (PH125.9x) - Konstantinos Liakopoulos
##########################################################



# Data Preparation

#Import libraries and seed.


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require("corrplot")) install.packages("corrplot")
if (!require("lubridate")) install.packages("lubridate")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(corrplot)

options(warn = -1) 
options(digits = 5)   
options(scipen = 999) 


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#Prepare the  data

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
# 
# # if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")


removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

anyNA(edx)
anyNA(validation)



#Investigate  data

head(edx)
str(edx)


#Add yearReleased,yearRated,yearsTorate  columns

edx <- mutate(edx, yearReleased =as.numeric(str_sub(title,-5, -2)))
edx <- mutate(edx, yearRated = year(as_datetime(timestamp)))
edx <- mutate(edx, yearsTorate = yearRated - yearReleased)



validation <- mutate(validation,yearReleased=as.numeric(str_sub(title,-5, -2)))
validation <- mutate(validation, yearRated = year(as_datetime(timestamp)))
validation <- mutate(validation, yearsTorate = yearRated - yearReleased)



# Data analysis

head(edx)
dim(edx)
str(edx)

# Unique users and unique movies

edx %>% summarize(no_users  = n_distinct(userId),
                  no_movies = n_distinct(movieId)) %>% knitr::kable()
##

users <- sample(unique(edx$userId), 180)
rafalib::mypar()
edx %>% filter(userId %in% users) %>%
  dplyr::select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% dplyr::select(sample(ncol(.), 180)) %>%
  as.matrix() %>% t(.) %>%
  image(1:180, 1:180,. , xlab="Movies", ylab="Users")
abline(h=0:180+0.5, v=0:180+0.5, col = "grey")

#


edx %>% count(movieId) %>% ggplot(aes(n))+
  geom_histogram(color = "black")+
  scale_x_log10()+
  ggtitle("Rating Count Per Movie")


edx %>% count(userId) %>% ggplot(aes(n))+
  geom_histogram(color = "black")+
  ggtitle("Rating Count Per User")+
  scale_x_log10()

#correlation plot - correlation between rating and yearReleased and YearsToRate

temp <- edx %>% select(one_of("rating", "movieId", "userId", "yearReleased","yearRated","yearsTorate")) %>% as.matrix()
M <- cor(temp, use = "pairwise.complete.obs")

corrplot(M)

#Mean rating is 3.5 -distribution of the rating frequency.

mean(edx$rating)

edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25) +
  scale_y_continuous(labels = scales::comma) +
  stat_count(aes(y=..count..,label=..count..),geom="text",vjust=-1) +
  ggtitle("Rating Frequency")

#Ratings Per Release Year

edx %>% ggplot(aes(x=yearReleased)) +
  geom_histogram(binwidth = 2) + 
  ggtitle("Ratings Per Release Year") +
  xlab("Year Released") +
  ylab("Number of Ratings")

#yearsTorate effect

edx %>%
  group_by(yearsTorate) %>%
  summarize(m_rating = mean(rating)) %>%
  ggplot(aes(yearsTorate,m_rating)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x) +
  ggtitle("Years Between Release And Rate")

edx %>%
  group_by(yearsTorate) %>%
  filter(n() >= 10000) %>%
  summarize(m_rating = mean(rating)) %>%
  ggplot(aes(yearsTorate,m_rating)) +
  geom_point() +
  geom_smooth(method = 'loess', formula = y ~ x) +
  ggtitle("Years Between Release And Rate (>10000 Entries)")

##genre effect

number_of_genres <- edx %>% group_by(genres) %>% summarize(n = n())

number_of_genres



edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 10000 & str_count(genres,"\\|")<2) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000 & str_count(genres,"\\|")<2) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Average Rate By Genre (More Than 100000 Rates)")

# residual mean square error RMSE

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# set project target

set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating,
times = 1, p = 0.1, list = FALSE)

edx_test <- edx[test_index]
edx_train <- edx[-test_index]
temp <- edx_test
edx_test <- temp %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")
removed <- anti_join(temp, edx_test, by = c("userId", "movieId"))
edx_train <- rbind(edx_train, removed)
rm(temp,removed)



rmse_results <- tibble(method = "Project Target", RMSE = 0.86490)
rmse_results %>% knitr::kable()

# simplest model - Average Movie Rating Model

mu <- mean(edx_train$rating)
mu



naive_rmse <- RMSE(edx_test$rating, mu)


rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Average Movie Rating Model",
                                     RMSE = naive_rmse ))
rmse_results %>% knitr::kable()

## Movie Effect Model

movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))


predicted_ratings <- mu + edx_test %>%
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

# Movie Effect and User Effect Model



user_avgs <- edx_train %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred


model_2_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effect Model",
                                     RMSE = model_2_rmse ))

rmse_results %>% knitr::kable()


## Movie and User and YearsToRate Effect Model




yearsTorate_avgs <- edx_train %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(yearsTorate) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u))


predicted_ratings <- edx_test%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(yearsTorate_avgs, by='yearsTorate') %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  .$pred



model_3_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
data_frame(method="Movie + User + YearsToRate Effect Model",
RMSE = model_3_rmse ))

rmse_results %>% knitr::kable()


## Movie and User and YearsToRate and Genre Effect Model


genre_avgs <- edx_train %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(yearsTorate_avgs, by='yearsTorate') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u- b_y))


predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(yearsTorate_avgs, by='yearsTorate') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  .$pred



model_4_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User + YearsToRate + Genre Effect Model",
                                     RMSE = model_4_rmse ))

rmse_results %>% knitr::kable()







# Regularized Movie  Effect Model

lambdas <- seq(0, 10, 1)

mu <- mean(edx_train$rating)


just_the_sum <- edx_train %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n())


rmses <- sapply(lambdas, function(l){
  predicted_ratings <- edx_test %>%
    left_join(just_the_sum, by='movieId') %>%
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, edx_test$rating))
})
qplot(lambdas, rmses)

lambdas[which.min(rmses)]

model_5_rmse <- RMSE(predicted_ratings, edx_test$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",
                                     RMSE = min(rmses)))

rmse_results %>% knitr::kable()


## Regularized Movie + User + YearsToRate + Genre Effect Model


lambdas_optimal <- seq(2, 8, 0.1)

rmses <- sapply(lambdas_optimal, function(l){

  mu <- mean(edx_train$rating)

  b_i <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

  b_u <- edx_train %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

  b_y <- edx_train %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(yearsTorate) %>%
  summarize(b_y = sum(rating - b_u - b_i - mu)/(n()+l))

  b_g <- edx_train %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="yearsTorate") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_y - b_u - b_i - mu)/(n()+l))


  predicted_ratings <-
  edx_test %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="yearsTorate") %>%
  left_join(b_g, by="genres") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  pull(pred)

  return(RMSE(predicted_ratings, edx_test$rating))
})



qplot(lambdas_optimal, rmses, main = "Optimal Lambda") +
theme(plot.title = element_text(hjust = 0.5))

lambdas_optimal[which.min(rmses)]

rmse_results <- bind_rows(rmse_results,
data_frame(method="Regularized Movie + User + Genre + YearsToRate Effect Model",
RMSE = min(rmses)))


rmse_results %>% knitr::kable()

####### validation

lambdas_optimal <- seq(2, 8, 0.1)

rmses <- sapply(lambdas_optimal, function(l){
  
  mu <- mean(edx_train$rating)
  
  b_i <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- edx_train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(yearsTorate) %>%
    summarize(b_y = sum(rating - b_u - b_i - mu)/(n()+l))
  
  b_g <- edx_train %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="yearsTorate") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_y - b_u - b_i - mu)/(n()+l))
  
  
  predicted_ratings <-
    validation %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="yearsTorate") %>%
    left_join(b_g, by="genres") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})



# qplot(lambdas_optimal, rmses, main = "Optimal Lambda") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
lambdas_optimal[which.min(rmses)]

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Validation vs Regularized Movie+User+Genre+YearsToRate Effect Model",
                                     RMSE = min(rmses)))


rmse_results %>% knitr::kable()