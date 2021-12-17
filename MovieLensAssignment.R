
---
 # title: "Capstone Movie recommendation project"
 # author: "Riaz Mohamed Vellamparambil"
 # date: "12/07/2021"
---
  

# Install all needed libraries 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra" , repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

# Load required libraries

library(tidyverse)
library(caret)
library(kableExtra)
library(data.table)
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(plotly)
library(ggthemes)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

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

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

edx %>% as_tibble()
---
# Executive Summary
# The purpose for this project is to create a movie recommendation system using the MovieLens dataset provided
# in this course
# The movielens dataset  contains about 10 Milions of movies rating divided 
# The training dataset has  approximately 69878 users and 9000055 movies
# The idea is to build a recommendation system on this data which is evaluated based on RMSE- Root Mean Squared Error that 
# should be at least lower than **0.87750**.
---
  
set.seed(1, sample.kind="Rounding")

# create a test & train set

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-test_index,]
temp <- edx[test_index,]

# Matching userId and movieId in both train and test sets
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows baqck into the train set
removed <- anti_join(temp, test)
train <- rbind(train, removed)

rm(removed, temp, test_index)

# Table shows userId, movieId , ratings, timestamp, title and gnres
train %>% as_tibble()

# The output shows that the same user has rated multiple movies

# Lets us check the unique set
train %>% summarize(
  users=n_distinct(userId),
  movies=n_distinct(movieId),
  minRating=min(rating), 
  maxRating=max(rating) 
)

# we see there are 69878 users and 10677 unique movies with a ratings between 0.5 to 5
#  We also know that the same users have rated many movies , hence there maybe movies that users havent rated 
# and users who have not rated any movies. From this we understand that this set contains many NA values as well
# We are trying to fill the NA values 

# RATINGS
# Lets check movie ratings by number of ratings and average rating
edx %>% group_by(title) %>%
  summarize(avgRating = mean(rating),numberOfRatings = n()) %>%
  arrange(desc(avgRating)) %>%
  top_n(10, wt=avgRating)

# Are these movies actually the highly rated movies ?  The top 5 have only 1 rating
# to actually determine the highest rated movies we need at an average a higher number of polls for the movie itself
# Lets take the top 10 Rated movies which have at the least 500 ratings

edx %>% group_by(title) %>%
  summarize(numberOfRatings = n(), avgRating = mean(rating)) %>%
  filter(numberOfRatings > 500) %>%
  arrange(desc(avgRating)) %>%
  top_n(10, wt=avgRating)

# Distribution of Ratings
ratingDist <- ggplot(edx, aes(rating, fill = cut(rating, 100))) + 
  geom_histogram(color = "blue", binwidth = 0.2) + scale_x_continuous(breaks = seq(0.5, 5, 0.5)) + 
  labs(title = "Rating distrubution of movies", x = "Ratings", y = "No Of Rating") + 
  theme(axis.text = element_text(size = 10), 
  plot.title = element_text(size = 11, color = "red", hjust = 0.25)) 
  

ggplotly(ratingDist)

# CONCLUSION: The histogram confirms most viewers tended to rate a movie on an average at 3 or above


# THE GENRE EFFECT

edx_genres <-edx %>% separate_rows(genres, sep = "\\|")

edx_genres %>%as_tibble()

edx_genres %>%
  group_by(genres) %>% summarize(Ratings_Sum = n(), Average_Rating = mean(rating)) %>%
  arrange(-Ratings_Sum)


edx_genres %>%as_tibble()


# Arrange the Genres by Mean Rating
edx_genres %>%
  group_by(genres) %>% summarize(Ratings_Sum = n(), avgRating = mean(rating)) %>%
  arrange(-avgRating)

# CONCLUSION: The number number of ratings for the first four-five highly rated genres have less ratings and hence
# some of these can skew data as well. IMAX is not a genre.

# Visual representation
# Coerce genres from characters to factors
edx$genres <-as.factor(edx$genres)
edx_genres$genres <-as.factor(edx_genres$genres)


# Sum of Movie Ratings per Genre
genres_ratings_edx <-edx_genres %>% group_by(genres) %>% summarize(Ratings_Sum = n())
genres_ratings_edx %>% ggplot(aes(x = Ratings_Sum , y = reorder(genres, Ratings_Sum)))+
  ggtitle("Distribution")+
  xlab("Sum of Ratings")+
  ylab("Genres")+
  geom_bar(stat = "identity",  fill = "orange", color = "black")+
  theme(plot.title = element_text(hjust = 1.0))




# CONCLUSION: Avg ratings for genres are between 3 and 4. Genre doesn’t really have an effect on ratings.

#Mean rating per genre
genres_mean_ratings_edx <-edx_genres %>% group_by(genres) %>% summarize(avg_rating = mean(rating)) %>%
  arrange(-avg_rating)
genres_mean_ratings_edx %>% ggplot(aes(x = reorder(genres, avg_rating), y = avg_rating))+
  ggtitle("Mean Rating by Genre")+
  xlab("Genres")+
  ylab("Mean Rating")+
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(stat = "identity",  fill = "orange", color = "black")


# CONCLUSION: The number of ratings for the first 3-4 highly rated genres have less 
# ratings and hence some of these can skew data as well. We also have some false Genres
# Avg ratings for genres are between 3 and 4. Genre doesn’t really have an effect on ratings.


# AGE OF MOVIE 

# Convert Timestamps


# Let us now convert the time stamp to the rating year for the dataset

edx <- edx %>% mutate(timestamp = format(as.POSIXct(timestamp, origin = "1970-01-01", 
                                          tz = Sys.getenv("TZ")),"%Y"))
names(edx)[names(edx) == "timestamp"] <- "RatingYear"
head(edx)


# Let us do the same for the validation set

validation <- validation %>% mutate(timestamp = format(as.POSIXct(timestamp, origin = "1970-01-01", 
                                                                  tz = Sys.getenv("TZ")),"%Y"))
names(validation)[names(validation) == "timestamp"] <- "RatingYear"
head(validation)


# We will have to do the same for the train and testg sets

train <- train %>% mutate(timestamp = format(as.POSIXct(timestamp, origin = "1970-01-01", 
                                                        tz = Sys.getenv("TZ")),"%Y"))
names(train)[names(train) == "timestamp"] <- "RatingYear"
head(train)

test <-test %>% mutate(timestamp = format(as.POSIXct(timestamp, origin = "1970-01-01", 
                                                     tz = Sys.getenv("TZ")),"%Y"))
names(test)[names(test) == "timestamp"] <- "RatingYear"
head(test)


range(edx$RatingYear)


# CONCLUTION : A look the range seems to point out that all movies have been rated between 1995-2009

# Change RatingYear from character to numeric to plot the histogram
edx$RatingYear <-as.numeric(edx$RatingYear)
str(edx)

edx %>% ggplot(aes(RatingYear))+
  geom_histogram(fill = "blue", color = "black", bins = 35)+
  ggtitle("User Distribution")+
  xlab("Rating Year")+
  ylab("No of Users")+
  theme(plot.title = element_text(hjust = 0.5))


# CONCLUTION : 1996, 2000 and 2005 have the highest user ratings

edx %>%as_tibble()

# If we check the table we would see that a large number of the best movies were rated in 1996, 2000 and 2005




# To find the age of a movie we will need to first find the year it was released. The release year of the movie is 
# in the title

premierDate <- stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE ) %>% as.numeric()
edx <- edx %>% mutate(yearReleased = premierDate)
head(edx)

# The Issue with this is if there was a four digit number in the movie title then that will be taken, can 
# we see if this can be solved in an any other way

yearPremierDate <-as.numeric(str_sub(edx$title, start = -5, end = -2))
edx <- edx %>% mutate(yearReleased = yearPremierDate)
head(edx)


# Lets do the same for the other datasets we have



# Let us do the same for the validation set

validationPremierDate <-as.numeric(str_sub(validation$title, start = -5, end = -2))
validation <- validation %>% mutate(yearReleased = validationPremierDate)
head(validation)


# We will have to do the same for the train and test sets

trainPremierDate <- as.numeric(str_sub(train$title, start = -5, end = -2))
train <- train %>% mutate(yearReleased = trainPremierDate)
head(train)

testPremierDate <- as.numeric(str_sub(test$title, start = -5, end = -2))
test <- test %>% mutate(yearReleased = testPremierDate)
head(test)


# Movie age will be the current year minus the premierReleaseYear

edx <-edx %>% mutate(age_of_movie = 2021 - yearReleased)
validation <-validation %>% mutate(age_of_movie = 2021 - yearReleased)
train <-train %>% mutate(age_of_movie = 2021 - yearReleased)
test <-test %>% mutate(age_of_movie = 2021 - yearReleased)


summary(edx$age_of_movie)
summary(validation$age_of_movie)
summary(train$age_of_movie)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   23.00   27.00   30.78   34.00  106.00

# Let us check if the age of a movie affects the movie ratings

edx %>% group_by(age_of_movie) %>% summarize(avg_movie_rating = mean(rating)) %>% 
  ggplot(aes(age_of_movie, avg_movie_rating))+
  ggtitle("Age of Movie & Average Movie Rating")+
  xlab("Age of Movie")+
  ylab("Avg Movie Rating")+
  geom_smooth(method = "gam")+
  geom_point(color = "honeydew")+
  theme(plot.title = element_text(hjust = 0.5))

# CONCLUSION: We see the longer the movie is there , there are more ratings as well as some of the highly
# rated movies are there longer, which can also be reffered to as classics. This seems is a positive effect.

### Lets Model ###
# From the above analysis we know that the number of users, age of movie seems to influence the overall ratings 


#  RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### First Model : Naive ###
# For the first model we will predict the same rating for all movies . Here no bias is considered. This method 
# confirms to a linear equation Y u,i = Mu + E u,i . E u,i = independent errors centered at 0
mean_train_mu <-mean(train$rating)
naivermse <- RMSE(test$rating, mean_train_mu)
naivermse

## Save Results in Data Frame
rmse_results = data_frame(method = "Naive Analysis by Mean", RMSE = naivermse)
rmse_results %>% knitr::kable(., format = "pipe", padding = 2)

### Second Model : Median ###
#  Lets include the the median or any other random number. 
#  confirms to a linear equation Y u,i = Median + E u,i where E u,i = independent errors centered at 0

median_train_mu <-median(train$rating)
medianrmse <-RMSE(test$rating, median_train_mu)
medianrmse


rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Analysis By Median",
                                     RMSE = medianrmse))
rmse_results %>% knitr::kable(., format = "pipe", padding = 2)

### Third Model : Movie Effect ###
# From our previous analysis we know that some movies are rated more than others. So let us add another bias for 
# movie effect Y u,i = Mu + Movie Bias + E u,i

movieBias <- train %>% group_by(movieId) %>%
  summarize(mean_rating_m = mean(rating - mean_train_mu))


# Create the Prediction
prediction_mov_eff <-mean_train_mu + test %>% 
  left_join(movieBias, by = "movieId") %>% .$mean_rating_m
moviermse <-RMSE(test$rating, prediction_mov_eff)
moviermse

rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Analysis By Movie Bias",
                                     RMSE = moviermse))
rmse_results %>% knitr::kable(., format = "pipe", padding = 2)

### Fourth Model : Movie Effect & Users ###
# Now let us introduce users to the above. This is the user bias where users can rate movies as per their interest
# The equation for predicted rating Y u,i = Mu + Movie Bias + User Bias + E u,i (Mu -- Mean ratging and  E u,i Independent Error)


movieUserBias <- train %>% left_join(movieBias, by = "movieId") %>% group_by(userId) %>%
  summarize(mean_rating_m_u = mean(rating - mean_train_mu - mean_rating_m))


# Create the Prediction
prediction_mov_usr_eff <-test %>% left_join(movieBias, by = "movieId") %>%
  left_join(movieUserBias, by = "userId") %>%
  mutate(predictions = mean_train_mu + mean_rating_m + mean_rating_m_u) %>% .$predictions
movie_usr_rmse <-RMSE(test$rating, prediction_mov_usr_eff)
movie_usr_rmse

rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Analysis By Movie  & User Bias",
                                     RMSE = movie_usr_rmse))
rmse_results %>% knitr::kable(., format = "pipe", padding = 2)


### Fifth Model : Movie Effect , Users  & Age of Movie ###
# Now let us introduce movie age to the above. This is the movie age bias where the more the number of years the more 
# the number of ratings
# The equation for predicted rating Y u,i = Mu + Movie Bias + User Bias + Movie Age Bias + E u,i (Mu -- Mean ratging and  E u,i Independent Error)


movieUserAgeBias <- train %>% left_join(movieBias, by = "movieId") %>% left_join(movieUserBias, by = "userId") %>%
  group_by(age_of_movie) %>% 
  summarize(mean_rating_m_u_a = mean(rating - mean_rating_m_u - mean_rating_m - mean_train_mu ))
  

# Create the Prediction
prediction_mov_usr_mage_eff <-test %>% left_join(movieBias, by = "movieId") %>%
  left_join(movieUserBias, by = "userId") %>% left_join(movieUserAgeBias, by = "age_of_movie") %>%
  mutate(predictions = mean_train_mu + mean_rating_m + mean_rating_m_u  + mean_rating_m_u_a) %>% .$predictions
movie_usr_mage_rmse <-RMSE(test$rating, prediction_mov_usr_mage_eff)
movie_usr_mage_rmse

rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Analysis By Movie, User & Movie Age Bias",
                                     RMSE = movie_usr_mage_rmse))
rmse_results %>% knitr::kable(., format = "pipe", padding = 2)

# Regularization

# we have observed that there was a good decrease in rmse when we had both movie and user bias , when we added age
# to the equation there was a very little difference
# Regularisation allows for reduced errors caused by movies with few ratings which can influence the prediction 
# and skew the error metric. The method uses a tuning parmeter, lambda, to minimise the RMSE. Modifying 
# movie bias and user bias for movies with limited ratings.



# lambdas is a tuning parameter. We can use cross-validation to choose it
# Movie & User Bias Model with Regularization
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  train_mu <- mean(train$rating)
  
  movie_bias_i <- train %>%
    group_by(movieId) %>%
    summarise(movie_bias_i = sum(rating - train_mu)/(n()+l))
  
  user_bias_u <- train %>%
    left_join(movie_bias_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(user_bias_u = sum(rating - movie_bias_i - train_mu)/(n()+l))
  
  predicted_ratings <- test %>%
    left_join(movie_bias_i, by = "movieId") %>%
    left_join(user_bias_u, by = "userId") %>%
    mutate(predictions = train_mu + movie_bias_i + user_bias_u) %>%.$predictions

  
  return(RMSE(predicted_ratings, test$rating))
})


qplot(lambdas, rmses)  
rmse_regularisation <- min(rmses)
rmse_regularisation


rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Movie and User Effect with Regularization",
                                     RMSE = rmse_regularisation))
rmse_results %>% knitr::kable(., format = "pipe", padding = 2)


# Validate the Model

# Let us validate the model with our validation set
### First Model : Naive Validation ###

mean_validation_mu <-mean(edx$rating)
naivermse_val <- RMSE(validation$rating, mean_validation_mu)
naivermse_val

## Save Results in Data Frame
rmse_results_val = data_frame(Method= "Naive Analysis by Mean", ValidationSet_RMSE = naivermse_val)
rmse_results_val %>% knitr::kable(., format = "pipe", padding = 2)

### Second Model : Median Validation###
#  Lets include the the median or any other random number. 
#  confirms to a linear equation Y u,i = Median + E u,i where E u,i = independent errors centered at 0

median_validation_mu <-median(edx$rating)
medianrmse_val <-RMSE(validation$rating, median_validation_mu)
medianrmse_val


rmse_results_val <- bind_rows(rmse_results_val, 
                          data_frame(Method="Analysis By Median",
                                     ValidationSet_RMSE = medianrmse_val))
rmse_results_val %>% knitr::kable(., format = "pipe", padding = 2)

### Third Model : Movie Effect ###
# From our previous analysis we know that some movies are rated more than others. So let us add another bias for 
# movie effect Y u,i = Mu + Movie Bias + E u,i

movieBias_val <- edx %>% group_by(movieId) %>%
  summarize(mean_rating_m_val = mean(rating - median_validation_mu))


# Create the Prediction
prediction_mov_eff_val <-median_validation_mu + validation %>% 
  left_join(movieBias_val, by = "movieId") %>% .$mean_rating_m_val
moviermse_val <-RMSE(validation$rating, prediction_mov_eff_val)
moviermse_val

rmse_results_val <- bind_rows(rmse_results_val, 
                          data_frame(Method="Analysis By Movie Bias",
                                     ValidationSet_RMSE = moviermse_val))
rmse_results_val %>% knitr::kable(., format = "pipe", padding = 2)

### Fourth Model : Movie Effect & Users ###
# Now let us introduce users to the above. This is the user bias where users can rate movies as per their interest
# The equation for predicted rating Y u,i = Mu + Movie Bias + User Bias + E u,i (Mu -- Mean ratging and  E u,i Independent Error)


movieUserBias_val <- edx %>% left_join(movieBias_val, by = "movieId") %>% group_by(userId) %>%
  summarize(mean_rating_m_u_val= mean(rating - mean_validation_mu - mean_rating_m_val))


# Create the Prediction
prediction_mov_usr_eff_val <-validation %>% left_join(movieBias_val, by = "movieId") %>%
  left_join(movieUserBias_val, by = "userId") %>%
  mutate(predictions = mean_validation_mu + mean_rating_m_val + mean_rating_m_u_val) %>% .$predictions
movie_usr_rmse_val <-RMSE(validation$rating, prediction_mov_usr_eff_val)
movie_usr_rmse_val

rmse_results_val <- bind_rows(rmse_results_val, 
                          data_frame(Method="Analysis By Movie  & User Bias",
                                     ValidationSet_RMSE = movie_usr_rmse_val))
rmse_results_val %>% knitr::kable(., format = "pipe", padding = 2)


### Fifth Model : Movie Effect , Users  & Age of Movie ###
# Now let us introduce movie age to the above. This is the movie age bias where the more the number of years the more 
# the number of ratings
# The equation for predicted rating Y u,i = Mu + Movie Bias + User Bias + Movie Age Bias + E u,i (Mu -- Mean ratging and  E u,i Independent Error)


movieUserAgeBias_val <- edx %>% left_join(movieBias_val, by = "movieId") %>% left_join(movieUserBias_val, by = "userId") %>%
  group_by(age_of_movie) %>% 
  summarize(mean_rating_m_u_a_val = mean(rating - mean_rating_m_u_val - mean_rating_m_val - mean_validation_mu ))


# Create the Prediction
prediction_mov_usr_mage_eff_val <-validation %>% left_join(movieBias_val, by = "movieId") %>%
  left_join(movieUserBias_val, by = "userId") %>% left_join(movieUserAgeBias_val, by = "age_of_movie") %>%
  mutate(predictions = mean_validation_mu + mean_rating_m_val + mean_rating_m_u_val  + mean_rating_m_u_a_val) %>% .$predictions
movie_usr_mage_rmse_val <-RMSE(validation$rating, prediction_mov_usr_mage_eff_val)
movie_usr_mage_rmse_val

rmse_results_val <- bind_rows(rmse_results_val, 
                          data_frame(Method="Analysis By Movie, User & Movie Age Bias",
                                     ValidationSet_RMSE = movie_usr_mage_rmse_val))
rmse_results_val %>% knitr::kable(., format = "pipe", padding = 2)


# lambda$ is a tuning parameter. We can use cross-validation to choose it
# Movie & User Bias Model with Regularization
lambdaval <- seq(0, 10, 0.25)
rmses <- sapply(lambdaval, function(l){
  
  edx_mu <- mean(edx$rating)
  
  movie_bias_i_val <- edx %>%
    group_by(movieId) %>%
    summarise(movie_bias_i_val = sum(rating - edx_mu)/(n()+l))
  
  user_bias_u_val <- edx %>%
    left_join(movie_bias_i_val, by="movieId") %>%
    group_by(userId) %>%
    summarise(user_bias_u_val = sum(rating - movie_bias_i_val - edx_mu)/(n()+l))
  
  predicted_ratings_val <- validation %>%
    left_join(movie_bias_i_val, by = "movieId") %>%
    left_join(user_bias_u_val, by = "userId") %>%
    mutate(predictions = edx_mu + movie_bias_i_val + user_bias_u_val) %>%.$predictions
  
  
  return(RMSE(predicted_ratings_val, validation$rating))
})


qplot(lambdaval, rmses)  
rmse_regularisation_val <- min(rmses)
rmse_regularisation_val




rmse_results_val <- bind_rows(rmse_results_val, 
                          data_frame(Method="Movie and User Effect with Regularization",
                                     ValidationSet_RMSE = rmse_regularisation_val))
rmse_results_val %>% knitr::kable(., format = "pipe", padding = 2)




