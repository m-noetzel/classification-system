# Please install all required packages using the install.packages function

### 1. Introduction ############################################################
# Firstly all packages need to be loaded

library(plyr)
library(tidyverse)
library(broom)
library(caret)
library(dslabs)
library(dplyr)
library(lubridate)
library(MASS)
library(matrixStats)
library(pdftools)
library(gridExtra)
library(purrr)
library(genefilter)
library(stringr)
library(stringi)
library(reshape2)
library(data.table)
library(recosystem)

### load the data
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

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

# remove some data from the enviroment 
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Define the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


### 2. Analysis ################################################################
### 2.1 Data pre-processing
# data structure
head(edx)

# format timestamp
edx <- edx %>% mutate(timestamp=as_datetime(timestamp))

# extract the year from the title
edx$year <- substr(edx$title ,nchar(as.character(edx$title))-4,nchar(as.character(edx$title))-1)
year <- edx %>% group_by(movieId) %>% summarize(years=year[1])
edx <- edx %>% dplyr::select(-year,-title)
head(year)

# generate dummy variables
genre <- edx %>%
  group_by(movieId) %>%
  dplyr::select(movieId,genres) %>%
  mutate(gComedy = ifelse(str_detect(genres,"Comedy"),1,0)) %>%
  mutate(gAction = ifelse(str_detect(genres,"Action"),1,0)) %>%
  mutate(gCrime = ifelse(str_detect(genres,"Crime"),1,0)) %>%
  mutate(gThriller = ifelse(str_detect(genres,"Thriller"),1,0)) %>%
  mutate(gSci_Fi = ifelse(str_detect(genres,"Sci_Fi"),1,0)) %>%
  mutate(gDrama = ifelse(str_detect(genres,"Drama"),1,0)) %>%
  mutate(gFantasy = ifelse(str_detect(genres,"Fantasy"),1,0)) %>%
  mutate(gAdventure = ifelse(str_detect(genres,"Adventure"),1,0)) %>%
  mutate(gWar = ifelse(str_detect(genres,"War"),1,0)) %>%
  mutate(gChildren = ifelse(str_detect(genres,"Children"),1,0)) %>%
  mutate(gRomance = ifelse(str_detect(genres,"Romance"),1,0)) %>%
  mutate(gAnimation = ifelse(str_detect(genres,"Animation"),1,0)) %>%
  mutate(gMystical = ifelse(str_detect(genres,"Mystical"),1,0)) %>%
  mutate(gMusical = ifelse(str_detect(genres,"Musical"),1,0)) %>% distinct_all() %>%
  dplyr::select(-genres)
head(genre)

# age_movie
firstRating_movie <- edx %>%
  dplyr::select(movieId,timestamp) %>%
  group_by(movieId) %>%
  summarise(firstRating_movie=min(timestamp)) %>%
  arrange(-desc(movieId))

age_movie <- edx %>%
  left_join(firstRating_movie ,by="movieId") %>%
  mutate(age_i =round(difftime(timestamp,firstRating_movie,units = "days"))) %>%
  group_by(movieId) %>%
  dplyr::select(age_i,userId,rating)

# find the best cluster size
max(age_movie$age_i)
min(age_movie$age_i)

# cluster the variable
age_movie_s <- age_movie %>% 
  mutate(days_i=ifelse(age_i<=1000,"1000",
                       ifelse(age_i<=2000,"2000",
                              ifelse(age_i<=3000,"3000",
                                     ifelse(age_i<=4000,"4000",
                                            ifelse(age_i<=5000,"5000","6000"))))))

age_movie_s <- age_movie_s %>% dplyr::select(-rating,-age_i)
head(age_movie_s)

# age_user
# to calculate the variable "age_user" the first timestamp given by a specific user has to 
# be calculated to get a reference point.
firstRating_user <- edx %>%
  dplyr::select(userId,timestamp) %>%
  group_by(userId) %>%
  summarise(firstRating_user=min(timestamp)) %>%
  arrange(-desc(userId))

# calculate variable
age_user <- edx %>%
  left_join(firstRating_user ,by="userId") %>%
  mutate(age_u =round(difftime(timestamp,firstRating_user,units = "days"))) %>%
  group_by(userId) %>%
  dplyr::select(age_u,movieId,rating)

# calculate the max,min values to pick the scale for a useful cluster
max(age_user$age_u)
min(age_user$age_u)

# cluster the variable
age_user_s <- age_user %>% 
  mutate(days_u=ifelse(age_u<=1000,"1000",
                       ifelse(age_u<=2000,"2000",
                              ifelse(age_u<=3000,"3000",
                                     ifelse(age_u<=4000,"4000","5000")))))

age_user_s <- age_user_s %>% dplyr::select(-rating,-age_u)
head(age_user_s)

# remove some data from the environment 
rm(age_movie,age_user)

## check variables for missing values
any(is.na(year))
any(is.na(genre))
any(is.na(age_movie_s))
any(is.na(age_user_s))

### 2.2 Data Exploration #######################################################
## userId
edx %>% group_by(userId) %>%
  summarise(n=n()) %>% head()

# user ratings
edx %>% group_by(userId) %>%
  summarise(bu=mean(rating)) %>%
  ggplot(aes(bu)) +
  geom_histogram(bins = 30,color="black") +
  ggtitle("User")

## movieId
edx %>% group_by(movieId) %>%
  summarise(n=n())

# movie ratings
edx %>% group_by(movieId) %>%
  summarise(bi=mean(rating)) %>%
  ggplot(aes(bi)) +
  geom_histogram(bins = 30,color="black") +
  ggtitle("Movie")

## year
# year Ratings
edx %>% left_join(year ,by="movieId") %>%
  group_by(years) %>%
  summarise(AvRating =mean(rating)) %>%
  ggplot(aes(years,AvRating)) +
  geom_point() +
  theme(text = element_text(size=7),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Year")

# Check the min,max year to define appropriate cluster
min(year$years)
max(year$years)

# standardize the variable "year"
year_s <- year %>%
  mutate(time_phase=ifelse(years<=1950,"40er or less",
                           ifelse(years<=1960,"50er",
                                  ifelse(years<=1970,"60er",
                                         ifelse(years<=1980,"70er",
                                                ifelse(years<=1990,"80er",
                                                       ifelse(years<=2000,"90er",
                                                              ifelse(years<=2004,"early 2000","late 2000"))))))))
# remove some data from the environment 
rm(year)

# boxplot of time_phases
edx %>%
  left_join(year_s ,by="movieId") %>%
  group_by(time_phase) %>%
  ggplot(aes(time_phase,rating)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean,geom="point", shape=20, size=3, color="red", fill="red") +
  ggtitle("Year")                                                            

## genres
# check number of genres
edx %>% group_by(genres) %>% summarise(n=n()) %>% nrow()

# plot top 10 genres
top_genres <- edx %>%
  group_by(genres) %>% 
  mutate(n=count(rating), avRating=mean(rating)) %>%
  dplyr::select(genres,avRating,n) %>%
  distinct(genres,.keep_all =T) %>%
  arrange(desc(n)) %>% head(10)
top_genres

# boxplot of top 10 genres
edx %>% filter(genres %in% top_genres$genres) %>%
  group_by(genres) %>%
  ggplot(aes(y = reorder(genres, rating,FUN = mean), x=rating)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean,geom="point", shape=20, size=3, color="red", fill="red") +
  ggtitle("Top 10 most rated genres") 

## age_user
# table of item numbers
age_user_s %>% 
  group_by(days_u) %>% summarise(n=n())

# boxplots of age_user
edx %>% left_join(age_user_s,by=c("movieId","userId")) %>% 
  group_by(days_u) %>%
  ggplot(aes(x = days_u, y=rating)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean,geom="point", shape=20, size=3, color="red", fill="red") +
  ggtitle("Age_user")

## age_movie
# table of item numbers
age_movie_s %>% 
  group_by(days_i) %>% summarise(n=n())

#boxplots of age_movie
edx %>% left_join(age_movie_s,by=c("movieId","userId")) %>% 
  group_by(days_i) %>%
  ggplot(aes(x = days_i, y=rating)) +
  geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean,geom="point", shape=20, size=3, color="red", fill="red") +
  ggtitle("Age_movie")

## 2.2.7 Modeling approach
## Generate Partitions
set.seed(1234, sample.kind = "Rounding")
test_index <- createDataPartition(edx$rating,times = 1,p=0.2,list = F)
train_set <- edx[-test_index,]
test_set0 <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- test_set0 %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by="genres")

# Add rows removed from test set back into train set
removed <- anti_join(test_set0, test_set)
edx <- rbind(train_set, removed)

rm(removed, test_set0)

### 3. Results #################################################################
## Just the average
# calculate the edx mean
mean(edx$rating)

# validate the predictions
prediction_av <- validation %>% mutate(results_av=mean(edx$rating)) %>%
  pull(results_av)

rmse_av <- RMSE(prediction_av,validation$rating)

# Write the result 
rmse_results <- tibble(Model="Average",  
                       RMSE = rmse_av)
rmse_results


## 3.1 Movie effect and regularization
# tune lambda by cross-validation

lambdas <- seq(0, 10, 0.25)

rmses_lm <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  bi <- train_set %>% 
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(bi, by = "movieId") %>%
    mutate(pred = mu + bi) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses_lm)  
lambdas[which.min(rmses_lm)]

# validate model 1
mu <- mean(edx$rating)

bi <- edx %>% 
  group_by(movieId) %>%
  summarize(bi = sum(rating - mu)/(n()+2.25))

predicted_ratings_1 <- validation %>% 
  left_join(bi, by = "movieId") %>%
  mutate(pred = mu + bi) %>%
  pull(pred)

# write results
rmse_1 <- RMSE(predicted_ratings_1,validation$rating)

rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Movie effect and reg",  
                                 RMSE = rmse_1))
rmse_results

## 3.2 Movie effect, user effect and regularization
# tune lambda by cross-validation

lambdas <- seq(0, 10, 0.25)

rmses_lm <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  bi <- train_set %>% 
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n()+l))
  
  bu <- train_set %>% 
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - bi - mu)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    mutate(pred = mu + bi + bu) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses_lm)  
lambdas[which.min(rmses_lm)]

# validate model 2
bi <- edx %>% 
  group_by(movieId) %>%
  summarize(bi = sum(rating - mu)/(n()+4.75))

bu <- edx %>% 
  left_join(bi, by="movieId") %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - bi - mu)/(n()+4.75))

prediction_2 <- validation %>% 
  left_join(bi, by = "movieId") %>%
  left_join(bu, by = "userId") %>%
  mutate(pred = mu + bi + bu) %>%
  pull(pred)

rmse_2 <- RMSE(prediction_2, validation$rating)

# write the result 
rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Movie effect, user effect and reg",  
                                 RMSE = rmse_2))

rmse_results

## 3.3 Movie effect, user effect, time phase effect and regularization
# tune lambda by cross-validation

lambdas <- seq(0, 10, 0.25)

rmses_lm <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  bi <- train_set %>% 
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n()+l))
  
  bu <- train_set %>% 
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - bi - mu)/(n()+l))
  
  by <- train_set %>%
    left_join(bi, by="movieId") %>%
    left_join(bu, by="userId") %>%
    left_join(year_s, by="movieId") %>%
    group_by(time_phase) %>%
    summarize(by = sum(rating - bi - bu - mu)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    left_join(year_s, by="movieId") %>%
    left_join(by, by = "time_phase") %>%
    mutate(pred = mu + bi + bu + by) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses_lm)  
lambdas[which.min(rmses_lm)]

# validate model 3
bi <- edx %>% 
  group_by(movieId) %>%
  summarize(bi = sum(rating - mu)/(n()+4.75))

bu <- edx %>% 
  left_join(bi, by="movieId") %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - bi - mu)/(n()+4.75))

by <- edx %>%
  left_join(bi, by="movieId") %>%
  left_join(bu, by="userId") %>%
  left_join(year_s, by="movieId") %>%
  group_by(time_phase) %>%
  summarize(by = sum(rating - bi - bu - mu)/(n()+4.75))

prediction_3 <- validation %>% 
  left_join(bi, by = "movieId") %>%
  left_join(bu, by = "userId") %>%
  left_join(year_s, by="movieId") %>%
  left_join(by, by = "time_phase") %>%
  mutate(pred = mu + bi + bu + by) %>%
  pull(pred)

rmse_3 <- RMSE(prediction_3, validation$rating)

# write the result 
rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Movie effect, user effect, time phase effect and reg",  
                                 RMSE = rmse_3))

rmse_results

## 3.4 Movie effect, user effect, time phase effect, genre effect and regularization
# tune lambda by cross-validation

rmses_lm <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  bi <- train_set %>% 
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n()+l))
  
  bu <- train_set %>% 
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - bi - mu)/(n()+l))
  
  by <- train_set %>%
    left_join(bi, by="movieId") %>%
    left_join(bu, by="userId") %>%
    left_join(year_s, by="movieId") %>%
    group_by(time_phase) %>%
    summarize(by = sum(rating - bi - bu - mu)/(n()+l))
  
  bg <- train_set %>%
    left_join(bi, by="movieId") %>%
    left_join(bu, by="userId") %>%
    left_join(year_s, by="movieId") %>%
    left_join(by, by="time_phase") %>%
    group_by(genres) %>%
    summarize(bg = sum(rating - bi - bu - by - mu)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    left_join(year_s, by="movieId") %>%
    left_join(by, by = "time_phase") %>%
    left_join(bg, by = "genres") %>%
    mutate(pred = mu + bi + bu + by + bg) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses_lm)  
lambdas[which.min(rmses_lm)]

# validate model 4
bi <- edx %>% 
  group_by(movieId) %>%
  summarize(bi = sum(rating - mu)/(n()+4.75))

bu <- edx %>% 
  left_join(bi, by="movieId") %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - bi - mu)/(n()+4.75))

by <- edx %>%
  left_join(bi, by="movieId") %>%
  left_join(bu, by="userId") %>%
  left_join(year_s, by="movieId") %>%
  group_by(time_phase) %>%
  summarize(by = sum(rating - bi - bu - mu)/(n()+4.75))

bg <- edx %>%
  left_join(bi, by="movieId") %>%
  left_join(bu, by="userId") %>%
  left_join(year_s, by="movieId") %>%
  left_join(by,by="time_phase") %>%
  group_by(genres) %>%
  summarize(bg = sum(rating - bi - bu - by - mu)/(n()+4.75))

prediction_4 <- validation %>% 
  left_join(bi, by = "movieId") %>%
  left_join(bu, by = "userId") %>%
  left_join(year_s, by="movieId") %>%
  left_join(by, by = "time_phase") %>%
  left_join(bg, by = "genres") %>%
  mutate(pred = mu + bi + bu + by + bg) %>%
  pull(pred)

rmse_4 <- RMSE(prediction_4, validation$rating)

# write the result 
rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Movie effect, user effect, time phase effect, genre effect and reg",  
                                 RMSE = rmse_4))

rmse_results


## 3.5 Movie effect, user effect, time phase effect, genre effect, age effect (movie) and regularization
# tune lambda by cross-validation

lambdas <- seq(0, 10, 0.25)

rmses_lm <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  bi <- train_set %>% 
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n()+l))
  
  bu <- train_set %>% 
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - bi - mu)/(n()+l))
  
  by <- train_set %>%
    left_join(bi, by="movieId") %>%
    left_join(bu, by="userId") %>%
    left_join(year_s, by="movieId") %>%
    group_by(time_phase) %>%
    summarize(by = sum(rating - bi - bu - mu)/(n()+l))
  
  bg <- train_set %>%
    left_join(bi, by="movieId") %>%
    left_join(bu, by="userId") %>%
    left_join(year_s, by="movieId") %>%
    left_join(by, by="time_phase") %>%
    group_by(genres) %>%
    summarize(bg = sum(rating - bi - bu - by - mu)/(n()+l))
  
  ba <- train_set %>%
    left_join(bi, by="movieId") %>%
    left_join(bu, by="userId") %>%
    left_join(year_s, by="movieId") %>%
    left_join(by, by="time_phase") %>%
    left_join(bg, by="genres") %>%
    left_join(age_movie_s, by=c("userId","movieId")) %>%
    group_by(days_i) %>%
    summarize(ba = sum(rating - bi - bu - by - bg - mu)/(n()+l))
  
  predicted_ratings <- test_set %>% 
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    left_join(year_s, by="movieId") %>%
    left_join(by, by = "time_phase") %>%
    left_join(bg, by = "genres") %>%
    left_join(firstRating_movie ,by="movieId") %>%
    mutate(age_i =round(difftime(timestamp,firstRating_movie,units = "days"))) %>%
    mutate(days_i=ifelse(age_i<=1000,"1000",
                         ifelse(age_i<=2000,"2000",
                                ifelse(age_i<=3000,"3000",
                                       ifelse(age_i<=4000,"4000",
                                              ifelse(age_i<=5000,"5000","6000")))))) %>%
    left_join(ba, by="days_i") %>%
    mutate(pred = mu + bi + bu + by + bg + ba) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses_lm)  
lambdas[which.min(rmses_lm)]

# validate model 5
bi <- edx %>% 
  group_by(movieId) %>%
  summarize(bi = sum(rating - mu)/(n()+5))

bu <- edx %>% 
  left_join(bi, by="movieId") %>%
  group_by(userId) %>%
  summarize(bu = sum(rating - bi - mu)/(n()+5))

by <- edx %>%
  left_join(bi, by="movieId") %>%
  left_join(bu, by="userId") %>%
  left_join(year_s, by="movieId") %>%
  group_by(time_phase) %>%
  summarize(by = sum(rating - bi - bu - mu)/(n()+5))

bg <- edx %>%
  left_join(bi, by="movieId") %>%
  left_join(bu, by="userId") %>%
  left_join(year_s, by="movieId") %>%
  left_join(by,by="time_phase") %>%
  group_by(genres) %>%
  summarize(bg = sum(rating - bi - bu - by - mu)/(n()+5))

ba <- edx %>%
  left_join(bi, by="movieId") %>%
  left_join(bu, by="userId") %>%
  left_join(year_s, by="movieId") %>%
  left_join(by, by="time_phase") %>%
  left_join(bg, by="genres") %>%
  left_join(age_movie_s, by=c("userId","movieId")) %>%
  group_by(days_i) %>%
  summarize(ba = sum(rating - bi - bu - by - bg - mu)/(n()+5))

prediction_5 <- validation %>% 
  left_join(bi, by = "movieId") %>%
  left_join(bu, by = "userId") %>%
  left_join(year_s, by="movieId") %>%
  left_join(by, by = "time_phase") %>%
  left_join(bg, by = "genres") %>%
  left_join(firstRating_movie ,by="movieId") %>%
  mutate(timestamp=as_datetime(timestamp)) %>%
  mutate(age_i = round(difftime(timestamp,firstRating_movie,units = "days"))) %>%
  mutate(days_i=ifelse(age_i<=1000,"1000",
                       ifelse(age_i<=2000,"2000",
                              ifelse(age_i<=3000,"3000",
                                     ifelse(age_i<=4000,"4000",
                                            ifelse(age_i<=5000,"5000","6000")))))) %>%
  left_join(ba, by="days_i") %>%
  mutate(pred = mu + bi + bu + by + bg + ba) %>%
  pull(pred)

rmse_5 <- RMSE(prediction_5, validation$rating)

# write the result 
rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Movie effect, user effect, time phase effect, genre effect, age effect and reg",  
                                 RMSE = rmse_5))

rmse_results

## 3.6 Matrix factorization ####################################################
# organize data in sparse matrix tripled form
edx_m <- edx %>% dplyr::select(userId,movieId, rating)
edx_m <- as.matrix(edx_m)

validation_m <- validation %>% dplyr::select(userId, movieId,rating)
validation_m <- as.matrix(validation_m)

# write the data sets into hard a disc
write.table(edx_m , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(validation_m, file = "validset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

# generate the final data sets to build the "recosystem"
set.seed(202)
training <- data_file("trainset.txt")
validating <- data_file("validset.txt")

# build a recommender object
r <- Reco()

# tuning the training set
opts <- r$tune(training, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))
opts

# train the recommender model
r$train(training,opts = c(opts$min,nthread=1,niter=20))

# validate the model
pred_file <- tempfile()
r$predict(validating,out_file(pred_file))
predicted_ratings_mf <- scan(pred_file)

rmse_mf <- RMSE(predicted_ratings_mf, validation$rating)

# write the result 
rmse_results <- bind_rows(rmse_results,
                          tibble(Model="Matrix factorization",  
                                 RMSE = rmse_mf))
rmse_results

################################################################################
### End of project
################################################################################