





# Preprocessing

RMSE <- function(y, g)
  return(sqrt(mean( (y-g)**2 )))

# List of packages we need
list_of_packages <- list("randomForest",
                         "kableExtra",
                         "tidyverse",
                         "lubridate",
                         "corrplot",
                         "ggplot2",
                         "readxl",
                         "dslabs",
                         "knitr",
                         "mlr")

# Function that loads and installs if necessary indicated packages
UsePackages = function(list_of_packages) {
  for (p in list_of_packages){
    # if (!is.element(p, installed.packages()[,1]))
    #   install.packages(p)
    require(p, character.only = TRUE)
  }
}
UsePackages(list_of_packages)

# precision
prec <- 3

# data frame to stock results
results <- tibble()





# Create test and validation sets

# Create edx set (train set) and validation set (test and validatiob set in the same time)

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


# Avoid downloading data we already have
if (file.exists("ml-10M100K/ratings.dat") & file.exists("ml-10M100K/movies.dat")){
  
  ratings <- read.table(text = gsub("::", "\t", readLines("ml-10M100K/ratings.dat")),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  movies <- str_split_fixed(readLines("ml-10M100K/movies.dat"), "\\::", 3)
  
}else{
  
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                        col.names = c("userId", "movieId", "rating", "timestamp"))
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  
}

# Treat data
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")


# Validation set will be 10% of MovieLens data
set.seed(1)
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

# change the format from 'data.frame' to 'tibble'. And blend the rows
edx <- as_tibble(edx)[sample(1:nrow(edx)),]
validation <- as_tibble(validation)[sample(1:nrow(validation)),]



# Feature Selection and Feature Engineering

# Feature Selection means that we remove irrelevant variables that only add noise.

# Feature Engineering means that we add new variables.


## Year of production and year of rating


# extract year from the title and remove title
edx <- edx %>%
  extract(title, "year", regex="\\(([0-9 \\-]*)\\)$") %>% mutate(year=as.integer(year))
validation <- validation %>%
  extract(title, "year", regex="\\(([0-9 \\-]*)\\)$") %>% mutate(year=as.integer(year))

# timestamp to year of the publication of rating
edx <- edx %>%
  mutate(timestamp = as.integer(year(as_datetime(timestamp))))
validation <- validation %>%
  mutate(timestamp = as.integer(year(as_datetime(timestamp))))


# Add mean/median rating per year


df_years <- edx %>% group_by(year) %>% 
  summarise(mean_per_year=mean(rating), median_per_year=median(rating))
edx <- edx %>% left_join(df_years, by="year")




## Genres


# data frame of genres and its numbers
(df_genres <- edx %>%
    separate_rows(genres, sep = "\\|") %>%
    group_by(genres) %>%
    summarise(number = n(), mean_rating = mean(rating)) %>%
    arrange(desc(number)))


# Divide 'genres' into separate genres


#' create a data.frame with dummy genres columns from a list of mixed genres
GetDummyGenres <- function(my_vector, my_genres){
  df2 <- sapply(my_vector,
                function(x){
                  zeros <- rep(0,length(my_genres))
                  x <- strsplit(x, "\\|")[[1]] # split by char "|" into two strings
                  zeros[match(x, my_genres)] <- 1
                  return(as.integer(zeros))
                }, 
                USE.NAMES=FALSE) %>% t
  colnames(df2) <- my_genres
  df2 <- df2 %>% as_tibble # %>% select(-`(no genres listed)`)# %>% mutate_all(as.factor)
  return(df2)
}

# movies and its genres
df_movies <- edx %>% group_by(movieId) %>%
  summarise("mean_per_movie"=mean(rating), 
            "median_per_movie"=median(rating), 
            "number"=n(),
            "genres" = genres[1])
df_movies <- df_movies %>% bind_cols(GetDummyGenres(.$genres, df_genres$genres))



# A movie can have multiple genres, so can estimate its rating as average of average ratings per genres


(df_movies <- df_movies %>% 
    mutate(mean_per_genre =
             rowSums(as.matrix(df_movies %>% select(df_genres$genres)) * df_genres$mean_rating) /
             rowSums(df_movies %>% select(df_genres$genres))) %>% 
    select(mean_per_genre, names(df_movies)))

edx <- edx %>% left_join(df_movies %>% select(movieId, mean_per_genre), by="movieId")




## Add mean/median rating per movie


edx <- edx %>% 
  left_join(df_movies %>% select(movieId, mean_per_movie, median_per_movie), by="movieId")


## Add mean/median rating per user


df_users <- edx %>% group_by(userId) %>% summarise("mean_per_user"=mean(rating),
                                                   "median_per_user"=median(rating),
                                                   "number"=n())
edx <- edx %>% left_join(df_users %>% select(-number), by="userId")







# Data Summary and Data Visualisation


# summary
print(summary(edx))
# check if there are NA in data
cat("Number of rows containing NA :", edx %>% filter(!complete.cases(.)) %>% nrow, "\n")

cat("Number of users  :", nrow(df_users), "\n")
cat("Number of movies :", nrow(df_movies), "\n")

# plot a Histogram of Ratings
ggplot(edx, aes(rating)) + 
  geom_histogram(binwidth=0.5, fill=I("blue"), col=I("red"), alpha=I(.2)) +
  ggtitle("Histogram of Ratings") + 
  scale_x_continuous(breaks=seq(0,5,.5))

# plot Mean Rating per Genre
ggplot(df_genres) + ggtitle("Mean Rating per Genre") + xlab(NULL) + ylab("Rating") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_point(aes(genres, mean_rating, size=number), col="tomato")
cat("We clearly see that the number of movies produced per year increases with time while the mean rating per year decreases\n")


# plot Mean Rating per Year and Timestamp
ggplot() + 
  geom_point(data = edx %>% group_by(year) %>% summarise(mean_rating = mean(rating), number=n()),
             aes(year, mean_rating, col="year", size=number)) + 
  geom_point(data = edx %>% group_by(timestamp) %>% summarise(mean_rating = mean(rating), number=n()),
             aes(timestamp, mean_rating, col="timestamp", size=number)) + 
  ggtitle("Mean Rating per Year and Timestamp") + xlab(NULL) + ylab("Rating")

# plot correlations
corrplot(cor(edx %>% select(-c(genres, movieId, userId))), type="upper", diag=FALSE, title="Correlation Matirx")
cat("We see that the rating is quite correlated with mean per user and mean per movie which are in their turn are highly correlated to its medians\n")

# edx is too big to plot it all, so the next analysis is done over a small part of edx
edx_short <- edx[1:1000,]

# plot ratings vs other variables
ggplot(edx_short) + geom_point(aes(rating, genres), col="#FF9999")
ggplot(edx_short) + geom_point(aes(rating, movieId), col="#56B4E9")
cat("we can see that some ratings are less frequant for some generes\n")

# plot histrograms
# df <- gather(edx_short %>% select(-c("genres", "timestamp")))
df <- gather(edx_short %>% select(userId, movieId, timestamp, year))
ggplot(df, aes(value, fill=key)) + 
  facet_wrap(~key, scales="free", ncol=1) +
  geom_histogram(bins=60)
cat("We see that our data is not homogeneous\n")

# Conclusion
cat("We do not see any obvious pattern that would certainly help us to determine a rating\n")



## Feature Importance


model_RandomForest <- randomForest(rating ~ ., 
                                   data = edx[1:1e5,] %>%
                                     select(-c(userId, movieId, genres, contains("median"))),
                                   ntree=10, keep.forest=FALSE, importance=TRUE)
varImpPlot(model_RandomForest)






# Model 0: Mean/Median Rating


# get mean and median and calculate rmse
rating_mean   <- mean(edx$rating)
rating_median <- median(edx$rating)
cat("Mean Rating:  ", round(rating_mean, prec),"\n")
cat("Median Rating:", round(rating_median, prec),"\n")

results_temp <- tibble("Model" = c("mean", "median"),
                       "RMSE"  = c(RMSE(validation$rating, rating_mean),
                                   RMSE(validation$rating, rating_median)))
# add it to results base
results <- results %>% bind_rows(results_temp)
# show results table in latex format
print(results_temp)



# Model 1: Mean/Median Rating per User


cat(min(df_users$number), max(df_users$number), "\n")
q_90 <- quantile(df_users$number, 0.9)
hist(df_users %>% filter(number<q_90) %>% .$number, breaks = 30)

# prediction
validation <- validation %>% left_join(df_users %>% select(-number), by="userId")

# get rmse and stock it
results_temp <- tibble("Model" = c("mean_per_user", "median_per_user"), 
                       "RMSE"  = c(RMSE(validation$rating, validation$mean_per_user),
                                   RMSE(validation$rating, validation$median_per_user)))
results <- results %>% bind_rows(results_temp)
# show results table in latex format
print(results_temp)





# Model 2: Mean/Median Rating per Movie


cat(min(df_movies$number), max(df_movies$number), "\n")
q_90 <- quantile(df_movies$number, 0.9)
hist(df_movies %>% filter(number<q_90) %>% .$number, breaks = 30)

# prediction
validation <- validation %>% 
  left_join(df_movies %>% select(movieId, mean_per_movie, median_per_movie), by="movieId")

# get rmse and stock it
results_temp <- tibble("Model" = c("mean_per_movie", "median_per_movie"),
                       "RMSE"  = c(RMSE(validation$rating, validation$mean_per_movie),
                                   RMSE(validation$rating, validation$median_per_movie)))
results <- results %>% bind_rows(results_temp)
# show results table in latex format
print(results_temp)





# Model 3 Mean/Median Rating per Year


# prediction
validation <- validation %>% left_join(df_years, by="year")

# get rmse and stock it
results_temp <- tibble("Model" = c("mean_per_year", "median_per_year"), 
                       "RMSE"  = c(RMSE(validation$rating, validation$mean_per_year),
                                   RMSE(validation$rating, validation$median_per_year)))
results <- results %>% bind_rows(results_temp)
# show results table in latex format
print(results_temp)



# Model 4 Mean Rating per Genre


# prediction
validation <- validation %>% left_join(df_movies %>% select(movieId, mean_per_genre), by="movieId")

# get rmse and stock it
results_temp <- tibble("Model" = "mean_per_genre", 
                       "RMSE"  = RMSE(validation$rating, validation$mean_per_genre))
results <- results %>% bind_rows(results_temp)
# show results table in latex format
kable(results_temp, "latex", booktabs = TRUE) %>% kable_styling(latex_options = "striped")




# Model 5 Ordinary Least Squares regression (OLS)


# data for regression type train
edx2 <- edx %>% select(-c(userId, movieId, genres, contains("median")))



ols <- lm(rating ~ ., data = edx2)
summary(ols)
# test
results_temp <- tibble("Model" = "ols",
                       "RMSE"  = RMSE(validation$rating, predict(ols, validation)))
results <- results %>% bind_rows(results_temp)
# show results table in latex format
print(results_temp)



# Model 6 Movie Effect Model
  
Movie_Effect_Model <- function(lambda, return_prediction=FALSE){
  # the average rating
  mu <- mean(edx$rating)
  
  # calculate b_movie coefficients
  movie_effect <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_m = sum(rating - mu) / (n() + lambda))
  
  # add to validation
  validation <- validation %>% left_join(movie_effect, by='movieId')
  
  # rmse
  my_rmse <- RMSE(validation$rating, (mu + validation$b_m))
  
  if (return_prediction){
    return(list("rmse"       = my_rmse,
                "prediction" = mu + validation$b_m))
  }else{
    return(my_rmse)
  }
}

# calculate errors for a set of lambda values and choose the smallest rmse
lambdas <- seq(0, 10, 0.25)
model_rmses <- sapply(lambdas, Movie_Effect_Model)
lambda_of_smallest_rmse <- lambdas[which.min(model_rmses)]

# plot it
ggplot() + geom_line(aes(lambdas, model_rmses), col="#56B4E9", size=1) +
  geom_vline(xintercept=lambda_of_smallest_rmse, col="red") + ggtitle("Movie Effect Model: RMSE as function of lambda parameter") + ylab("RMSE") + xlab("lambda")

# add the best prediction
results_temp <- tibble("Model" = "movie_effect",
                       "RMSE"  = model_rmses[which.min(model_rmses)])
results <- results %>% bind_rows(results_temp)
# show results table in latex format
print(results_temp)





# Model 7 Movie User Effect Model



Movie_User_Effect_Model <- function(lambda, return_prediction=FALSE){
  # the average rating
  mu <- mean(edx$rating)
  
  # calculate b_movie coefficients
  movie_effect <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_m = sum(rating - mu) / (n() + lambda))
  
  # calculate b_user coefficients
  movie_user_effect <- edx %>% 
    left_join(movie_effect, by="movieId") %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_m) / (n() + lambda))
  
  # add to validation
  validation <- validation %>% 
    left_join(movie_effect, by='movieId') %>% 
    left_join(movie_user_effect, by='userId')
  
  # rmse
  my_rmse <- RMSE(validation$rating, (mu + validation$b_m + validation$b_u))
  
  if (return_prediction){
    return(list("rmse"       = my_rmse,
                "prediction" = mu + validation$b_m + validation$b_u))
  }else{
    return(my_rmse)
  }
}

# calculate errors for a set of lambda values and choose the smallest rmse
lambdas <- seq(0, 10, 0.25)
model_rmses <- sapply(lambdas, Movie_User_Effect_Model)
lambda_of_smallest_rmse <- lambdas[which.min(model_rmses)]

# plot it
ggplot() + geom_line(aes(lambdas, model_rmses), col="#56B4E9", size=1) +
  geom_vline(xintercept=lambda_of_smallest_rmse, col="red") + ggtitle("Movie Effect Model: RMSE as function of lambda parameter") + ylab("RMSE") + xlab("lambda")

# add the best prediction
results_temp <- tibble("Model" = "movie_user_effect",
                       "RMSE"  = model_rmses[which.min(model_rmses)])
results <- results %>% bind_rows(results_temp)
# show results table in latex format
print(results_temp)



# Conclusion

# The model that has the smallest RMSE is the Movie User Effect model

print(results)

# Predicted ratings

submission <- validation %>% select(userId, movieId, rating) %>% 
  mutate("prediction" = Movie_User_Effect_Model(lambda_of_smallest_rmse, return_prediction=TRUE)$prediction)
# write on the disk
write_csv(submission, "submission.csv")



