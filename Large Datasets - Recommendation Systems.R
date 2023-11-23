## Large Datasets
## 
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(caret)
library(dslabs)
library(matrixStats)
library(MASS)

if(!exists("mnist")){mnist = read_mnist()}

x <- mnist$train$images
y <- factor(mnist$train$labels)


# Plot an image for row i
i <- 54000
grid <- matrix(x[i,], 28, 28)
image(1:28,1:28,grid[,28:1])

# Row and Column Summaries - Assessing distribution of pixel darkness
# each column is a pixel and each row is an image

# MMNIST: Row Summaries ====
row.sums <- rowSums(x) # Sum pixel values for each image
row.means <- rowMeans(x) # mean pixel darkness for each image

# generate a boxplot
tibble(labels = as.factor(y),
       row.means = row.means) |> 
  ggplot(aes(labels, row.means)) +
  geom_boxplot(outlier.shape = 21,
               outlier.size = 2,
               outlier.color = "black",
               outlier.fill = "pink") +
  labs(title = "Distribution of mean pixel darkness",
       caption = "Data: mnist", 
       x = "Label") +
  theme_bw()

## box plot of pixel sd
row.sd = rowSds(x)
tibble(label = as.factor(y),
       sd = row.sd) |> 
  ggplot(aes(label, sd)) + 
  geom_boxplot(outlier.shape = 21,
               outlier.size = 2,
               outlier.color = "black",
               outlier.fill = "pink") +
  labs(title = "Distribution of row dispersion",
       caption = "Data: mnist", 
       x = "Label") +
  theme_bw()


## Filtering Columns Based on Summaries ====

## We can remove those pixels which exhibit little to no variation
col.sds <- colSds(x)
tibble(sd = col.sds) |> 
  ggplot(aes(sd)) +
  geom_histogram(color = "black", fill = "orange") +
  labs(title = "Distribution of Pixel Variation",
       caption = "Data: mnist") +
  theme_bw()

# visualize the distribution of sd's
# using image()
image(1:28, 1:28, matrix(col.sds, 28,28)[,28:1])

## Observation: pixels at the margins show little to no variance
## and can be safely omitted from the model

## to remove uninformative predictors we simply have to set a cutoff
## to determine the cutoff, we can go back to the distribution
## 
## For this example we'll set the cutoff to 60

new_x <- x[,colSds(x)>60]
dim(x); dim(new_x)

## this reduced our feature space from 784 to 322
## 
## Looking at the histogram allows us to determine a cut off for smudges in
## the pixels
histogram(as.vector(new_x))


## We see there's a clear dichotomy in pixel values, now. We could set the cutoff at 
## 50 and anything below that will get a pixel value of 0
new_x[new_x < 50] <- 0


## Now, we reinspect the histogram
histogram(as.vector(new_x))

# On the left are all the pixels we've set to 0 and on the right are what we hope are the 
# most informative numbers

## Based on the new histogram, we can employ binarization 
bin_x <- new_x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1

## or alternatively:
bin_x <- (x > 255/2)*1



## Movielens: Load Data ====
data("movielens")


## Movielens: Prep Data ====

## Using left_join + dplyr::count 
movies.df <- 
  movielens |>
  rename(release.year = year) |> 
  # Number of reviews each user submitted
  left_join(dplyr::count(movielens, userId), by = "userId") |> 
  rename(n.reviews.user = n) |> 
  # number of reviews each movie received
  left_join(dplyr::count(movielens, movieId), by = "movieId") |> 
  rename(n.reviews.movie = n) |> 
  # Convert timestamp to datetime object
  #   and extract the year of the review
  mutate(timestamp   = as.POSIXct(timestamp, origin = "1970-01-01"),
         review.year = lubridate::year(timestamp),
         review.week = lubridate::week(timestamp),
         review.day  = lubridate::wday(timestamp, label = TRUE))

## Using add_count
movies.df <- 
  movielens |> 
  # convert time stamp and extract year
  mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
         review.year = lubridate::year(timestamp)) |> 
  select(-c(genres, timestamp)) |> 
  ## number of reviews each movie received 
  add_count(movieId, name = "n.reviews.movie") |>
  ## number of reviews each user submitted
  add_count(userId, name  = "n.reviews.user")  |> 
  # Convert timestamp to datetime object
  #   and extract the year of the review
  mutate(timestamp   = as.POSIXct(timestamp, origin = "1970-01-01"),
         review.year = lubridate::year(timestamp),
         review.week = lubridate::week(timestamp),
         review.day  = lubridate::wday(timestamp, label = TRUE))


## Using Nest and Unnest
movies.df <-   
  movielens |> 
  # convert time stamp and extract year
  mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
         review.year = lubridate::year(timestamp)) |> 
  # Get number of reviews for each movie
  nest(data = -c(movieId)) |>
  mutate(n.reviews.movie = sapply(data, nrow)) |>
  unnest(cols = data) |> 
  # get number of reviews by each user
  nest(data = -c(userId)) |>
  mutate(n.reviews.user = sapply(data, nrow)) |>
  unnest(cols = data) |> 
  # Convert timestamp to datetime object
  #   and extract the year of the review
  mutate(timestamp   = as.POSIXct(timestamp, origin = "1970-01-01"),
         review.year = lubridate::year(timestamp),
         review.week = lubridate::week(timestamp),
         review.day  = lubridate::wday(timestamp, label = TRUE))


## Movielens: Distribution Plots ====

## Dist. Plot: Users vs. Movie ====
# sample 100 random users and 100 random movies to visualize the density of 
# ratings

set.seed(4242)
users   <- sample(unique(movies.df$movieId), size = 100, replace = F)
movies  <- sample(unique(movies.df$userId) , size = 100, replace = F)


movies.df |> 
  filter(userId %in% users) |> 
  filter(movieId %in% movies) |>
  ggplot(aes(factor(movieId), factor(userId))) + 
  geom_point(shape = 21, 
             size = 3, 
             fill = "#FFC107") +
  labs(title = "Users vs. Movie - Did they Review?",
       subtitle = "Sample of 100 users and 100 movies",
       caption = "Data: Movielens",
       x = "\nMovie ID", y = "User ID") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = .5))


## Distribution of n.reviews.Movie ====

movies.df |> 
  ggplot(aes(n.reviews.movie, after_stat(density))) + 
  geom_histogram(color = "black", 
                 fill = "orange",
                 bins = 40) +
  geom_density(kernel = "cosine", linewidth = 1.1, color = "#1E88E5")+
  labs(title = "Reviews per Movie",
       subtitle = "Distribution Plot",
       caption = "Data: Movielens",
       x = "\nNum. Reviews", y = "") +
  theme_bw()



## Distribution of n.reviews.user
## 
## Observation: The vast majority of users have reviewed very few movies

movies.df |> 
  ggplot(aes(n.reviews.user, after_stat(density))) +
  geom_histogram(color = "black", 
                 fill = "orange", bins = 30) +
  geom_density(kernel = "gaussian", 
               linewidth = 1.1, 
               color ="#1E88E5" ) +
  labs(title = "Reviews Per User",
       subtitle = "Distribution",
       caption = "Data: Movielens", x="Num Reviews", y = "")

### Recommendation Systems - Machine Learning Algo ====
### 

### Only consider movies rated five or more times and users that rated more than 100 of these movies

movies <- 
  movies.df |> 
  filter(n.reviews.movie >=100 & n.reviews.user >=5)


## Movielens: Model Building ====
# first, write a helper function to calculate RMSE
RMSE <- function(true_rating, predicted_rating){
  sqrt((mean((predicted_rating - true_rating)^2)))
}

# Modelling Movie Effect
# 
# Y_{u,i}= \mu + b_i +\epsilon_{u,i}
#     b_i = average ranking of movie i

# calculate mu
mu <- mean(movies$rating, na.rm = T)


## Movielens: Movie Effect ====
# Calculate the movie effect:
#   - we could model using lm(rating ~ factor(movieID)) but that would
#     take forever
#   - Instead, we know that the movie effect, is just the average of the difference
#     between the movie rating and the avg rating of all movies
#     
#   - Calculation: 
#       b.i = mean(y.ui - mu)


## partition the data 80/20
set.seed(4242)
test.idx <- createDataPartition(movies$rating, p =.2, list = F)
test_set <- movies.df[test.idx, ]
train_set <- movies.df[-test.idx, ]

## ensure that both sets have the same userId's and movieId's
train_set <- semi_join(train_set, test_set, by = "userId")
test_set <-  semi_join(test_set, train_set, by = "userId")

mu <- mean(train_set$rating, na.rm = TRUE)

train.wide <- 
  train_set |> 
  select(userId, movieId, rating) |> 
  pivot_wider(names_from = movieId, values_from = rating)

y <- train.wide[,-1]

b.i <- colMeans(y-mu, na.rm=T)


fit.movies <- data.frame(movieId = as.integer(colnames(y)), 
                         mu = mu, b.i = b.i)
movie.effect.df <- 
  test_set |> 
  left_join(fit.movies, by = "movieId") |> 
  mutate(pred = mu+b.i) |> 
  select(movieId, userId, b.i, mu, rating, pred)

movie.effect.df |> 
  summarize(rmse = RMSE(pred, rating))

## Movielens: User Effect ====

b.u <- rowMeans(sweep(y-mu, 2, b.i), na.rm = T)


fit.users <- data.frame(userId = train.wide$userId,
                        b.u = b.u)

test_set |> 
  left_join(fit.movies, by = "movieId") |> 
  left_join(fit.users, by = "userId") |>
  mutate(pred = mu + b.i + b.u) |> 
  summarize(rmse = RMSE(pred, rating))


qplot(b.u, bins = 30, color = I("black"))

## Can the model be improved by taking it to be the combination of 
## rating, user effect, and movie effect? 
## 
## Y_i = \mu + b_{i} + b_{u} + \epsilon_{u,i}
## 
## Approximate b_u as the average difference between y_{u,i} and (\hat{\mu} - \hat{b_i}
## 

## take the rowMeans of Y after removing mu and b.i
##  - sweep => sweep out/remove a value from a matrix.
b.u <- rowMeans(sweep(y - mu, 2, b.i), na.rm = T)


# make predictions
fit_users <- data.frame(userId = as.integer(rownames(y)), 
                        b_u = b.u)

test_set |> 
  left_join(fit_movies, by = "movieId") |> 
  left_join(fit_users, by = "userId") |> 
  mutate(pred = mu + b_i + b_u) |> 
  summarize(rmse = RMSE(rating, pred))

### Ch. 33.8 Exercises ====

## 33.8 Q1 - Ratings/Year ====
# Compute the number of ratings and plot against release year. Use sqt. transform on count

str(movielens)

ratings.by.year <- 
  group_by(year) |>
  summarize(n = sum(n.reviews.movie))


## Movielens: Plot - Ratings vs. Year ====
movies |> 
  group_by(year) |> 
  summarize(n = sum(n.reviews.movie)) |> 
  ggplot(aes(year, sqrt(n))) + geom_area(aes(fill = n), fill = "#005AB5", alpha = 0.7) +
  geom_line(na.rm =T) +
  geom_point(data = data.frame(year = 1994, n = 848),
             aes(year, n), 
             color = "#D41159", 
             size = 3, 
             shape = 18) +
  geom_label(data = data.frame(year = 1994, n = 848),
             aes(label = "1994: 848^2 Ratings"),
             x = 1983,y=845) +
  geom_segment(aes(x = 1994, xend = 1994,
                   y = 0, yend = 848),
               linetype = 3,
               linewidth = 1.5, 
               color = "black",
               fill = "#D41159") +
  labs(title = "Movielens: Ratings/year",
       x = "Year", 
       y = latex2exp::TeX("$\\sqrt{n}$")) +
  theme_fivethirtyeight() + 
  theme(axis.title.y = element_text(size = 15, angle = 0,
                                    vjust = 0.5))

## Exercises 33.8 Q2 ====
## Among movies that came out in 1993 & after, which movies had the most ratings?
## Report their average rating

## using movies made with mutate + dplyr::count(c(userId, movieId, year))
movies |> 
  filter(year >= 1993) |> 
  left_join(movies |> 
              group_by(movieId) |> 
              summarize(avg.rating = mean(rating)),
            by = "movieId") |> 
  select(title, n.reviews.movie, avg.rating) |> 
  rename(n.reviews = n.reviews.movie) |> 
  distinct() |> 
  mutate(avg.rating = avg.rating - mean(movies$rating)) |>
  arrange(desc(n.reviews)) |> 
  slice(1:25)

top_25 <- 
  movies |> 
  # select movies that came out after 1993
  filter(year >= 1993) |> 
  select(title, movieId, year, n_ratings.movie) |> 
  # arrange by number of ratings
  arrange(desc(n_ratings.movie)) |> 
  # select distinct movies
  distinct() |> 
  # grab the top 25
  top_n(25) |> 
  # add the mean rating of the movie
  left_join(
    movies |> 
      group_by(movieId) |> 
      # AvgRating = over/under the overall mean rating
      summarize(AvgRating = mean(rating)-mu),
    by = "movieId"
  )

## Exercises 33.8 Q3 ====
## Stratify post 1993 movies by number of ratings/year & compute their average ratings
## make a plot of avg.rating vs. ratings per year, & show an estimate of the trend

## Movielens: Plot: Avg. Rating vs Reviews/Year ====
test_set |> 
  # select movies that came out after 1993
  filter(release.year >= 1993) |> 
  # get average ratings & join back
  left_join(train_set |> 
              group_by(movieId) |> 
              summarize(avg.rating = mean(rating)),
            by = "movieId") |>
  # calculate ratings per year
  mutate(ratings.py = n.reviews.movie/(2018-release.year)) |> 
  select(movieId, title, ratings.py, avg.rating) |> 
  distinct() |> 
  ggplot(aes(ratings.py, avg.rating)) + 
  geom_point(shape = 21, size = 3, fill = "orange") + 
  geom_smooth(method = "lm") +
  geom_hline(yintercept = mu) +
  annotate("text", x = 13, y = mu-.10, 
           label = "Avg. Rating\nAll Movies") +
  labs(title = "Avg. Rating vs. Reviews/Year",
       caption = "Data: movielense",
       x = "\nReviews per Year",
       y = "Average(rating)") +
  theme_bw()

## Exercises 33.8 Q6 ====
## 

## Compute the average rating for each week and plot this average against day

movies.df |> 
  group_by(week.rounded) |> 
  summarize(avg.rating = mean(rating)) |> 
  right_join(movies.df,
             by = "week.rounded") |> 
  mutate(review.day = factor(review.day)) |> 
  ggplot(aes(review.year, avg.rating, color = review.day)) +
  geom_boxplot(outlier.shape = 21,
               outlier.color = "black",
               outlier.fill = "orange",
               outlier.size = 1.1) +
  scale_color_discrete() +
  labs(title = "Dist. of avg. weekly ratings by day",
       caption = "Data: Movielens",
       x = "Day", 
       y = "Avg. Rating") +
  theme_bw()


## Is there a time effect?
movies.df |> 
  group_by(week.rounded) |> 
  summarize(avg.rating = mean(rating)) |> 
  right_join(movies.df,
             by = "week.rounded") |>
  ggplot(aes(lubridate::yday(timestamp), avg.rating)) + 
  geom_smooth(method = "lm")


## Exercises 33.8 Q8 ====
## Genres has multiple genres that are applicable to each movie
## Define a category as whatever combination appears in this column
## Keep only categories with more than 1000 ratings
## then compute the average and std. error for each category
## plot these as error bar plots
## 
## 

movies.df |> 
  left_join(dplyr::count(movies.df, genres), by = "genres") |> 
  rename(n.genres = n) |> 
  filter(n.genres >= 1000) |> 
  group_by(genres) |> 
  summarize()


## Comprehension Check ====

## Compute n.ratings for each movie and make a boxplot for each year
movielens |> 
  # number of reviews for each movie
  add_count(movieId, name = "n") |> 
  # factorize year
  mutate(year = factor(year)) |> 
  ggplot(aes(year, sqrt(n))) + 
  geom_boxplot() +
  theme_economist() +
  labs(title = "Distribution of Number of Movie Reviews") +
  theme(axis.text.x = element_text(angle = 90)) 

movielens |>
  # movies from 1993 and up
  filter(year >= 1993) |> 
  # total movie reviews
  nest(data = -c(movieId)) |> 
  mutate(n = sapply(data, \(x) nrow(x))) |> head()


top_25 <- 
  movielens |>
  # movies from 1993 and up
  filter(year >= 1993) |> 
  # total movie reviews
  group_by(movieId) |> 
  summarize(n = n()) |> 
  right_join(movielens |>
               filter(year >= 1993) |> 
               select(movieId, year), 
             by = "movieId") |> 
  mutate(n_py = n/(2018-year)) |>  
  left_join(movies |> 
              filter(year >= 1993) |> 
              select(movieId, title) |> 
              distinct(), 
            by = "movieId") |>
  distinct() |> 
  left_join(movielens |> 
              filter(year >= 1993) |> 
              nest(data = -c(movieId)) |> 
              mutate(AvgRating = sapply(data, \(x) mean(x$rating))) |> 
              unnest(data) |> 
              group_by(movieId) |> 
              summarize(AvgRating = mean(AvgRating)),
            by = "movieId") |>
  select(movieId, title, n_py, AvgRating) |> 
  arrange(desc(n_py)) |> 
  slice(1:25) 


# Plot average rating against reviews per year

rating <- movielens |> summarize(rating = mean(rating))
print(rating)
top_25 |> 
  ggplot(aes(n_py, AvgRating)) + 
  geom_point() +
  geom_hline(yintercept = rating[[1]]) +
  geom_smooth(method = "lm") +
  labs(title = "Average Movie Rating vs. Reviews Per year",
       x = "Reviews Per Year", 
       y = "Average Rating") +
  theme_economist()

movies |>
  group_by(movieId) |> 
  summarize(tot.reviews = n(),
            avg.reviews = mean(rating)) |> 
  arrange(desc(tot.reviews)) |> 
  right_join(movies, by = "movieId") |>
  filter(year >= 1993 & !is.na(year)) |>
  select(year, title, tot.reviews, avg.reviews) |> 
  distinct() |> 
  mutate(ratings.py = tot.reviews/(2018-year)) |> 
  arrange(desc(avg.reviews)) |> head()
ggplot(aes(ratings.py, avg.reviews)) +
  geom_point(color = "#D41159", fill = "black")


## Movielens Genres ====
## Movielens has a genres column. Define a category as whatever combination appears in genres
## Only keep the categories with more than 1000 reviews. Compute the avg and std. dev. for each
## category
data("movielens")

genres.counted <- 
movielens |> 
  mutate(timestamp = lubridate::as_datetime(timestamp)) |>
  left_join(dplyr::count(movielens, genres), by = 'genres') |> 
  rename(genre.count = n) |> 
  filter(genre.count >= 1000) |> 
  group_by(genres) |> 
    summarize(mean.rating = mean(rating),
              sd.rating = sd(rating))
  
genres.counted |> 
  mutate(genres = reorder(genres, mean.rating, decreasing = F)) |> 
  ggplot(aes(genres, mean.rating)) +
  ## point for mean rating
  geom_point() +
  ## error bars
  geom_errorbar(aes(ymin = mean.rating - sd.rating,
                    ymax = mean.rating + sd.rating)) +
  labs(title = "Mean Rating by Distribution",
       subtitle = "geom_pointrange",
       caption = "Data: Movie Lens") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90),
        plot.caption.position = "plot")

arrange(genres.counted, mean.rating)

mean(movielens$rating)
## Matrix Algebra Operations:

# Matrix multiplication operator: %*%
# Cross product:
#   t(x) %*% x
#   crossprod(x)

## To Compute the inverse: solve
## To get the QR composition: qr(x)

## Exercises

x <- matrix(rnorm(100*10), 100, 10)

sweep(x, 1, 1:100, "+") |> head()
sweep(x, 2, 1:10, "+") |> head()
colSds(x)
rowSds(x)


## For each digit in MNIST train, compute proportion of pixels in the grey area
## (50-205) then make a boxplot for each class

grey_area <- matrix(apply(x, 2, \(x) mean(x >=50 & x <= 205)), 28,28)

grey_area.rows <- apply(x, 1, \(x) mean(x >=50 & x <= 205))

tibble(labels = y,
       prop.grey = grey_area.rows) |> 
  ggplot(aes(labels, prop.grey)) +
  geom_boxplot(outlier.shape = 21,
               outlier.size = 2,
               outlier.color = "black",
               outlier.fill = "pink") +
  labs(title = "Proportion of pixels between 50 and 205",
       caption = "Data: MNIST",
       x = "Class",
       y = "%'age grey") +
  theme_bw()

