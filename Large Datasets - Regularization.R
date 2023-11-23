
## Large Datasets - Regularization ====
## 

### Regularization ====

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

# to model the movie and user effects, we first need the overall average
# in order to determine their individual offsets.
overall = mean(movies.df$rating, na.rm = T)


## To determine what the user effect is, we would be looking at the row to row differences
## whereas for the movie effect we're looking at the column to column differences
## 
## In the simple 1 variable model, you have y.ui = mu + b.i + e
##    b.i.hat = y.ui - mu
## 
## In the 2 variable model, you have y.ui = mu + b.i + b.u + e
##    b.u.hat = y.ui - mu - b.i
##    
## First determine the b.i's  


set.seed(4242)
test.idx <- caret::createDataPartition(movies.df$rating, p =.2, list = F)
test_set <- movies.df[test.idx, ]
train_set <- movies.df[-test.idx, ]

## ensure that both sets have the same userId's and movieId's
train_set <- semi_join(train_set, test_set, by = "userId")
test_set <-  semi_join(test_set, train_set, by = "userId")
train_set <- semi_join(train_set, test_set, by = "movieId")
test_set <-  semi_join(test_set, train_set, by = "movieId")

mu <- mean(train_set$rating, na.rm = TRUE)

train.wide <- 
  train_set |> 
  select(userId, movieId, rating) |> 
  pivot_wider(names_from = movieId, values_from = rating)

y <- train.wide[,-1]

b.i <- colMeans(y-mu, na.rm=T)


fit.movies <- data.frame(movieId = as.integer(colnames(y)), 
                         mu = mu, b.i = b.i)
movie.effect.df <- left_join(test_set, fit.movies, by = "movieId")


# +++++++++++++++++++++++++++++ 
# Calculate User Effect:      
# +++++++++++++++++++++++++++++
b.u <- rowMeans(sweep(y-mu, 2, b.i), na.rm = T)


fit.users <- data.frame(userId = train.wide$userId,
                        b.u = b.u)

movie.effect.df <-
  movie.effect.df |>
  left_join(fit.users, by = "userId") |>
  mutate(preds = mu + b.i + b.u)


movie.effect.df |> 
  summarize(rmse = mean((preds - rating)^2))  ## Yields RMSE of 0.8245788


# ++++++++++++++++++++++++++++++++++++++++++++++
# Apply Regularization to improve our estimates
# ++++++++++++++++++++++++++++++++++++++++++++++

## Rank movies based on our predictions

movie.effect.df |>
  group_by(movieId) |>
  summarize(n = n(),
            avg.rating = mean(rating)) |>
  slice_max(avg.rating, n = 10)

## The top 3 movies with 5 star ratings only have 1
## rating. We can improve our model, by including an 
## penalty in our objective function. 
## 
## In our case, the penalty lambda will be an offset
## for the number of ratings the movie received

# start with lambda = 3

b.i.reg <- 
  movie.effect.df |>
  nest(data = -c(movieId))|>
  mutate(b.i.reg = sapply(data, \(df){
    sum(df$rating - mu)/(nrow(df) + 3)
  })) |> dplyr::select(movieId,b.i.reg)

movie.effect.df <-
  movie.effect.df |>
  left_join(b.i.reg, by = "movieId") |>
  mutate(preds.reg.1 = mu + b.i.reg + b.u)

## calculate the rmse
movie.effect.df |>
  summarize(rmse = sqrt(sum((preds.reg.1-rating)^2)/nrow(movie.effect.df))) ## .822707

n.ratings <- 
  dplyr::select(movie.effect.df, movieId, n.reviews.movie) |> 
  distinct()

n <- colSums(!is.na(y))
fit.movies$n = n

fit.movies <- 
  fit.movies |>
  mutate(avg.rating = mu + b.i)

## Now test for a range of lambdas
lambdas <- seq(0, 10, .1)

sums <- colSums(y - mu, na.rm = T)

rmses = sapply(lambdas, function(lambda){
  
  b_i = sums / (n + lambda)
  fit.movies$bi2 <- b_i
  movie.effect.df |>
    left_join(fit.movies |> 
                select(movieId, bi2), 
              by = "movieId") |>
    mutate(preds.reg.2 = mu + bi2 + b.u) |>
    summarize(rmse = sqrt(sum((preds.reg.2-rating)^2)/nrow(movie.effect.df))) |>
    pull(rmse)
})

rmse.df <- tibble(lambda = lambdas, rmse = rmses)

lambda.hat <- lambdas[which.min(rmse.df$rmse)]

head(movie.effect.df)

tmp <- 
  movie.effect.df |>
  nest(data = -c(movieId)) |>
  mutate(bi.reg = map(data, \(x){
    sum(x$rating - mu)/(nrow(x)+lambda.hat)
  }) |> unlist() ) |>
  unnest(data) |> 
  select(movieId, userId, rating, mu, b.u, bi.reg) |>
  left_join(fit.movies |> select(movieId, b.i), 
            by = "movieId") |>
  mutate(orig.preds = mu + b.i + b.u,
         new.preds  = mu + bi.reg + b.u)
write_csv(tmp, "movie_regulized.csv")

tmp |>
  summarize(rmse.new = mean((new.preds-rating)^2),
            rmse.old = mean((orig.preds-rating)^2))


## Exercise: 33.10 ==== 


## An education expert advocates for smaller schools based on the fact that above the 
## best performing schools, many are small. 
## 
## Simulate a dataset for 100 schools 

# Simulate number of students at each school

set.seed(1986)
sizes <- round(2^rnorm(1000, 8, 1))

## Assign a true quality to the school, independent of size
set.seed(1)
quality <- round(80 + 2*rt(1000,5))

## Simulate a test
## Test results are random variables, so we'll model 
## them as normal with the average determined
## by school quality with a sd of 30 points

scores <- 
  sapply(1:nrow(schools), \(i){
    scores = rnorm(schools$size[i], schools$quality[i], 30)
    scores
  })

schools <- 
  tibble(
    id           = paste("PS", 1:1000),
    size         = sizes,
    quality      = quality,
    rank.quality = rank(-quality),
    log.size     = log(size,2),
    small        = ifelse(log.size < median(log.size),1,0),
    score        = sapply(scores,mean),
    rank.score   = rank(-score)
  )
## Top 10 schools by quality
## 
top.10.quality <- 
  schools |> 
  slice_max(quality, n = 10) |>
  arrange(desc(quality))

## Top 10 Schools by score
## 
top.10.score <-
  schools |>
  slice_max(score, n = 10)|>
  arrange(desc(score))

## worst 10 Schools based on score
## 
bottom.10.scores <-
  schools |>
  slice_min(score, n = 10)|>
  arrange(desc(score))

# Compare the overall median school size to median school size for the:
#   - top 10 and
#   - bottom 10 schools based on score

medians.df <- tibble(overall   = median(schools$size),
                     top_10    = median(top.10.score$size),
                     bottom_10 = median(bottom.10.scores$size))
knitr::kable(medians.df)

glue::glue("Median Size - Top 10")

## To inspect what's happening, plot the average score against log.size
## Highlighting the best schools by quality

## Set labels for each quadrant: 
quadrant.labels <- expand.grid(school = c("Small ", "Big "),
                               score = c("Low ", "High ")) |> 
  mutate(label = paste(school, "School","\n",score,"Score",sep =""),
         hjust = ifelse(school == "Small", 0.85, .85),
         vjust = ifelse(score == "Low", 0.85, .85)) 
quadrant.labels

# +++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Plot size vs.score - Top 10 by quality in red ====
# +++++++++++++++++++++++++++++++++++++++++++++++++++ #
schools |> 
  dplyr::select(id, log.size, score) |> 
  ggplot(aes(x = log.size,
             y = score)) +
  geom_point(shape = 21, 
             color = "black",
             fill = cool.grey.80,
             alpha = 0.8,
             size = 2.2) +
  ## Divide the canvas into quadrants based on median
  ## 
  geom_vline(aes(xintercept = median(log.size)),
             linewidth = 1.75, 
             color = "tomato", 
             alpha = 0.50) +
  ## Median School Size
  ## 
  geom_hline(aes(yintercept = median(score)),
             linewidth = 1.75, 
             color = "tomato", 
             alpha = .50) + 
  ## Median Test Score
  ## 
  ## Add labels to the quadrants 
  ## 
  geom_label(data = quadrant.labels, 
             aes(x = c(4.25,11.5,4.25,11.5), 
                 y = c(65,65,92,92), 
                 label = label, 
                 hjust = c(0.20, .80, 0.20 ,.80), 
                 vjust = c(.05, .05, .05, .05))) +
  ## Color in the top 10 schools
  ## 
  geom_point(data = top.10.quality, 
             aes(log.size, score), 
             shape = 21,
             stroke = 2, 
             fill = "red", 
             size = 3.5 ) +
  annotate("text", x = 4.25, y = median(schools$score), 
           label = "Median\nTest Avg.",
           color = "grey10", size = 4, 
           alpha = .75) +
  annotate("text", x = 8.1, y = 65, 
           label = "Median\nSchool Size",
           hjust = 0, vjust = 0,
           color = "grey10", size = 4, 
           alpha = .75) +
  ## Set theme options
  ## 
  labs(title = "Avg. Test Score vs. School Size",
       subtitle = "Top 10 schools by quality highlighted in red",
       caption = "Data: Simulated Schools Data",
       x = "Log School Size",
       y = "Avg. Test Score") + 
  theme_fivethirtyeight() +
  theme(plot.caption.position = "plot")

## observation: Quality is evenly distributed
## analytic verification:

mean(schools$size <= median(schools$size) & schools$score > median(schools$quality))
mean(schools$size <= median(schools$size) & schools$score < median(schools$quality))
mean(schools$size > median(schools$size) & schools$score > median(schools$quality))
mean(schools$size > median(schools$size) & schools$score < median(schools$quality))

# +++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Plot: Size vs. Score - top 10 by score in red ==== 
# +++++++++++++++++++++++++++++++++++++++++++++++++++ #
schools |> 
  dplyr::select(id, log.size, score) |> 
  ggplot(aes(x = log.size,
             y = score)) +
  geom_point(shape = 21, 
             color = "black",
             fill = cool.grey.80,
             alpha = 0.8,
             size = 2.2) +
  ## Divide the canvas into quadrants based on median
  ## 
  geom_vline(aes(xintercept = median(log.size)),
             linewidth = 1.75, 
             color = "tomato", 
             alpha = 0.50) +
  ## Median School Size
  ## 
  geom_hline(aes(yintercept = median(score)),
             linewidth = 1.75, 
             color = "tomato", 
             alpha = .50) + 
  ## Median Test Score
  ## 
  ## Add labels to the quadrants created by the
  ## lines above
  ## 
  geom_label(data = quadrant.labels, 
             aes(x = c(4.25,11.5,4.25,11.5), 
                 y = c(65,65,92,92), 
                 label = label, 
                 hjust = c(0.20, .80, 0.20 ,.80), 
                 vjust = c(.05, .05, .05, .05))) +
  ## Color in the top 10 schools
  ## 
  geom_point(data = top.10.score, 
             aes(log.size, score), 
             shape = 21,
             stroke = 2, 
             fill = "red", 
             size = 3.5 ) +
  annotate("text", x = 4.25, y = median(schools$score), 
           label = "Median\nTest Avg.",
           color = "grey10", size = 4, 
           alpha = .75) +
  annotate("text", x = 8.1, y = 65, 
           label = "Median\nSchool Size",
           hjust = 0, vjust = 0,
           color = "grey10", size = 4, 
           alpha = .75) +
  ## Set theme options
  ## 
  labs(title = "Avg. Test Score vs. School Size",
       subtitle = "Top 10 schools by score highlighted in red",
       caption = "Data: Simulated Schools Data",
       x = "Log School Size",
       y = "Avg. Test Score") + 
  theme_fivethirtyeight() +
  theme(plot.caption.position = "plot")

# ++++++++++++++++++++++++++++++++++++++++
# Observation:
#   All but 1 of the top 10 schools by
#   score are small schools. 
#   
#   We need to adjust for school size via
#   regularization
# ++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++
## Applying regularization ====
##    1. Calculate the mean deviation from
##    the overall average for each school, 
##    dividing by n+lambda instead of n alone. 
# ++++++++++++++++++++++++++++++++++++++++
lambdas = seq(10,250)
overall <- mean(sapply(scores, mean))

# Calculate the mean offset for each lambda
b.i <-
  sapply(1:length(lambdas), \(i){
    sapply(1:nrow(schools), \(j){
      sum((scores[[j]]- overall))/(length(scores[[j]]) + lambdas[i])
    })
  })

# Calculate the rmse for each lambda
rmse <-
  apply(b.i, 2, \(x){
    y = schools$quality
    x = x + overall
    rmse = sqrt(sum((x-y)^2)/1000)
  })
rmse.df <- tibble(lambda = lambdas, rmse = rmse)

lambda.hat <-rmse.df[which.min(rmse.df$rmse),1] |> pull(lambda)

rmse.df |> ggplot(aes(lambda, rmse)) + 
  # cross-hairs of min rmse
  geom_hline(yintercept = rmse.df$rmse[which.min(rmse.df$rmse)], col = green.40) + 
  geom_vline(xintercept = rmse.df$lambda[which.min(rmse.df$rmse)], col = green.40) +
  geom_line() +
  labs(title = "RMSE as a function of Lambda", subtitle = "Regularization Applied to Centered Scores")

## best b.i
b.i.best <- b.i[, which.min(rmse.df$rmse)]

schools <-
  schools |>
  mutate(b_i = 
           sapply(scores, \(x){
             sum( (x-overall) )/(length(x) + lambda.hat)
           }),
         preds = b_i + overall,
         rank.preds = rank(-preds))

top.10.preds <-
  schools |>
  slice_min(rank.preds, n = 10) |>
  arrange(rank.preds)


schools |>
  dplyr::select(id, log.size, preds) |> 
  ggplot(aes(x = log.size,
             y = preds)) +
  geom_point(shape = 21, 
             fill = "gray15",
             alpha = 0.8,
             size = 2.2)  +
  ## Highlight the top 10 best by preds
  geom_point(data = top.10.preds,
             aes(log.size, preds),
             shape = 21,
             fill = "tomato",
             alpha = 0.8,
             size = 2.2) +
  ## Set theme options
  ## 
  labs(title = "School Size vs. Predicted Quality",
       subtitle = "Top 10 by predicted score in red",
       caption = "Data: Simulated Schools Data",
       x = "Log School Size",
       y = "Score.hat") + 
  theme_fivethirtyeight() +
  theme(plot.caption.position = "plot")



## Suppose we didn't de-center the test scores. Which lambda leads to minimum mse then?
## 

mean.pred <- 
  sapply(1:length(lambdas),\(i){
    sapply(1:1000, \(j){
      sum(scores[[j]])/(length(scores[[j]]) + lambdas[i])
    })
  })

rmse1 <- apply(mean.pred, 2, \(x){sqrt(sum((x-schools$quality)^2)/1000)})

tibble(lambda = lambdas,
       rmse = rmse1) |> ggplot(aes(lambda, rmse)) + geom_line()

### Helper Functions ====  
## Helper Function for generating rgb colors
##
make.rgb = function(r,g,b, rgb = F){
  if(rgb == T){rgb(r/255, g/255, b/255)}
  else{c(r,b,g)/255}
}
make.rbg(10,200,55)
make.rbg(10,200,55, rbg=T)
class(make.rbg(10,200,55, rbg=T))



## Helper Functions: Color ====
## IBM Colors -- todo: create a color library of ibm colors
##
## 
teal.80 = make.rgb(0,65,68, rbg = T)  
cyan.60 = make.rgb(0, 114, 195, rbg = T)
## Blues
##
##
cool.grey.80 = make.rgb(52,58,63,T)
## Greys
## 
##
green.40 = make.rgb(66, 190, 101, T)
green.80 = make.rgb(4, 67, 23, T)
green.50 = make.rgb(36, 161, 72, T)
## Greens
## 
## 
red.70 = make.rgb(162,25,31, T)
red.50 = make.rgb(250,131,137, T)
## Reds
## 
##
## Colors from some website
##
custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", 
                "#4E84C4", "#293352")

