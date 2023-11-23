
## Large Data sets - Matrix Factorization

library(tidyverse)
library(ggthemes)
library(dslabs)

## Related to Factor Analysis, PCA, and SVD
## 
## Using the movies dataset as a model we have:
## 
##    Y.ui = mu + b.i + b.u + e.ui
##    
##  The current model doesn't account for ratings patterns
##  that occur within groups of movies and groups of users.
##  
##  To uncover the patterns, we inspect the residuals:
##  
##      r.ui = y.ui - b.i.hat - b.hat.u
##  If the model sufficiently describes the underlying data
##  generating process, we would expect the residuals to be
##  randomly spread around 0. 
##  
##  If that's not the case, and a pattern is evident in the
##  graph of the residuals, then there is a source of variation
##  which you haven't accounted for in the model. 
##  
##  Matrix factorization let's us decompose a matrix so that
##  you identify the factors that account for the greatest 
##  amount of variation
 

## Movielens: Load Data ====
data("movielens")

## Movielens: Prep Data ====
movies.df <- 
  movielens |>
    filter(!is.na(movieId) | !is.na(title)) |>
    rename(release.year = year) |> 
    mutate(timestamp   = as.POSIXct(timestamp, origin = "1970-01-01"),
           review.year = lubridate::year(timestamp),
           review.week = lubridate::week(timestamp),
           review.day  = lubridate::wday(timestamp, label = TRUE))


# calculate the residuals of the model that was just built in regularization
# regularized <- read_csv("movie_regulized.csv")

## Select data for which the number of reviews > 49 & for users who have more than 250 reviews

# Define movies to keep
keep <- c("Godfather, The", "Godfather: Part II, The", "Goodfellas", "Ghost", "Titanic", 
          "Scent of a Woman")

data <- 
  movies.df |>
  group_by(userId)  |> filter(n() >= 250) |> ungroup() |>
  group_by(movieId) |> filter(n() >= 50 | title %in% keep) |>
  ungroup()

## cols = movies, rows = users
y <- 
  data |> 
  select(userId, movieId, rating) |>
  pivot_wider(names_from = movieId, values_from = rating) |>
  select(-userId)

colnames(y) <- 
  data |> select(movieId, title) |>
  distinct(movieId, .keep_all = T) |> 
  right_join(tibble(movieId = as.integer(colnames(y))), by = 'movieId') |>
  pull(title)

# remove the movie effect  
r <- sweep(y, 2, colMeans(y, na.rm = T))
# if there isn't a rating, assume it won't 
# be a good one
r[is.na(r)] <- -1
# remove the user effects
r <- r - rowMeans(r)

# Conduct a PCA
pca = prcomp(r)
dim(pca$x)
dim(pca$rotation)

## How does sdev with the 
## number of principle components
data.frame(x = 1:ncol(pca$x), 
           y = pca$sdev) |> 
  ggplot(aes(x,y)) + geom_point()


## Plot the first 2 principal components against each other
rotation <- 
  data.frame(pca$rotation) |> 
  mutate(title = rownames(pca$rotation)) |>
  select(title, PC1, PC2)

## What are the top 10 movies in each direction for PC1 and PC2. What does that tell us?
top.PC1 <- rotation |> slice_max(PC1, n = 10) 
bot.PC1 <- rotation |> slice_min(PC1, n = 10)
top.PC2 <- rotation |> slice_max(PC2, n = 10) 
bot.PC2 <- rotation |> slice_min(PC2, n = 10)

rotation |>
  ggplot(aes(PC1, PC2)) + 
  geom_point(shape = 21, col = "black", fill = cool.grey.80, size = 1.5) +
  geom_text(data = top.PC1, aes(x = PC1+0.005, y = PC2+0.005, label = title)) +
  geom_text(data = bot.PC1, aes(x = PC1+0.005, y = PC2+0.005, label = title)) +
  labs(title = "PC1 vs PC2",
       subtitle = "Labels represent range of top/bottom 10 movies for PC1",
       caption = "Data: Movielens") +
  theme_bw()

rotation |>
  ggplot(aes(PC1, PC2)) + geom_point() +
  geom_text(data = top.PC2, aes(x = PC1+0.005, y = PC2-0.005, label = title)) +
  geom_text(data = bot.PC2, aes(x = PC1+0.005, y = PC2+0.05, label = title)) +
  labs(title = "PC1 vs PC2",
       subtitle = "Labels represent range of top/bottom 10 movies for PC2",
       caption = "Data: Movielens") +
  theme_bw()
  

## Exercises: Student Data ====
## 
## We'll be exploring SVD in the following exercises using simulated data
## for 100 students in 24 different subjects

set.seed(1987)
n <- 100
k <- 8

Sigma = 64 * matrix(c(1.00, 0.75, 0.50, 
                      0.75 ,1.00, 0.50, 
                      0.50, 0.50, 1.00), 3, 3)
m <- MASS::mvrnorm(n, rep(0,3), Sigma)
m <- m[order(rowMeans(m), decreasing = T), ]

y <- m %x% matrix(rep(1, k), nrow = 1) + # %x% returns the kronecker product
  matrix(rnorm(matrix(n*k*3)), n, k*3)   # by multiplying by 1...we're essentially
                                         # repeating the matrix k times
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

## Function: my_image ====
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y, main = "Y")
my_image(cor(y), zlim = c(-1,1), main = "Correlations of Y")
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)


## Compute the sum of squares for columns of Y and store in ss_y
##

ssy = numeric(24)
for(i in 1:24){
  x = y[,i]
  a = mean(x)
  s = (x-a)^2
  ssy[i] <- sum(s)
}
ssy

ss_y <- apply(y, 2, \(col){
  sum((col-mean(col))^2)
})

ss_y <- (sweep(y, 2, colMeans(y))^2) |> colSums()
sum(ss_y)


data.frame(x = 1:24, y = ssy, y2 = ssyv) |>
  ggplot(aes(x = x)) + geom_point(aes(y = y))
data.frame(x = 1:24, y = ssy, y2 = ssyv) |>
  ggplot()+
  geom_point(aes(x = x, y = y2), shape = 21, fill = "orange", size = 1.5)

## Compute the Sum of Squares for YV
SVD  <- svd(y)
YV   <- SVD$u %*% diag(SVD$d)
UD   <- YV
ssyv <- 
  apply(YV, 2, \(x){
    sum((x-mean(x))^2)
    })
ss_ud <- apply(UD, 2, \(col){sum((col-mean(col))^2)})
sum(ss_ud)

# Proportion of variance accounted for
# by the first three eigenvalues
sum((SVD$d^2)[1:3])/sum((SVD$d)^2)

# UD
UD = SVD$u %*% diag(SVD$d)
UD1 = sweep(SVD$u, 2, SVD$d, FUN = "*")
identical(UD, UD1)

## Assertion: 
##  We don't have to compute the sum of squares for YV directly. They can be
##  found in the squared diagonal elements of D which are the eigenvalues associated
##  with the eigenvectors which make up the column space of Y'Y and YY'
##  
##  Can we verify this empirically?

D = sum((SVD$d)^2)
D; SS_YV; SS_Y
D-SS_YV

data.frame(x = (SVD$d)^2, y = (SVD$d)^2, z = ss_yv) |> 
  ggplot(aes(x,y)) + 
  geom_line() +
  geom_point(aes(x = x, y = z), 
             col = "red", 
             size = 3) +
  labs(title = "Sum of Squares of matrix YV\nVersus D^2") +
  theme_bw()


## What percentage of total variability is accounted for by the first 3 columns of YV?

sum(SVD$d[1:3]^2)/sum(SVD$d^2)

## use the sweep function to create UD without matrix multiplication or using diag(s$d)

UD = sweep(SVD$u, 2, SVD$d, FUN = "*")



## Compute student averages and plot against UD1
avg.stdnt <- rowMeans(y)
data.frame(avg = avg.stdnt, ud = UD[,1]) |> 
  ggplot(aes(ud, avg)) + geom_point()

## Plot U1 and then V1' using same yaxis lims
data.frame(x = 1:100, y1 = U[,1]) |>
  ggplot() +
  geom_point(aes(x, y1))+ 
  
  geom_point(data = 
         data.frame(x1 = 1:24,
                    y2 = t(V)[,1]),
         aes(x1, y2),
         shape = 21, 
         fill = "tomato",
         size = 3)

## Y = UDV.1
UD.v1 <- sweep(SVD$u[,1, drop = F], 2, SVD$d[1], FUN = "*") %*% t(SVD$v[,1,drop = F])

residuals <- y - UD.v1
my_image(y, main = "Y")
my_image(UD.v1, main = "Image of UD.V1")
my_image(residuals, main = "Residuals of Y = UDV.1")
my_image(cor(residuals), zlim = c(-1,1), main = "Image of Residual Correlations - UD.V1")


# Plot U2 and V2'
data.frame(x = 1:100, 
           y = U[,2]) |>
  ggplot() +
  geom_point(aes(x, y)) 

ggplot(data = 
         data.frame(x = 1:24, 
                         y = V[,2]))+
  geom_point(aes(x, y),
             shape = 21, fill = 'orange', size = 2)
## UD.v2 ====

UD.v2 <- sweep(SVD$u[,1:2, drop = F],2,SVD$d[1:2],FUN = "*")%*%t(V[,1:2,drop = F])
residuals.1 <- y - UD.v2

my_image(y, main = "Y")
my_image(UD.v2, main = "Image of UD.V2")
my_image(residuals.1, main = "Residuals of Y = UD.V2")
my_image(cor(residuals.1), zlim = c(-1,1), main = "Image of Residual Correlations- UD.V1")


## UD.v3 ====

UD.v3 <- sweep(SVD$u[,1:3, drop = F],2, SVD$d[1:3],FUN = "*")%*%t(V[,1:3,drop = F])
residuals.2 <- y - UD.v3

my_image(y, main = "Y")
my_image(UD.v3, main = "Image of UD.V3")
my_image(residuals.2, main = "Residuals of Y = UDV.1 + UDV.2 + UDV.3")
my_image(cor(residuals.2), main = "Image of Residual Correlations")
mean(residuals.2)

## UD.v4 ====

UD.v4 <- sweep(SVD$u[,1:4, drop = F],2,SVD$d[1:4],FUN = "*")%*%t(V[,1:4,drop = F])
residuals.3 <- y - UD.v4

my_image(y, main = "Y")
my_image(UD.v4, main = "Image of UD.V4")
my_image(residuals.3, main = "Residuals of Y = UDV4")
my_image(cor(residuals.3), main = "Image of Residual Correlations")
mean(residuals.3)




# +++++++++++++++++++++++++++ #
# Advanced - Movielens ====
# +++++++++++++++++++++++++++ #

# Movielens: Load Data ====
data("movielens")

movies <- 
  movielens |>
  rename(release.year = year) |> 
  # Convert timestamp to datetime object
  # and extract the day, week, year of the review
  mutate(timestamp   = as.POSIXct(timestamp, origin = "1970-01-01"),
         review.year = lubridate::year(timestamp),
         review.week = lubridate::week(timestamp),
         review.day  = lubridate::wday(timestamp, label = TRUE))

## The resulting wide matrix is very, very large, so we need to take a sample
## Restrict the analysis to movies with more than 5 ratings and users with more
## than 50 ratings. 

movies <- 
  movies |> group_by(userId) |> filter(n() >= 50) |> ungroup() |>
  group_by(movieId) |> filter(n() >= 5 ) |> ungroup()

test.idx <- caret::createDataPartition(movies$rating, p = 0.30, list = F)
test <- movies[test.idx, ]
train <- movies[-test.idx, ]

# Ensure that movies that are in train, are in test
test <- semi_join(test, train, by = "movieId")
train <- semi_join(train, test, by = "movieId")

y.train.all <- 
  train |> 
  select(movieId, userId, rating) |>
  pivot_wider(names_from = movieId, values_from = rating)

y.train <- 
  train |> 
  select(movieId, userId, rating) |>
  pivot_wider(names_from = movieId, values_from = rating) |>
  select(-userId) |>
  as.matrix()

colnames(y.train) <-
  movies |> 
  select(movieId, title) |>
  distinct(movieId, .keep_all = T) |> 
  right_join(tibble(movieId = as.integer(colnames(y.train))), by = 'movieId') |> 
  pull(title)

# sweep out the column and user effects and we're left with residuals
y <- sweep(y.train, 2, colMeans(y.train, na.rm = T))
y <- sweep(y.train, 1, rowMeans(y.train, na.rm = T))
y |> head(c(5,5)) 

y1 <- y.train
y1[is.na(y1)] <- -1
y1 <- sweep(y1, 2, colMeans(y1, na.rm = T))
y1 <- sweep(y1, 1, rowMeans(y1, na.rm = T))
y1 |> head(c(5,5))

## Select some movies
keep <- c("Godfather, The", "Godfather: Part II, The", "Goodfellas", "Ghost", 
          "Scent of a Woman")
which(colnames(y1) %in% keep)

y1.small <- y1[, which(colnames(y1) %in% keep)]
