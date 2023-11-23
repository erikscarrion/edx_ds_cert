## Machine Learning - Linear Regression Assessment
## Comprehension check
library(tidyverse)
library(caret)

## ++++++++++++++++++++++++
## Simulated MVN Data ====
## ++++++++++++++++++++++++

set.seed(1, sample.kind = "Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

## Build 100 linear models using replicate
## for each, split the data evenly into test and train
## using dat$y for the indexing
## generate predictions on the test set
## calculate rmse
## report mean and sd from all 100 models

rmse <- replicate(n, {
  idx   <- createDataPartition(dat$y, list = F)
  test  <- dat[idx, ]
  train <- dat[-idx, ]
  fit   <- lm(y~x, data = train)
  preds <- predict(fit, newdata = test["x"])
  # calculate & assign RMSE
  sqrt(mean((preds-test$y)^2))
})
mean(rmse)
sd(rmse)

### Q2 - Write a function that takes a size n, then:
###     1. builds a dataset using the code at the top but with n observations
###     2. runs the replicate loop from above to build 100 linear model and returns
###         a vector or RMSE's
###     3. calculates the mean and standard deviation of the 100 RMSE's

set.seed(1, sample.kind = "Rounding")
rmse_dist <-  function(n, r = 0.5, sd, mu){
  # Build dataset
  Sigma <-  9*matrix(c(1.0, r, r, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  # Model, Predict, RMSE
  RMSE <- replicate(100, {
    # split data
    test_idx <- createDataPartition(dat$y,
                                    times = 1,
                                    p = 0.5,
                                    list = F)
    test_data  <- dat[test_idx, ]
    train_data <- dat[-test_idx, ]
    # build a model with the training data
    fit <- lm(y~x, data = train_data)
    # predict on the test data
    preds <- predict(fit, newdata = test_data["x"])
    # calculate Root Mean Square Error
    sqrt(sum((preds-test_data$y)^2/length(test_idx)))
    
  })
  
  average = mean(RMSE)
  std_dev = sd(RMSE)
  
  if(sd == T & mu == F){
    return(std_dev)
  }
  else if(sd == F & mu == T){
    return(average)
  }
  else if(sd == T & mu == T){
    return(c(average = average,sd = std_dev))
  }
}

ns <- c(100, 500, 1000, 5000, 10000)

set.seed(1, sample.kind = "Rounding")
## apply rmse_dist using map()
rmse_means <- map(ns, rmse_dist, mu = T, sd = F) |> unlist()
rmse_sds <-  map(ns, rmse_dist, mu = F, sd = T) |> unlist()
rmse_map <- data.frame(n = ns, mean = rmse_means, sd = rmse_sds)
rmse_map

## apply rmse_dist using sapply
rmse_means.1 <- sapply(ns, rmse_dist, mu = T, sd = F)
rmse_sds.1 <- sapply(ns, rmse_dist, mu = F, sd = T)
rmse_sapp <- data.frame(n = ns, 
                        mean = rmse_means.1, 
                        sd = rmse_sds.1)

rmse_sapp |> left_join(rmse_map, by = "n")

## use map to get BOTH mu and sd
rmse_both <- map(ns, rmse_dist, mu = T, sd = T) |> unlist()
data.frame(rmse_both) |> mutate(stat = rep(c("mean", "sd"), 5),
                                n = rep(ns, each = 2)) |>
  rename(value = rmse_both) |>
  pivot_wider(names_from = stat, values_from = value) |>
  ggplot(aes(n, mean))+geom_line()


## repeat but now make the correlation
## between x and y larger
set.seed(1, sample.kind = "Rounding")
Sigma.1 <-  9*matrix(c(1.0, .95, .95, 1), 2, 2)
sim.data <- MASS::mvrnorm(n = 100, mu = c(69,69), Sigma.1 ) |>
  data.frame() |>setNames(c('x', 'y'))


rmse_means.2 <- map(ns, rmse_dist, r = 0.95, mu = T, sd = F) |> unlist()
rmse_sds.2   <- map(ns, rmse_dist, r = 0.95, mu = F, sd = T) |> unlist()
rmse_df.1 <- data.frame(n = ns, 
                        mean = rmse_means.2, 
                        sd = rmse_sds.2)
rmse_df.1

rmse1 <- replicate(100, {
  idx = createDataPartition(sim.data$y, list = F)
  test = sim.data[idx, ]
  train = sim.data[-idx, ]
  fit = lm(y~x, data = train)
  preds = predict(fit, test["x"])
  sqrt(sum(((preds-test$y)^2)/length(idx)))
})
mean(rmse1)


## investigate the effect of two variables on the response
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
sim.data <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x1", "x2"))

## What is the RMSE for the following 3 models:
##    1. y ~ x1
##    2. y ~ x2
##    3. y ~ x1 + x2

set.seed(1)
idx = createDataPartition(sim.data$y, list =F)
test = sim.data[idx, ]
train = sim.data[-idx, ]
fit = lm(y ~ x1, data = train)
preds = predict(fit, newdata = test['x1'])
rmse1 = sqrt(sum(((preds - test$y)^2))/length(idx))
rmse11 = RMSE(preds, test$y)

set.seed(1)
idx = createDataPartition(sim.data$y, list =F)
test = sim.data[idx, ]
train = sim.data[-idx, ]
fit = lm(y ~ x2, data = train)
preds = predict(fit, newdata = test['x2'])
rmse2 = sqrt(sum(((preds - test$y)^2))/length(idx))
rmse22 = RMSE(preds, test$y)

set.seed(1)
idx = createDataPartition(sim.data$y, list =F)
test = sim.data[idx, ]
train = sim.data[-idx, ]
fit = lm(y ~ x1+x2, data = train)
preds = predict(fit, newdata = test[c("x1", "x2")])
rmse3 = sqrt(sum(((preds - test$y)^2))/length(idx))
rmse33 = RMSE(preds, test$y)

model_results_1 = c(x1 = c(rmse1, rmse11), 
                    x2 = c(rmse2, rmse22), 
                    x1_x2 = c(rmse3, rmse33))

## Repeat Q6 but now use highly correlated data
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
sim.data <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x1", "x2"))

## Model 1 - x1 only
set.seed(1)
idx = createDataPartition(sim.data$y, list =F)
test = sim.data[idx, ]
train = sim.data[-idx, ]
fit = lm(y ~ x1, data = train)
preds = predict(fit, newdata = test['x1'])
rmse1 = sqrt(sum(((preds - test$y)^2))/length(idx))
rmse11 = RMSE(preds, test$y)

set.seed(1)
idx = createDataPartition(sim.data$y, list =F)
test = sim.data[idx, ]
train = sim.data[-idx, ]
fit = lm(y ~ x2, data = train)
preds = predict(fit, newdata = test['x2'])
rmse2 = sqrt(sum(((preds - test$y)^2))/length(idx))
rmse22 = RMSE(preds, test$y)

set.seed(1)
idx = createDataPartition(sim.data$y, list =F)
test = sim.data[idx, ]
train = sim.data[-idx, ]
fit = lm(y ~ x1+x2, data = train)
preds = predict(fit, newdata = test[c("x1", "x2")])
rmse3 = sqrt(sum(((preds - test$y)^2))/length(idx))
rmse33 = RMSE(preds, test$y)

model_results_2 = c(x1 = c(rmse1, rmse11), 
                    x2 = c(rmse2, rmse22), 
                    x1_x2 = c(rmse3, rmse33))
model_results_1
model_results_2
