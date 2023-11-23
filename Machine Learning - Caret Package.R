### Caret Package 

### Overview
###   1. use caret to implement variety of ml algos
###   2. use classification trees
###   3. use random forests

### Load dependencies
library(tidyverse)
library(caret)


### load mnist_27 data
data("mnist_27")

data <- mnist_27
train = data$train
test  = data$test

### caret::train()
###   - allows us to train multiple types of models
###       - for a full list: names(getModelInfo())

train_glm <-  caret::train(y ~ ., method = "glm", data = train)
train_knn <-  caret::train(y ~ ., method = "knn", data = train)

### output of caret::train() can be used for predictions without 
### needing to look at the specifics of predict.glm or predict.knn. 
### Instead, just refer to ?caret::predict.train:

y_hat_glm <- predict(train_glm, test, type = "raw")
y_hat_knn <- predict(train_knn, test, type = "raw")

# Model validation is easy as model comparison 
mat.glm <- confusionMatrix(y_hat_glm, test$y)$overall[["Accuracy"]]
mat.knn <- confusionMatrix(y_hat_knn, test$y)$overall[["Accuracy"]]


### Cross Validation
###   - Hyper-parameter tuning

### Specify the range of neighbors to CV via tuneGrid
### Specify what type of cross validation via trControl - which takes a 
### trainControl() object

set.seed(2008)
## // default 5-fold cv
train_knn <-  caret::train(y ~ ., method = "knn", data = train,
                           tuneGrid  = data.frame(k = seq(9,71,2)))
ggplot(train_knn, highlight = T)
## // optimal k
k_cv <- train_knn$bestTune
k_cv
## // best model
train_knn$finalModel

## specify 10-fold cv
set.seed(2008)
ctrl <- trainControl(method = "cv", number = 10, p = .90)
train_knn <- caret::train(y ~ ., 
                          method = "knn", 
                          data = train,
                          tuneGrid  = data.frame(k = seq(9,71,2)),
                          trControl = ctrl)
ggplot(train_knn, highlight = T)

## // optimal k
k_cv <- train_knn$bestTune
k_cv
## // best model
train_knn$finalModel

## // inspect the results of the training algo
train_knn$results

## inspect the decision boundary of the best model chosen

range_x1 = seq(min(train$x_1), max(train$x_1), length.out = 200)
range_x2 = seq(min(train$x_2), max(train$x_2), length.out = 200)
grid.knn = expand.grid(x_1 = range_x1, x_2 = range_x2)

cond.probs = predict(train_knn, grid.knn, type = "prob")
grid.knn <- cond.probs |> 
  rename(p.2 = "2",
         p.7 = "7") |> 
  bind_cols(grid.knn) |> 
  mutate(label = factor(ifelse(p.7 > p.2, "7", "2"), levels = c("2", "7")))

pred.label = predict(train_knn, grid.knn)

grid.knn |> 
  ggplot(aes(x = x_1, y = x_2, color = label)) + 
  geom_point() +
  geom_contour(aes(x = x_1, y = x_2,z = p.7), breaks = c(0,.50)) +
  labs(title = "Decision boundary for best knn model")


## Since the boundary is wiggly, we can attempt to use Loess to fit
## We'll use: "gamLoess" in the method for train

train_loess <- caret::train(y ~ ., 
                            data = train, 
                            method = "gamLoess",
                            trControl = ctrl,
                            tuneGrid = data.frame(span = seq(0,1,by=0.1),
                                                  degree = 1))

range_x1 = seq(min(train$x_1), max(train$x_1), length.out = 200)
range_x2 = seq(min(train$x_2), max(train$x_2), length.out = 200)
grid.loess = expand.grid(x_1 = range_x1, x_2 = range_x2)

grid.loess <- grid.loess |> mutate(pred = predict(train_loess, grid, type = "raw"))

grid.loess |> ggplot(aes(x_1, x_2, color = pred)) + 
  geom_point() +
  geom_contour(aes(z = as.numeric(pred == "2")),
               color = "black",
               breaks = 0.5, 
               linewidth = 1.3) +
  labs(title = "Decision boundary for best loess model")

## Immediately we see that the decision boundary is much smoother
## What is the comparative accuracy?  

accuracy.knn <- confusionMatrix(predict(train_knn, test, type = "raw"),
                                test$y)$overall[["Accuracy"]]
accuracy.loess <- confusionMatrix(predict(train_loess, test, type = "raw"),
                                test$y)$overall[["Accuracy"]]

accuracy.knn 
accuracy.loess


### Comprehension Check - Cross validation


# create random data 
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

# run comparative t-tests to find the best predictors
pvals = rep(0, ncol(x))
for(i in 1:ncol(x)){
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal = T)$p.value
}

ind <- which(pvals <= .01)
length(ind)

## Re run the analysis using your best predictors
x_subset <- x[, ind]

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

## Re-run using KNN
set.seed(1)
fit <- train(x_subset, y, method = "knn",
             tuneGrid = data.frame(k = seq(101,301,25)))
