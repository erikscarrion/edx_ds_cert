### Machine Learning - Cross Validation and KNN

## ++++++++++++++++
## Set up ====
## ++++++++++++++++

library(tidyverse)
library(dslabs)
library(caret)

## Load Data
data("mnist_27")

train = mnist_27$train
test = mnist_27$test

## x1 v x2
mnist_27$train |> 
  ggplot(aes(x_1, x_2, color = y)) + 
  geom_point()

### KNN
###   - smoothing/complexity controlled by k (number of neighbors)
###       - more neighbors = smoother fit / lower complexity


### Fit a knn model on mnist_27$train
knn.fit <- knn3(y ~ ., data = mnist_27$train, k = 5, prob = TRUE)
summary(knn.fit)
plot(knn.fit$theDots$prob)

preds = predict(knn.fit, newdata = mnist_27$train, type = "class")
mean(preds == mnist_27$train$y)
confusionMatrix(preds, mnist_27$train$y)

## visualize
train |> 
  mutate(y_hat = preds) |> 
  ggplot(aes(x_1, x_2, color = y_hat)) +
  geom_density2d()

## +++++++++++++++++++++++++
## Decision Boundary ====
## +++++++++++++++++++++++++
min_x1 <- min(train$x_1)
max_x1 <- max(train$x_1)
min_x2 <- min(train$x_2)
max_x2 <- max(train$x_2)

# mesh step size
H = 0.01
x1_axis_range <- seq(min_x1, max_x1, by = H)
x2_axis_range <- seq(min_x2, max_x2, by = H)

# create mesh grid
grid = expand.grid(x_1 = x1_axis_range, x_2 = x2_axis_range)
probs = predict(knn.fit, grid) |> data.frame()
grid <- bind_cols(grid, probs)

grid <- grid |> mutate(label = factor(ifelse(X7>X2, "7", "2"), levels = c("7", "2")))

grid |> 
  ggplot(aes(x_1, x_2, color = label)) + 
  geom_point() + 
  labs(title = "Model Decision Boundary",
       subtitle = "Data: MNIST_27")

### What is the optimal number of neighbors?
k = seq(3, 300)
temp = map(k, {\(x) knn3(y~., data = train, k = x, prob = TRUE)})

model_performance <- map(temp, \(model){
  # generate probabilities
  labels.train = predict(model, train) |> 
    data.frame() |> 
    bind_cols(train) |> 
    mutate(label = factor(ifelse(X7>X2, "7", "2"), levels = c("7", "2"))) |> 
    pull(label)
  
  labels.test = predict(model, test) |> 
    data.frame() |> 
    bind_cols(test) |> 
    mutate(label = factor(ifelse(X7>X2, "7", "2"), levels = c("7", "2"))) |> 
    pull(label)
  
  data.frame(k = model$k,
             train_error = 1 - mean(labels.train == train$y),
             test_error = 1 - mean(labels.test == test$y))
  
}) |> bind_rows() |> arrange(desc(test_error)) |> 
  pivot_longer(cols = c(train_error, test_error),
               names_to = "error", values_to = "value") 

model_performance |>
  ggplot(aes(k, value, color = error)) + geom_line()

model_wide <- model_performance |> pivot_wider(names_from = error, values_from = value) 
model_wide[which.min(model_wide$test_error),]


summary(knn.fit)

### Previously, we used logistic regression to predict sex based on height. Now 
### we are going to use KNN to do the same. Set the seed to 1, then use the 
### caret package to partition the dslabs heights data into a training and test 
### set of equal size (p = 0.5). Use the sapply() function to perform KNN with k 
### values of seq(1, 101, 3) on the training set and calculate F1 scores on the 
### test set with the F_meas() function using the default value of the relevant 
### argument.

data("heights")
k = seq(1,101,3)
set.seed(1)
idx <- createDataPartition(heights$height, list = F)
test = heights[idx, ]
train = heights[-idx, ]
knn_f1_meas <- map(k, \(neighbors){
  model = knn3(sex ~ height, data = train, k = neighbors)
  preds = factor(predict(model, test, type = "class"), levels = c("Female","Male")) 
  data.frame(k = neighbors, F = F_meas(table(preds,test$sex)))
}) |> bind_rows()

knn_f1_meas |> ggplot(aes(k, F)) + geom_line()
options(digits = 3)
knn_f1_meas[which.max(knn_f1_meas$F),]
head(knn_f1_meas)

### For the next question, we'll be using the tissue gene data set
data("tissue_gene_expression")
str(tissue_gene_expression)
tissue_gene_expression$x[185:189, 1:5]
?tissue_gene_expression

genes <- tissue_gene_expression$x |> data.frame() |>
  mutate(tissue = rownames(tissue_gene_expression$x)) |> 
  pivot_longer(cols = -tissue,
               names_to = "gene", values_to = "expression") |> 
  bind_cols(tissue_gene_expression$y)

set.seed(1)
#split into test and train
idx = createDataPartition(tissue_gene_expression$y, list = F)
x_test = tissue_gene_expression$x[idx, ]
y_test = tissue_gene_expression$y[idx]

test <- x_test |> as.data.frame() |> mutate(y = y_test)
colnames(test) <- make.names(colnames(test))
x_train = tissue_gene_expression$x[-idx, ]
y_train = tissue_gene_expression$y[-idx]

train <- x_train |> as.data.frame() |> mutate(y = y_train)
colnames(train) <- make.names(colnames(train))
# neighbors to validate
k = seq(1,12,2)

## fit models for each level of k and record accuracy
accuracy <- map(k, \(neighbors){
  model       = knn3(y~ ., data = train, k = neighbors)
  preds.train = factor(predict(model, newdata = train, type = "class"),
                       levels = levels(tissue_gene_expression$y) )
  preds.test  = factor(predict(model, newdata = test,  type = "class"),
                       levels = levels(tissue_gene_expression$y))
  
  data.frame(neigh = neighbors,
             train = mean(preds.train == y_train),
             test = mean(preds.test == y_test))
}) |> bind_rows()
accuracy |> ggplot(aes(x = neigh)) + 
  geom_line(aes(y = train), color = "blue") +
  geom_line(aes(y = test), color = "orange")
  

### Cross Validation - Exercises

# generate random data
set.seed(1996)
n <-  1000
p <- 10000
x <-  matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep="_")
y <- rbinom(n, 1, 0.5) |> factor()

x_subset <- x[,sample(p,100)]

## Cross-Validate a logistic regression model
ctrl <- trainControl(method = 'cv', number = 5)

cv_models <- data.frame(x_subset) |> mutate(y = y) |> 
  {\(df) caret::train(y~., data = df, method = "glmnet", trControl = ctrl, family="binomial")}()

cv_models$results

### createResample as a way to boot strap
set.seed(1995)
indexes <-createResample(mnist_27$train$y, 10)
class(indexes[[1]])

first_resample <- indexes$Resample01
sum(first_resample %in% c(3,4,7))
sum(first_resample==7)

map(indexes, \(idx){
  g = sum(idx == 3)
  data.frame(three = g)
}) |> bind_rows() |> summarize(threes = sum(three))

set.seed(1)
quant.dist <- replicate(10e3, {
  y <- rnorm(100,0,1)
  quantile(y, 0.75)
})
mean(quant.dist)
sd(quant.dist)

## bootstrap the same data
set.seed(1)
y <- rnorm(100,0,1)

quant.dist.boot <-  replicate(10e3, {
  y.b <- sample(y, 100, replace = T)
  quantile(y.b, 0.75)
})
mean(quant.dist.boot)
sd(quant.dist.boot)