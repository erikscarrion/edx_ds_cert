
### Machine Learning - Section 6 
### Model Fitting   ====

### Set Up ====
# Load dependencies
library(tidyverse)
library(caret)
library(dslabs)
library(matrixStats)
library(randomForest)
library(ggthemes)

# set options
options(digits = 3,
        scipen = 999)

## MNIST: Load Data ====
mnist <- read_mnist()

# extract test/train
x.train <- mnist$train$images 
y.train <- factor(mnist$train$labels)

x.test <- mnist$test$images 
y.test <- factor(mnist$test$labels)

## MNIST: Partition Data ====
# sample 10k rows for training, 1k rows for testing
set.seed(1990)

train_idx <- sample(nrow(x.train), 10e3)
x  <- x.train[train_idx, ]
y  <- y.train[train_idx]

test_idx <- sample(nrow(x.test), 1e3)
x_test <- x.test[test_idx,]
y_test <- y.test[test_idx]


###---------------------###
### Preprocessing ====  ###
###---------------------###

# Calculate column standard deviations and 
# visualize their distribution

colsds <- colSds(x)
colsds |> qplot(bins = 256)

# use caret's nearZeroVar() function to identify variables with near zero variance that
# we can safely discard

vars_near_0 <- nearZeroVar(x)

# retrieve indices of retained columns
cols.idx    <- setdiff(1:ncol(x), vars_near_0)

## assign column names. Needed by caret
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

###
### MNIST - KNN and Random Forest ====
### 

# to get an idea of how long it's going to run, start on a small subset of data

control <- trainControl(method = "cv", number = 5, p = .9)
set.seed(1990)
index = sample(nrow(x), 3000)

start <- Sys.time()
train_knn <- caret::train(x[index, cols.idx], y[index],
                          method  = "knn", 
                          tuneGrid = data.frame(k = c(3,5,7)),
                          trControl = control) 
print(Sys.time() - start)

start <- Sys.time()
train_knn <- caret::train(x[, cols.idx], y[],
                          method  = "knn", 
                          tuneGrid = data.frame(k = c(3,5,7)),
                          trControl = control,
                          subset = index) 
print(Sys.time() - start)
## chosen k
train_knn$bestTune

## accuracy
preds = predict(train_knn,x_test[, cols.idx])
mean(preds == y_test)

## fit a knn model on entire set
start <- Sys.time()
fit_knn <- knn3(x[, cols.idx], y, train_knn$bestTune)
print(Sys.time()-start)

## accuracy
start <- Sys.time()
preds_knn <- predict(fit_knn, x_test[, cols.idx], type = "class")
print(Sys.time()-start)


cm <- confusionMatrix(preds_knn, y_test)

##
### MNIST: Random Forest - Cross Validate mtry ====
##

start.rf <- Sys.time()
train.rf <- train(x[,cols.idx], y,
                  method = "rf",
                  ntree = 150,
                  trControl = trainControl(method = "cv", number = 5),
                  tuneGrid = data.frame(mtry = c(1,5,10,25,50,100)),
                  # work with a smaller subset 
                  nSamp = 5000)  
print(Sys.time() - start.rf)

train.rf$bestTune

### MNIST: Random Forest - Train Full Model ====

start <- Sys.time()
fit.rf <- randomForest(x[,cols.idx], y, 
                       mtry = 5)
print(Sys.time() - start)

# Check for convergence
plot(fit.rf)

# Predict on the test set
preds.full.rf <- predict(fit.rf, x_test[,cols.idx], type = "response")
y_hat_rf <- preds.full.rf
# Model accuracy
mean(preds.full.rf == y_test)

## With further tuning, we can improve on the .953 accuracy this model achieves

# Variable Importance
importance(fit.rf)


### MNIST:Random Forest - Variable Importance ====

# Use importance to generate images

var_imp <- importance(fit.rf)
image_vector <- rep(0, ncol(x))
image_vector[cols.idx] <- var_imp
image_matrix <- matrix(image_vector, 28,28)

### MNIST:Random Forest - Visual Assessment ====

# grab probabilities
probs <- predict(fit.rf, x[,cols.idx], type = "prob")
p_max <- probs/rowSums(probs)

class_max <- apply(p_max, 1, {\(x) which.max(x)-1}) |> as.numeric()
max_prob  <- apply(p_max, 1, max) |>  as.numeric()


p_max <- data.frame(pred.label = class_max, p = max_prob)

ind <- which(y_hat_rf != y_test)
ind <- ind[order(p_max$p[ind], decreasing = T)]

mypar(1,4)
for(i in ind[1:4]){
  image(matrix(x_test[i,], 28, 28)[,28:1],
        main = paste0("Pr(y_hat = ",p_max$pred.label[i],") = ",round(p_max$p[i], 2),
                     " but is a ", y_test[i]),
        xaxt="n", yaxt="n"
        )
}

##
### MNIST:Ensembles ====
##  Combine the results of multiple methods for better results
##  


start = Sys.time()
p_rf <- predict(fit.rf, x_test[,cols.idx], type = "prob")
print(Sys.time() - start)

start = Sys.time()
p_knn <- predict(fit_knn, x_test[,cols.idx], type = "prob")
print(Sys.time() - start)

p <- (p_rf + p_knn)/2

y_pred = factor(apply(p, 1, which.max)-1)
mean(y_pred == y_test)

### MNIST:Training Multiple Models ====

models <- c("glm", "lda", "naive_bayes",
            "svmLinear", "gamboost", "gamLoess", 
            "qda", "knn", "kknn", 
            "loclda", "gam", "rf",
            "ranger", "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp", 
            "gbm","svmRadial", 
            "svmRadialCost", "svmRadialSigma")


## Fit each model to the data and save the results in a list
start = Sys.time()
fits <- lapply(models, \(mod){
  print(mod)
  train(y ~ ., mnist_27$train,  method = mod)})
print(Sys.time()-start)

## Make predictions on the test set
model_predictions <- map2(fits, models, \(fit, mod){
  print(mod)
  data.frame(mod = predict(fit, mnist_27$test))
}) |>  bind_cols()

colnames(model_predictions) <- models


accuracy <- apply(model_predictions, 2, function(preds){mean(preds == mnist_27$test$y)})

## Build an Ensemble Model by majority vote
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

model_predictions <- model_predictions |> 
  mutate(MajorityVote = apply(model_predictions, 1, getmode) )

model_predictions <- 
  model_predictions |> 
  mutate(votes = apply(model_predictions, 1, {\(x)
    factor(ifelse(sum(x == "7") > sum(x == "2"), "7", "2"))
  }))

##
### Exercises ====
### 
models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")

# load data

set.seed(1)
data("mnist_27")
start <- Sys.time()
model.fits <- map(models, \(x){
  print(paste("Training model: ", x))
  train(y ~., 
        data = mnist_27$train,
        method = x,
        )
})
print(Sys.time() - start)

# create matrix of predictions on the test_set
model.predictions <- map(model.fits, \(mod){
  data_frame("preds" = predict(mod, mnist_27$test))
}) |> bind_cols()

colnames(model.predictions) = models

str(model.predictions)

accuracies <- apply(model.predictions, 2, \(x)(mean(x == mnist_27$test$y)))
accuracies

majority_vote <- apply(model.predictions, 1,
                       \(x) ifelse(mean(x == 7) > 0.5, "7", "2"))

train_accuracies <- map(model.fits, \(x){
  mean(x$results$Accuracy)
}) |> unlist()

train_accuracies <- 
  sapply(model.fits, function(fit) min(fit$results$Accuracy))

train_accuracies
mean(train_accuracies)

# Select models that had Accuracy >= 80%
# take a majority vote, and assess accuracy
model.idx <- which(train_accuracies >= 0.80)
model.predictions.best <- model.predictions[model.idx] 
majority_vote <- apply(model.predictions.best, 1, \(pred) ifelse(mean(pred == "7") > 0.5, "7", "2"))
mean(majority_vote == mnist_27$test$y)
