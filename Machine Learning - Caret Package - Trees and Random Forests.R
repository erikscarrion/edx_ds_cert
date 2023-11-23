### Trees and Random Forests
### 
### The curse of dimensionality:
###   As the number of predictors increases, the greater the proportion of the dataset we need for
###   estimating the model. If our aim is to a smooth function estimate generated using local
###   regions, then high dimensionality needs to be countered since it works to enlarge the 
###   neighborhood to the entire space. 
###  
###  Visually it looks like this:
###  

##+++++++++++++++++
## Set Up ====
## ++++++++++++++++ 

## Load dependencies
library(tidyverse)
library(caret)
library(titanic)
library(rpart)
library(mgcv)
library(gam)
## Set options
options(digits = 3)

## ++++++++++++++++++++++++++++++++++++
##   The Curse of Dimensionality ====
## ++++++++++++++++++++++++++++++++++++
### Assume we want windows that capture 10% of the data
### p = # predictors, y = proportion of data needed
### 

p <- 1:100
qplot(p, .1^(1/p), main = "Proportion of data needed as a function of p")

## +++++++++++++++++++++++++++++++++++++++
## Circumventing high dimensionality ====
## +++++++++++++++++++++++++++++++++++++++

## Dimensionality - Olives Dataset
data("olive")
names(olive)
head(olive)
unique(olive$area)

## Goal: Build a model that predicts region using fatty acid profiles

dat <- olive |> select(-area)

## +++++++++++++++++++++++++++++++++++++++
## Olives - Region ~ Fatty Acid ====
## +++++++++++++++++++++++++++++++++++++++
ctrl = caret::trainControl(method = "cv", number = 10, p = .90)
grid = data.frame(k = seq(1, 75, by = 1))
model = caret::train(region ~ ., 
                     data  = dat,
                     method = "knn",
                     trControl = ctrl,
                     tuneGrid = grid)
model$results
ggplot(model)

dat |> pivot_longer(cols = - c(region),
                    names_to = "type",
                    values_to = "amount") |> 
 ggplot(aes(amount/100, color = region)) + 
  geom_boxplot() +
  coord_flip()+
  facet_wrap(vars(type))
head(dat)  

dat |> mutate(across(-c(region), \(x) x/100)) |> 
  pivot_longer(cols = -c(region),
               names_to = "type",
               values_to = "percentage") |> 
  mutate(type = factor(type, levels = unique(type))) |> 
  group_by(region) |> 
  arrange(desc(percentage)) |> 
  ggplot(aes(percentage, color = region)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap( ~ type, scales = "free_y")

### We see that there are a couple of variables which can uniquely identify 
### certain regions. eicosenoic acid content, for example, is enough to identify
### if it's from southern Italy or not.  

### This plot shows us that we can make a perfect decision rule using just linoleic
### and eicosenoic acid content.  
dat |> 
  ggplot(aes(linoleic, eicosenoic, color = region)) + geom_point() +
  coord_flip()

## RPART Regression Trees ====
##   We'll split the data into multiple, non-overlapping, regions and if a given x.i is in the
##   region we assign it the average value of points contained within the region
library(rpart)
data("polls_2008")

# fit an rpart model
fit <- rpart(margin ~ ., data = polls_2008)
plot(fit, margin = 0.1)
text(fit, cex=0.75)

# visualize the model
polls_2008 |> 
  mutate(fit = predict(fit)) |> 
  ggplot(aes(day, margin)) + 
  geom_point() +
  geom_line(aes(y=fit), col="red") +
  labs(title = "2008 Poll Margins by Day",
       subtitle = "Method: Regression Tree")

#####################################################
### RPART: Cross-Validate Complexity Parameter ====
### 

ctrl = trainControl(method = "cv", number = 10, p = .1)
grid = data.frame(cp = seq(0,.05, length.out = 10))

train_rpart = caret::train(margin ~ ., 
                           data = polls_2008,
                           method = "rpart",
                           trControl = ctrl, 
                           tuneGrid = grid)
ggplot(train_rpart)
cp.optimal <- train_rpart$finalModel$tuneValue

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# plot the final predictor
polls_2008 |> mutate(yhat = predict(train_rpart)) |> 
  ggplot(aes(day, margin)) + 
  geom_point() +
  geom_step(aes(y = yhat), col = "red") +
  labs(title = "2008 Poll Margins by Day",
       subtitle = "Cross Validated Regression Tree")

## If we already have a regression tree and we want to impose a higher complexity parameter, 
## we can prune the tree we currently have 
## 
## Suppose we fit a model with cp = 0 and saved it to fit > pruned_fit <- prune(fit, cp = .01)

######################################
### Classification trees ====
### The objective function is no longer RSS. 
###   Options: Gini Index & Entropy (cross-entropy loss is preferred)
###   
###   Gini(j)        = sum(p.hat(j,k)*q.hat(j,k))
###   entropy(j)     = -sum(p.hat(j,k)*log(q.hat(j,k))) w/ 0*log(0) = 0

library(randomForest)
fit.rf <- randomForest(margin ~ ., data = polls_2008)
rafalib::mypar()
plot(fit.rf)

polls_2008 |> 
  mutate(preds = predict(fit.rf)) |> 
  ggplot(aes(day, margin)) + geom_point() +
  geom_line(aes(y = preds), col = "red") +
  labs(title = "Random Forest Model fit on Polls 2008")

######################################
## Random Forest - MNIST_27 ====
## 

nodesize = seq(1, 51, 10)
acc <- sapply(nodesize, function(ns){
  train(y ~ ., method = "rf", data = mnist_27$train,
        tuneGrid = data.frame(mtry = 2),
        nodesize = ns)$results$Accuracy
})
qplot(nodesize, acc)

plot(nodesize, accuracy)

## optimized model
train_rf_opt <- randomForest(y~., data = mnist_27$train,
                             nodesize=nodesize[which.max(acc)])

confusionMatrix(predict(train_rf_opt, mnist_27$test),
                mnist_27$test$y)$overall[["Accuracy"]]

######################################
## Exercises ====
## 1. Create a data set where the outcome grows 0.75 units on average for every
## increase in a predictor

n <-  1000
sigma   <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75*x + rnorm(n,0,sigma)
dat <- data.frame(x = x, y = y)

ggplot(dat, aes(x,y)) + geom_point()

######################################
## RPART - Regression Tree - Sim Data ====
fit.1 <- rpart(y~., data = dat)
plot(fit.1, margin = .10, main = "rpart regression tree on simulated data")
text(fit.1, cex = 0.75)

dat |> mutate(pred = predict(fit.1)) |> 
  ggplot(aes(x,y)) + geom_point(color = "grey", size = 2, alpha = 0.8) +
  geom_line(aes(y = pred), col = "red") +
  labs(title = "Regression Tree Model - Sim Data")

######################################
## Random Forest - Sim Data
## 

fit.2.rf <- randomForest(y ~., data = dat)
dat |> mutate(pred = predict(fit.2.rf)) |> 
  ggplot(aes(x, y)) + geom_point(col = "grey", size = 2, alpha = 0.8) +
  geom_line(aes(y = pred), col = "red") + 
  labs(title = "Random Forest Model - Sim Data")

plot(fit.2.rf)

## refit the rf model with nodesize = 50, maxnodes = 25
## 
fit.3.rf <- randomForest(y ~., data = dat, nodesize = 50, maxnodes = 25)

dat |> mutate(preds = predict(fit.3.rf)) |> 
  ggplot(aes(x, y)) + geom_point(col = "grey", size = 2, alpha = 0.8) +
  geom_line(aes(y = preds), col = "red") + 
  labs(title = "Random Forest Model - Sim Data",
       subtitle = "nodesize = 50, maxnodes = 25")

######################################
## Tissue Gene Expression ====
## 

data("tissue_gene_expression")
GENES <- tissue_gene_expression$x |> 
  data.frame() |> 
  mutate(y = tissue_gene_expression$y)

ctrl = trainControl(method = "cv", number = 5)
grid = data.frame(cp = seq(0,0.1, by = 0.01))

set.seed(1991)
GENES.train_rpart = caret::train(y ~ ., 
                           data = GENES,
                           method = "rpart",
                           trControl = ctrl, 
                           tuneGrid = grid)
ggplot(GENES.train_rpart) + labs(title = "Gene Expression RPART Model Accuracy")

GENES.conf.mat <- confusionMatrix(predict(GENES.train_rpart, GENES), tissue_gene_expression$y)
GENES.conf.mat
GENES.train_rpart$results |> ggplot(aes(cp, Accuracy)) + 
  geom_line() + 
  geom_point()

## Plot the final tree
plot(GENES.train_rpart$finalModel, margin = .10)
text(GENES.train_rpart$finalModel, cex = 0.75)

imp <- varImp(train_rpart.1)

### Repeat but set control = rpart.control(minsplit = 0)
set.seed(1991)
GENES.train_rpart.1 = caret::train(y ~ ., 
                                 data = GENES,
                                 method = "rpart",
                                 control = rpart.control(minsplit = 0),
                                 tuneGrid = grid)
ggplot(GENES.train_rpart.1) + labs(title = "Gene Expression RPART Model Accuracy",
                                   subtitle = "minsplit = 0")

# Plot the tree & Identify the gene at the first split
plot(GENES.train_rpart.1$finalModel, margin = .10, main = "Gene Expression RPART Tree")
text(GENES.train_rpart$finalModel, cex = 0.75)

## What does the confusion matrix look like?
confusionMatrix(predict(GENES.train_rpart.1, GENES), tissue_gene_expression$y)

## What's the best complexity parameter
GENES.train_rpart.1$bestTune

## What's the accuracy of the model
confusionMatrix(predict(GENES.train_rpart.1), GENES$y)$overall[["Accuracy"]]

## Repeat with method = rf
ctrl <- trainControl(method = "cv", number = 5)
grid_rf <- data.frame(mtry = seq(50,200,25))

set.seed(1991)
GENES.train_rf = caret::train(y ~ ., 
                              data = GENES,
                              method = "rf",
                              nodesize = 1, 
                              tuneGrid = grid_rf,
                              trControl = ctrl)

## Which value of mtry maximizes accuracy?
ggplot(GENES.train_rf)

confusionMatrix(predict(GENES.train_rf.1, dat), dat$y)

imp.rf <- varImp(train_rf.1)

imp
imp.rf
top.50.vars.rf <- train_rf.1$finalModel$importance |> 
  data.frame() |> 
  rename(importance = MeanDecreaseGini) |> 
  mutate(rank = rank(-importance)) |> 
  arrange(rank)

top.50.vars.rf <- top.50.vars.rf[1:50,]
top.50.vars.rf <- top.50.vars.rf |> mutate(gene = rownames(top.50.vars.rf))

x_subset <-  select(dat,c(colnames(dat)[colnames(dat) %in% top.50.vars.rf$gene]))

train.rf.opt <- caret::train(y ~ ., 
                      data = x_subset,
                      method = "rf",
                      nodesize = 1, 
                      tuneGrid = data.frame(mtry = seq(5,40)),
                      trControl = ctrl)

x_subset |> as.matrix() |> heatmap()


###################################################
### Titanic Exercises ====
### 
### Data Preparation
### 

titanic_clean <- titanic_train |> 
  mutate(
    # Change Survived to factor
    Survived = factor(Survived),
    # Change Embarked to factor
    Embarked = factor(Embarked),
    # Impute missing Age using the median
    Age = ifelse(is.na(Age), median(Age, na.rm = T), Age),
    # Compute Family Size
    FamilySize = SibSp+Parch+1
  ) |> 
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

###################################################
### Partition into test/train @ 20%/80%
### 

set.seed(42)
test.idx <- createDataPartition(titanic_clean$Survived, p = .20, list = F)
test_set  <- titanic_clean[ test.idx, ]
train_set <- titanic_clean[-test.idx, ]

str(train_set); str(test_set)

###################################################
### Titanic - Random Prediction Model ====
### 

set.seed(3)
test_preds = factor(sample(c(0,1), size = dim(test_set)[1], replace = T))
random.accuracy <- mean(test_preds == test_set$Survived)

###################################################
### Titanic: Survived ~ Sex ====
###
### What prop. of train_set females survived?
### 

train_set |> 
  group_by(Sex) |> 
  summarize(survived = mean(Survived == 1))

### Predict survival using sex on the test_set
test_preds_sex = factor(ifelse(test_set$Sex == "female", 1, 0), levels = c(0,1))
mean(test_preds_sex == test_set$Survived)

###################################################
### Titanic: Survived ~ PClass ====
### 
### Which classes were passengers more likely 
### to survive than die - p(survived) >.50?
### 

train_set |> 
  group_by(Pclass) |> 
  summarize(survival_rate = mean(Survived == factor("1", levels = c(0,1))))
  
## using these probs, predict death or survival
test_preds_class <- factor(ifelse(test_set$Pclass == 1, 1, 0), levels = c(0,1))
mean(test_preds_class == test_set$Survived)


###################################################
## Titanic: Survived ~ Sex + Pclass ====
## 

train_set |> 
  group_by(Sex, Pclass) |> 
  summarize(survival_rate = mean(Survived == factor("1", levels = c(0,1))))

# use these probs to predict survival
test_preds_sex_class <- factor(
  ifelse(
    (test_set$Sex == "female" & (test_set$Pclass == 1 |test_set$Pclass == 2)),
    1,0
  ), 
  levels = c(0,1))
### Accuracy
mean(test_preds_sex_class == test_set$Survived)

###################################################
## Titanic: Model Comparison ====
## 
## What is each model's sensitivity, specificity, 
## and balanced accuracy?
## 

cmats <- list(confusionMatrix(test_preds_sex, test_set$Survived), 
              confusionMatrix(test_preds_class, test_set$Survived),
              confusionMatrix(test_preds_sex_class, test_set$Survived))

test_predictions = list(test_preds_sex, test_preds_class, test_preds_sex_class)

model_comparison <-  map(cmats, \(matrix){
  sensitivity       = matrix$byClass[["Sensitivity"]]
  specificity       = matrix$byClass[["Specificity"]]
  balanced_accuracy = matrix$byClass[["Balanced Accuracy"]]
 
  data.frame(sensitivity = sensitivity,
             specificity = specificity,
             balanced.accuracy = balanced_accuracy)
  }) |> 
  bind_rows() |> 
  mutate(model = c("Sex", "Class", "Sex and Class"),
         F1.Score = map(test_predictions, \(preds) {F_meas(preds,test_set$Survived)})
         ) |> 
  relocate(model)
    
###################################################
### Titanic: Survived ~ Fare (LOESS) ====
### 

### set the tune grid and control
ctrl        <- trainControl(method = "cv", number = 10)
loess.grid  <- expand.grid(span = seq(.05,.90, length.out = 20),
                           degree = 1)

### Cross Validate and Train
set.seed(1)
titanic_loess_fareOnly <- caret::train(Survived ~ Fare, 
                                       data      = train_set,
                                       method    = "gamLoess",
                                       family    = binomial(link="logit"),
                                       control   = gam.control(surface = "direct"),
                                       trControl = ctrl,
                                       tuneGrid  = loess.grid)

titanic_loess_fareOnly$finalModel$tuneValue


## Loess Model Accuracy
preds <- predict(titanic_loess_fareOnly, test_set)
mean(preds == test_set$Survived)


###################################################
### Titanic: Survived ~ Fare (Log. Reg.) ====
### 

set.seed(1)
titanic_logit_age <- caret::train(Survived ~ Age, 
                               data = train_set, 
                               method = "glm",
                               family = binomial(link = "logit"))
### Accuracy
mean(predict(titanic_logit_age, test_set)==test_set$Survived)

###################################################
### Titanic: Survived ~ Sex + Fare + Class + Age ====
### 

set.seed(1)
titanic_logit_multi <- caret::train(Survived ~ Sex + Fare + Pclass + Age, 
                                  data = train_set, 
                                  method = "glm",
                                  family = binomial(link = "logit"))
### Accuracy
mean(predict(titanic_logit_multi, test_set)==test_set$Survived)

###################################################
### Titanic: Survived ~ . ====
### 
set.seed(1)
titanic_logit_all <- caret::train(Survived ~ ., 
                                    data = train_set, 
                                    method = "glm",
                                    family = binomial(link = "logit"))
### Accuracy
mean(predict(titanic_logit_all, test_set)==test_set$Survived)


###################################################
### Titanic: KNN Model ====
###
ctrl.knn <- trainControl(method = "cv", number = 10, p = .10)
grid.knn <- data.frame(k = seq(3,51,2))

set.seed(6)
titanic_knn <- caret::train(Survived ~ ., 
                            data = train_set, 
                            method = "knn",
                            #trControl = ctrl.knn,
                            tuneGrid = grid.knn)

# optimal number of neighbors
titanic_knn$bestTune
## accuracy
mean(predict(titanic_knn, test_set) == test_set$Survived)

set.seed(8)
titanic_knn.ctrl <- caret::train(Survived ~ ., 
                            data = train_set, 
                            method = "knn",
                            trControl = ctrl.knn,
                            tuneGrid = grid.knn)

titanic_knn.ctrl$bestTune

mean(predict(titanic_knn.ctrl, test_set)==test_set$Survived)

###################################################
### Titanic: RPART Decision Tree ====
###
grid.rpart <-  data.frame(cp = seq(0,.05,.002))

set.seed(10)
titanic_rpart <- caret::train(Survived ~ .,
                              data = train_set,
                              method = "rpart",
                              tuneGrid = grid.rpart)
## best cp
titanic_rpart$bestTune

## accuracy
mean(predict(titanic_rpart, test_set) == test_set$Survived)

plot(titanic_rpart$finalModel, margin = .10, main = "Titanic Classification Tree")
text(titanic_rpart$finalModel, cex = .75)

###################################################
### Titanic: Random Forests Decision Tree ====
###
grid.rf <-  data.frame(mtry = seq(1:7))

set.seed(14)
titanic_rf <- caret::train(Survived ~ .,
                           data = train_set,
                           method = "rf",
                           ntree = 100,
                           tuneGrid = grid.rf)
## best cp
titanic_rf$bestTune

## accuracy
mean(predict(titanic_rf, test_set) == test_set$Survived)

plot(titanic_rf$finalModel, margin = .10, main = "Titanic Random Forest Decision Tree")
text(titanic_rft$finalModel, cex = .75)
