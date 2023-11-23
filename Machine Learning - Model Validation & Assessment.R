## Machine Learning - Model Validation and Assessment


##+++++++++++++++
## Set Up ====
##+++++++++++++++

## load dependencies
library(tidyverse)
library(reshape2)
library(dslabs)
library(caret)

## load data 
data("heights")

## define outcome and predictors
x <- heights$height
y <- heights$sex

## +++++++++++++++++++++++++++
## Split in2 Test-Train ====
## +++++++++++++++++++++++++++


## set seed and partition data into train and test
set.seed(2007, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = F)
test_set <- heights[test_index,]
train_set <- heights[-test_index,]

## make a random guess
y_hat <- sample(c("Male", "Female"), size = length(test_index), replace = T) %>% 
  factor(levels = levels(heights$sex))
mean(y_hat == heights$sex)


## assign male if height is > 62"
y_hat <- ifelse(x > 62, "Male", "Female") %>% 
  factor(levels = levels(heights$sex))
mean(y_hat == heights$sex)

## examine the accuracy of various height cutoffs
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(heights$sex))
  mean(y_hat == train_set$sex)
})
accuracy
plot(cutoff, accuracy)
cutoff[which.max(accuracy)]

## validate the model
y_hat_test <- 
  ifelse(test_set$height > 65, "Male", "Female") %>% 
  factor(levels = levels(heights$sex))
mean(y_hat_test == test_set$sex)


## Sensitivity and Specificity
## Sensitivity = TPR = Recall = TP/TP+FN    = P(y_hat == 1 | y == 1)
## Specificity = TNR = 1 - FPR = TN/TN+FP   = P(y_hat == 0 | y == 0)
## Specificity = PPV = Precision = TP/TP+FP = P(y == 1 | y_hat == 1)

## Creating a confusion matrix with caret
cm <- caret::confusionMatrix(data = y_hat_test, reference = test_set$sex)
cm$byClass[c("Sensitivity", "Specificity", "Prevalence")]


## Balanced Accuracy and F1 Score
## instead of using specificity and sensitivity together, instead we 
## use their harmonic mean = 1 / .5*(1/recall + 1/precision)
## Or we can specify a weight: b^2/1+b^2 & 1/1+b^2 for each, allowing us
## to change how we weight recall and precision

## caret::F_meas
cutoff = 61:70
F_1 <- map_dbl(cutoff, function(x){
 y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
   factor(levels = levels(test_set$sex))
 caret::F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff = cutoff, F1=F_1) %>% 
  ggplot(aes(cutoff, F1)) +
  geom_point()
max(F_1)
cutoff[which.max(F_1)]


## Prevalence matters. Imbalanced data = biased data => biased models if 
## not accounted for. As such, when constructing our algorithms, we should 
## always keep in mind the balance of positive vs. negative outcomes

## ROC Curves
## generate an ROC curve for guessing vs cutoff
n <- length(test_index)
y_test <- test_set$sex
p <- seq(0,1,length.out=10)

guessing <- map_df(p, function(p){
 yhat = sample(c("Male", "Female"), n, T, c(p,1-p)) %>% 
   factor(levels = c("Male", "Female"))
 list(method = "Guessing",
      p = p,
      FPR = 1 - specificity(yhat, test_set$sex),
      TPR = sensitivity(yhat, test_set$sex))
}) 

y.train <- train_set$sex
x.train <- train_set$height
y.test  <- test_set$sex
x.test  <- test_set$height


cutoffs <-c(50, seq(60,75), 80)
by_cutoff <- map_df(cutoff, function(x){
  y.hat = ifelse(x.test > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "cutoff",
       cutoff = cutoffs, 
       FPR = 1 - specificity(y.hat, y.test),
       TPR = sensitivity(y.hat, y.test))
  list(method = "Height cutoff",
       FPR = 1-specificity(y.hat, y.test),
       TPR = sensitivity(y.hat, y.test))
})

bind_rows(guessing, by_cutoff) %>% 
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point()


## Plot ROC curve using roc(response, predictor)
p.male = mean(y.train == "Male")
y.hat <- sample(c("Male", "Female"), n, T, c(p.male, 1 - p.male)) %>% 
  factor(levels = c("Male", "Female"), ordered = T)
y.test <- factor(y.test, levels = c("Male", "Female"), ordered=T)
plot(pROC::roc(y.test, y.hat))
class(y.test)

## Exercises Ch. 27
data("reported_heights")

dat <- 
  reported_heights %>% 
  # format date column
  mutate(date_time = ymd_hms(time_stamp)) %>% 
  # Analyze the 1st 2 days survey was released
  filter(date_time >= make_date(2016,01,25) & date_time < make_date(2016,02,1)) %>% 
  # Poll was done in person b/t 8:15 and 8:30
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time)== 8 &
                         between(minute(date_time),15,30),
                       "Live", "Online")) %>% 
  select(sex,type)

x <- dat$type
y <- factor(dat$sex, levels = c("Female", "Male"))

## Chi Square Test ====
## Show summary statistics that show type is predictive of sex
chisq.test(table(x,y))

## predict Sex using type
## partition the data
test_idx <- createDataPartition(y, times = 1, p = 0.49, list = F)
length(test_idx)

## ++++++++++++++++
## test 
## +++++++++++++++
x_test <- x[test_idx]
y_test <- y[test_idx]

## ++++++++++++++++
## train
## ++++++++++++++++
x_train <- x[-test_idx]
y_train <- y[-test_idx]

## ++++++++++++++++++
## prevalence of type
## ++++++++++++++++++
type_sex_table <-  table(x_train,y_train)
type_sex_prop_table <- prop.table(type_sex_table, margin = 1)

p_female_live = type_sex_prop_table[1,1]
p_female_online = type_sex_prop_table[2,1]

p_male_live = 1 - p_female_live
p_male_online = 1 - p_female_online


## ++++++++++++++++
## predict
## ++++++++++++++++


## Method 1
yhat.1 <- ifelse(x_test == "Online",
              sample(c("Female", "Male"), 
                     length(x_test),
                     replace = T, 
                     prob = c(p_female_online, p_male_online)),
              sample(c("Female", "Male"), 
                     length(x_test),
                     replace = T, 
                     prob = c(p_female_live, p_male_live)))

## ++++++++++++++++
## Train accuracy
## ++++++++++++++++
mean(yhat.1 == y_train)

## ++++++++++++++++
## Test Accuracy
## ++++++++++++++++
mean(yhat.1 == y_test)

## Method 2

### Sample type
status.sums <- table(x_train, y_train) %>% rowSums()
p.online = status.sums["Online"]/(status.sums["Live"]+status.sums["Online"])

### Predict - first randomly sample type status then predict
type_hat <- sample(c("Online", "Live"), n, replace = T, prob = c(p.online,1-p.online))
y_hat_type <- ifelse(type_hat == "Online",
                     sample(c("Male", "Female"),
                            length(type_hat),
                            replace = T,
                            prob = c(p_male_online,p_female_online)),
                     sample(c("Male", "Female"),
                            length(type_hat),
                            replace = T,
                            prob = c(p_male_live,p_female_live)))

## Accuracy with training data = 50.67%
mean(y_hat_type == y_train)
## Accuracy on test data = 45.33%
mean(y_hat_type == y_test)

## ++++++++++++++++++++++++++++++++++++
## Can we make a better approximation?
## ++++++++++++++++++++++++++++++++++++
table(test_data$type, test_data$sex)

### Simple model if Online => Male

## Model using all data
y_hat_type.2 <- factor(ifelse(dat$type == "Live", "Female", "Male"),
                       levels = c("Female", "Male"), ordered = T)

mean(y_hat_type.2 == dat$sex)
table(y_hat_type.2, dat$sex)
## sensitivity
sensitivity(y_hat_type.2, y)
## specificity
specificity(y_hat_type.2, y)

y_hat_type.1 <- 
  ifelse(x_train == "Online", "Male", "Female") %>% 
  factor(levels = c("Male", "Female"), ordered = T)

## Accuracy on train data = 65.33%
mean(y_hat_type.1 == y_train)
## Accuracy on test data = 62.67%
mean(y_hat_type.1 == y_test)

## calculate sensitivity and specificity
FPR.1 <- 1 - specificity(y_train, y_hat_type.1)
FPR.1

TPR.1 <- sensitivity(y_train,y_hat_type.1)
TPR.1

plot(pROC::roc(y_test, y_hat_type.1))

confusionMatrix(y_hat_type.1, y_test)


## Iris Dataset

data("iris")
# filter out setosa
setosa.idx <- which(iris$Species == "setosa")
data.1 <- iris[-setosa.idx,] 
levels(data.1$Species) <- c("versicolor" ,"virginica" )
# Set seed for reproducibility
set.seed(76)
# split into train and test
indxs = createDataPartition(data.1$Species, times=1, p=0.5, list=F)

x_test <- data.1[indxs,] %>% select(-Species)
y_test <- data.1$Species[indxs]

x_train <- data.1[-indxs,] %>% select(-Species)
y_train <- data.1$Species[-indxs]

## which features best predict Species
tmp <- x_train %>% summarize(min.sepal.length = min(Sepal.Length),
                      max.sepal.length = max(Sepal.Length),
                      min.sepal.width = min(Sepal.Width),
                      max.sepal.width = max(Sepal.Width),
                      min.petal.length = min(Petal.Length),
                      max.petal.length = max(Petal.Length),
                      min.petal.width = min(Petal.Width),
                      max.petal.width = max(Petal.Width)) %>% 
  as_tibble()

tmp %>% pivot_longer(cols = colnames(tmp)) %>% 
  extract(name, c("func", "var", "measurement"), 
          regex = "^(\\w+).(\\w+).(\\w+)", 
          remove = T) %>% 
  pivot_wider(names_from = c(func, measurement),
              values_from = value)

sepal_length_cutoff <- seq(4.9, 7.9, by = 0.1)
sepal_width_cutoff  <- seq(2.0, 3.8, by = 0.1)
petal_length_cutoff <- seq(3.3, 6.7, by = 0.1)
petal_width_cutoff  <- seq(1.0, 2.5, by = 0.1)
cutoffs <- c(sepal_length_cutoff, sepal_width_cutoff, 
                petal_length_cutoff, petal_width_cutoff) 
vars <- c(rep("Sepal.Length", length(sepal_length_cutoff)), 
          rep("Sepal.Width" , length(sepal_width_cutoff)),
          rep("Petal.Length", length(petal_length_cutoff)), 
          rep("Petal.Width" , length(petal_width_cutoff)))



## use map to create the accuracy dfs
accuracy_df <- map2(cutoffs,vars, 
    \(x,y) {
      y_hat <- ifelse(x_train[y] > x, "virginica", "versicolor") %>% 
        factor(levels = levels(data.1$Species))
      data.frame(cutoff = x,
                 var = y,
                 train_accuracy = mean(y_hat == y_train),
                 test_accuracy = mean(y_hat == y_test))
}) |> 
  list_rbind() 

accuracy_df |>
  ggplot(aes(cutoff, train_accuracy, color = var)) +
  geom_line() +
  geom_point()

## Optimal Petal Width
optimal_petal_width <- accuracy_df$cutoff[which.max(accuracy_df$train_accuracy)]

## Optimal Petal Length
optimal_petal_length <- 
  ## for every cutoff, calculate y_hat and then the resulting accuracy on the
  ## training data
  map(petal_length_cutoff,
      \(x){
        y.hat = ifelse(x_test$Petal.Length > x, 
                       "virginica", "versicolor") |>
          factor(levels = levels(data.1$Species))
        
        data.frame(var = "Petal.Length",
                   cutoff = x,
                   accuracy = mean(y.hat == y_train))
      }) |> 
  list_rbind() |> 
  ## retrieve idx of max accuracy
  {\(x) which.max(x$accuracy)}() |>
  ## use idx to select the cut off value
  {\(x) petal_length_cutoff[x]}()

## Joint predictive ability of petal width and petal length
temp <- ifelse(x_test$Petal.Length > optimal_petal_length &
                 x_test$Petal.Width > optimal_petal_width,
               "virginica", "versicolor") |>
  factor(levels = levels(data.1$Species))
mean(temp == y_test)  

