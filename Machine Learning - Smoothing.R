## Machine Learning - Smoothing & Linear Regression

## Load dependencies
library(tidyverse)
library(dslabs)
library(caret)
library(HistData)
library(pdftools)
## load data
data("mnist_27")

## plot x_1 v x_2
mnist_27$train |> 
  ggplot(aes(x_1,x_2, fill=y)) + 
  geom_point(shape = 21, size = 4.5, color = "black", alpha = .50) +
  labs(fill = "Label",
       x = "X1", y = "X2")

## Fit a model to the data
fit.mnst <- 
  mnist_27$train |>
  mutate(y = ifelse(y == 7, 1, 0)) |>
  {\(data)lm(y ~ x_1 + x_2, data = data)}()

summary(fit.mnst)

## Pull the predicted probabilities and assign to a factor variable
p.hat <- predict(fit.mnst, newdata = mnist_27$test)
y.hat <- factor(ifelse(p.hat > 0.5, 7, 2))
confusionMatrix(y.hat, mnist_27$test$y)$overall[["Accuracy"]]

## Accuracy: 75%

## Smoothing

## Bin Smoothing: 
##  for small ranges in X we assume constant Y
##  ex: assuming voter preferences are the same 
##      from week to week. We can map this preference
##      to the differential in polling

## Kernels:
##  Bin smoothing may result in a relationship that's 
##    wobbly. This is because every time the window 
##    moves, two points change. We can modify this 
##    by assigning lower weights to points further 
##    away from the center

## LOESS - Local Weighted Regression
data("polls_2008")
## Define the bin width
total_days <- diff(range(polls_2008$day))
## Assume preferences stay the same over 3 week intervals {span < 0}
span = 21/total_days
## Fit the loess model
model <- loess(margin~day, 
               degree = 1,
               span = span,
               data = polls_2008)
model$fitted
# plot the data and fitted line
data.frame(x = polls_2008$day,
           y = polls_2008$margin,
           fitted = model$fitted) |>
  ggplot(aes(x,y)) + geom_point() + 
  geom_line(aes(y=fitted),color = "red")

### Smoothing Ex. Polls Data
data("polls_2008")
qplot(day, margin, data = polls_2008)

# use regression to estimate
resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 %>% 
  mutate(resid = resid) %>% 
  ggplot(aes(day, margin)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = resid), size = 3)

## Bin Smoothers
span <- 3.5
tmp <- polls_2008 %>%
  crossing(center = polls_2008$day) %>%
  mutate(dist = abs(day - center)) %>%
  filter(dist <= span) 

tmp %>% filter(center %in% c(-125, -55)) %>%
  ggplot(aes(day, margin)) +   
  geom_point(data = polls_2008, size = 3, alpha = 0.5, color = "grey") +
  geom_point(size = 2) +    
  geom_smooth(aes(group = center), 
              method = "lm", formula=y~1, se = FALSE) +
  facet_wrap(~center)

# larger span
span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "box", bandwidth = span))
# Box Kernel
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# Normal Kernel
span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))


total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)


polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1)


polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth()

## Exercises/Comprehension Check
## Puerto Rico excess mortalities
fn <- 
  system.file("extdata", 
              "RD-Mortality-Report_2015-18-180531.pdf",
              package="dslabs")
texts <-  str_split(pdf_text(fn), "\n")
# extract data
mortalities <- 
  map_df(texts, (\(s){
    s <- str_trim(s)
    header_index <- str_which(s, "2015")[1]
    tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
    month <- tmp[1]
    header <- tmp[-1]
    tail_index  <- str_which(s, "Total")
    n <- str_count(s, "\\d+")
    out <- c(1:header_index, which(n == 1), 
             which(n >= 28), tail_index:length(s))
    s[-out] |>  
      str_remove_all("[^\\d\\s]") |> 
      (\(.) str_trim(.))() |>
      str_split_fixed("\\s+", n = 6) |> 
      {\(x) x[,1:5]}() |> 
      as_tibble() |> 
      setNames(c("day", header)) |>
      mutate(month = month, day = as.numeric(day)) |>
      pivot_longer(-c(day, month), names_to = "year", values_to = "deaths") |>
      mutate(deaths = as.numeric(deaths))
  })) |>
  mutate(month = recode(month, 
                        "JAN" = 1, "FEB" = 2, "MAR" = 3, 
                        "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, 
                        "OCT" = 10, "NOV" = 11, "DEC" = 12)) |>
  mutate(date = make_date(year, month, day)) |>
  filter(!is.na(deaths) & date <= "2018-05-01")

death_days = (max(mortalities$date)-min(mortalities$date)) |> (\(x) x[[1]])()
mortalities <- mortalities |> filter(!is.na(deaths))
span <- 60/death_days

apply(mortalities, 2, function(x){sum(x == Inf)})

# plot deaths as a function of time
mortalities |> 
  ggplot(aes(date, deaths)) + 
  geom_point(color = "grey", alpha = 0.5, size = 3) +
  geom_smooth(method = "loess", color = "orange") +
  geom_smooth(method = "lm", color = "lightblue")
  


## fit a loess model on mortalities data

loess.1 <- loess(deaths ~ as.numeric(date), data = mortalities, degree =1, span = span)
loess.2 <- loess(deaths ~ as.numeric(date), data = mortalities, degree = 2, span = span)

mortalities |> mutate(fitted.1 = loess.1$fitted,
                      fitted.2 = loess.2$fitted) |>
  ggplot(aes(date, deaths)) +
  geom_point(color = "black",
             alpha = 0.8,
             size = 1.2) +
  geom_line(aes(y = fitted.1), color = "red", size = 1.5) # + 
  geom_line(aes(y = fitted.2), color = "orange", size = 1)

get_fitted <- function(data, degree = 1, span = span){
  loess(deaths ~ as.numeric(date), 
        data = data, 
        degree = degree, 
        span = span) |>
    (\(x) x$fitted)()
  
}

## Group by day and run a loess for deaths ~ date for each group
## First, we'll use nest and map

mortalities_nested <- mortalities |> nest(data = -c(day))

mortalities_nested |> 
  mutate(model = map(data, {\(df) loess(deaths ~ as.numeric(date), data = df, 
                                        degree=2, span = .75)})) |> 
  mutate(fitted = map(model, {\(model) model$fitted})) |> 
  select(-model) |> 
  unnest(cols = c(data, fitted)) |> 
  ggplot(aes(day, deaths, color = year)) +
  geom_line(alpha = 0.8)


### Smoothing - Comprehension Check - MNIST_27
# load data
data("mnist_27")
str(mnist_27)

train = mnist_27$train
test = mnist_27$test


# train a logistic model on just x_2
fit <- train |> {\(df) glm(y ~ x_2, family = "binomial", data = df)}() 

## fit a loess model with degree = 1 using just x_2
fit.loess <- train |> mutate(y = ifelse(y==2, 1, 0)) |> 
  {\(df) loess(y ~ x_2, 
               data = df,
               degree = 1,
               span = .50,
               control = loess.control(surface = "direct"))}()

min_max_x2 <- c(min = min(train$x_2), max = max(train$x_2))

loess.summary <- summary(fit.loess)
loess.summary
# predict on the test set
preds <- predict(fit.loess, test)
preds <- ifelse(preds > 0.5,2,7)

table(preds,test$y)
mean(preds==test$y)

# calculate error
RMSE(preds, as.numeric(test$y))

## Is 0.5 the optimal span? Cross validate the span
# define k-fold cross val method
ctrl <- trainControl(method = 'cv', number = 5)
grid <- expand.grid(span = seq(0.2,.90, len = 7), degree = 1)

# cross validate
model <- caret::train(y ~ x_2, data = train, method = "gamLoess",tuneGrid = grid, trControl = ctrl)
span_cv <- model$bestTune[1,1]

train1 = train[c("y","x_2")]
## use gam.lo to run a loess regression on y

train1 <- train1 |> 
  mutate(y = ifelse(y == 2, 1, 0))

loess.lo <- gam(y ~ x_2, 
                span = span_cv, 
                data = train, 
                degree = 1,
                family = binomial(link = "logit"))
summary(loess.lo)

preds1 <- predict.Gam(loess.lo, newdata = test)
preds2 <- predict.Gam(loess.lo, newdata = test, type = "response")
preds3 <- predict.Gam(loess.lo, newdata = test, type = "terms")

head(preds1)
head(preds2)
head(preds3)

### Fit a model using loess()
fit.loess.1 <- loess(y ~ x_2, data = train, span = span_cv, degree = 1)
