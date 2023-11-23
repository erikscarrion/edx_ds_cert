## Machine Learning - Conditional Probability
## Useful Property
##    y.hat = E(Y|X=x) minimizes MSE
##    it is because of this that we seek f(x) = E[Y|X=x]


## Compute conditional probability of being male|height

## load dependencies
library(tidyverse)
library(dslabs)
library(reshape2)
library(caret)

## load data
data("reported_heights")
data <- reported_heights

str(data)
data <- reported_heights |> 
  mutate(sex = factor(sex, levels = c("Male", "Female")),
                      height = as.numeric(height))

temp <- suppressWarnings(as.numeric(reported_heights$height))
reported_heights$heights[]
sum(is.na(temp))
na.idx <- which(is.na(temp))
reported_heights$height[na.idx]


## Repair the heights column

## function for identifying problems
not_inches_or_cm <- function(x, smallest = 50, largest = 84){
  # convert to numeric
  inches <- suppressWarnings(as.numeric(x))
  # indices without issues
  ind <- !is.na(inches) & ((inches >= smallest & inches <= largest)|
                             (inches/2.54) >= smallest & (inches/2.54) <= largest)
  # return indices with issues
  !ind
}


## problem entries
problems <- reported_heights |> 
  mutate(problems=not_inches_or_cm(height)) |>
  pull(problems)|>
  {\(x) reported_heights$height[which(x)]}()

problems

## Issues: 
##    1. Not in inches: 5'8,
##    2. Feet and inches written out: 5 feet 6 inches
##    3. Use of escape character for double quotes
##    4. Period Notation: 5.6
##    5. European Notation: 5,6


## convert all single numbers to inchses

## Function to inches
to_inches <- function(chr){
  num = as.numeric(chr)
  if(num <= 84){
    as.character(round(num*12))
  }
  else{
    as.character(round(num/2.54))
  }
}

str_replace_all(problems,
                "^(\\d{1})$", to_inches)

## Go from CM to Inches
str_detect(problems,
           regex("^[0-9]{3}\\s?[a-zA-Z]+$")) |> 
  which() |>
  {\(x) problems[x]}() |>
  str_replace_all(pattern = "^([0-9]{3})\\s?[a-zA-Z]+$",
                  replacement = "\\1") |>
  lapply(to_inches) |> 
  unlist()

## Probability of Disease & positive test results

## Sensitivity and Specificity of the Test
p.positive.disease = .85
p.negative.healthy = .90

## prevalence of disease in the population
p.disease <- .02

## sample the population
set.seed(1)
observations <- sample(c("Healthy","Disease"),
                       size = 1e6, 
                       replace = T, 
                       prob = c(.98,.02))
test_results <- logical(1e6)
test_results[observations == "Healthy"] <- 
  sample(c(TRUE, FALSE),
         size = sum(observations == "Healthy"),
         replace = T,
         prob = c(.10,.90))
test_results[observations == "Disease"] <- 
  sample(c(TRUE,FALSE),
         size = sum(observations == "Disease"),
         replace = T,
         prob = c(.85,.15))

## probability someone has the disease given a negative test result
mean(observations[test_results == FALSE]=="Disease")

## P(disease|positive test)
mean(observations[test_results == TRUE] == "Disease")

## compare prevalence of disease in those who test positive
## versus the overall prevalence of the disease
## If a patient's test is positive, how much likelier are they to have the disease
mean(observations[test_results == TRUE] == "Disease")/mean(observations=="Disease")


## Comprehensions Check - 2
## Conditional probability of being male given height
heights |> 
  mutate(height = round(height)) |>
  group_by(height) |>
  summarize(p_male = mean(sex=="Male")) |>
  ggplot(aes(height,p_male)) + geom_line() + geom_point()

## smooth out the variability by utilizing the cut function
ps <- seq(0., .9, 0.1)

heights |> 
  mutate(g = cut(height, quantile(height, ps),include.lowest = T)) |>
  group_by(g) |>
  summarize(p = mean(sex == "Male")) |>
  ggplot(aes(g, p)) + geom_line() + geom_point()

## Generate Bivariate data with the MASS Package:
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
	data.frame() %>% setNames(c("x", "y"))
plot(dat)

dat |> 
  mutate(g = cut(x, quantile(x,seq(0,1,0.1)),include.lowest = T)) |>
  group_by(g) |>
  summarize(x = mean(x), y = mean(y)) |>
  ggplot(aes(x,y)) + geom_point(alpha = 0.5)
