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