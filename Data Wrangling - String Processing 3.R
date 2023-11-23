# Wrangling - String Processing Pt. 3

# Load required packages
library(tidyverse)
library(dslabs)

# read in data
data("reported_heights")

# Function for identifying data not in inches
not.inches <- function(x, shortest = 50, tallest = 84){
  x <- suppressWarnings(as.numeric(x))
  ind <- is.na(x) | x < shortest | x > tallest
  ind
}

# Function for inches & centimeters
not.inches.cm <- function(x, shortest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  # indices without issues
  ind <- !is.na(inches) & ((inches >= shortest & inches <= tallest)|
                             (inches/2.54) >= shortest & (inches/2.54) <= tallest)
  # return indices with issues
  !ind
}

# collect the problems entries
problems <- reported_heights %>% filter(not.inches.cm(height)) %>% 
  .$height

# How many problem entries do we have?
length(problems)


# Identifying the issues
head(problems)
problems

# Write a function to address each of the issues in turn
convert_data <- function(problemas){
  problemas %>% 
    # case 1: append 0 when we have just a 5,6 5', or 6'
    str_replace_all("^([56])'?$", "\\1'0") %>% 
    # case 2 - inches written out
    str_replace_all("inches|in|''|\"|cm|and","") %>% 
    # case 3 - feet written out
    str_replace("feet|foot|ft|ft.", "'") %>% 
    # height written in words
    str_replace_all("four|Four", "4") %>% 
    str_replace_all("five|Five", "5") %>% 
    str_replace_all("six|Six", "6") %>% 
    str_replace_all("seven|Seven", "7") %>%
    str_replace_all("eight|Eight", "8") %>%
    # case 4 - change x.y, x y, x,y, x .y, x ,y
    str_replace_all("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% 
    # remove spaces between characters
    str_replace_all("^([5-6])\\s*'\\s*(\\d*)","\\1'\\2") %>% 
    # change the european decimal
    str_replace("^([56])\\s*,\\s*(\\d*)$", "\\1.\\2") %>% 
    # trim white space
    str_trim()
}
converted_problems <- convert_data(problemas = problems)
remaining_problems <- converted_problems[not.inches.cm(converted_problems)]


# Use str_detect to identify the remaining issues
filter <- str_detect(converted_problems, pattern = "^[4-7]'\\d+$", negate=T)
converted_problems[filter]


