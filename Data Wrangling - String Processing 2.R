# Wrangling - regex
library(tidyverse)
library(dslabs)
library(magrittr)

# str_detect
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")

s <- c(yes,no)
# We don't need to do all this though
str_detect(s, "cm") |str_detect(s, "inches")
# We can simply include the OR statement in str_detect
str_detect(s, "cm|inches")


# Load reported heights
data("reported_heights")

# Function for identifying entries that aren't in inches
not_inches <- function(x, smallest = 50, largest = 84){
  x <- suppressWarnings(as.numeric(x))
  ind <- is.na(x) | x < smallest | x > largest
  ind
}

problems <- reported_heights %>%    
  filter(not_inches(height))  %>%  
  .$height

length(problems)


# Define the base pattern
pattern <- "^([4-7]),(\\d*)$"
pat <- "^[4-7]'\\d{1,2}\"$"


# How many of our problems match the pattern?
problems %>% str_detect(pat) %>% sum()

# What do those look like?
problems %>% str_view(pattern)
problems %>% str_view(pat)

# What do some of the other issues look like?
problems[c(2,10,11,12,15)] %>% str_view(pattern)
problems[10:15]

# Some people wrote out inches and feet. To see which did that
str_subset(problems, "inches")

# We also seee some people used 2 single quotes instead of double quotes for inches
str_subset(problems, "''")

# define another function to address centimeters
not_inches_or_cm <- function(x, smallest = 50, largest = 84){
  # convert to numeric
  inches <- suppressWarnings(as.numeric(x))
  # indices without issues
  ind <- !is.na(inches) & ((inches >= smallest & inches <= largest)|
                             (inches/2.54) >= smallest & (inches/2.54) <= largest)
  # return indices with issues
  !ind
}

# identify problem entries
problems <- reported_heights %>% filter(not_inches_or_cm(height)) %>% 
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% # replace foot with '
  str_replace("inches|in|\"|''", "") %>% # standardize inches
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") # format the output
converted

# printing converted shows that we've got some interesting errors
pat1 <- "^[4-7]\\s*'\\s*\\d{1,2}"
index <- str_detect(converted, pat1)
mean(index)
converted[!index]

#################
## Assessment
#################
not_inches(c(175))
not_inches(c("5'8\""))
not_inches(c(70))
not_inches(c(85))

s <- c("70", "5 ft", "4'11", "", ".", "Six feet")
str_view_all(s, "\\d|feet")

# Q: 7 and 8
animals <- c("moose", "monkey", "meerkat", "mountain lion")

patterns <- c("mo*", "mo?", "mo+", "moo*")
animals <- c("cat", "puppy", "Moose", "MONKEY")

for(pattern in patterns){
  g <- str_detect(animals, pattern)
  print(g)
}
# Question 9
schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts",
             "University Georgia", "U California", "California State University")

final <- c("University of Kentucky", "University of New Hampshire", 
           "University of Massachusetts", "University of Georgia",        
           "University of California", "California State University")

patt <- "^U\\.?\\s|^Univ\\.?\\s"
str_detect(schools, patt)

schools %>% str_replace(patt, "University ") %>% 
  str_replace("^University of |^University ", "University of ")

# Question 10
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

# Question 11 - after correcting your mistake from question 10
# you update your code to the following:
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"
"^[4-7]\\s*'\\s*\\d{1,2}$"

# Question 12
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]

# Question 13
# Noticing a few entries that aren't being converted properly, we look to troubleshoot

yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")


converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")


pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)
converted

g <- as.character(seq(10,25,1))

g[str_detect(g, "[193+]")]
str_detect(g, "\\d")


