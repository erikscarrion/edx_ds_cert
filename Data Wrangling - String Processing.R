
## +++++++++++++++++++++++++++++++++
## String Processing & Web Scraping ====
## +++++++++++++++++++++++++++++++++

## Load dependencies
library(tidyverse)
library(dslabs)
library(ggthemes)
library(rvest)


## +++++++++++++++++++++++++++++++++
## Gun Violence in the US by State ====
## +++++++++++++++++++++++++++++++++


## URL of dataset
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")

## Create the raw table 
murders_raw <-
  ## read the html
  read_html(url) %>%
  ## find the tables 
  html_nodes("table") %>% 
  ## html_table() returns a list of tibbles
  html_table() %>%
  ## We want the first table
  .[[1]] %>%
  ## NOTE: . notation only works with %>%
  ## R's native pipe |> requires an anonymous 
  ## function + a function call : {\(.) .[[1]]}()
  ##
  ## Column Names
  setNames(c("state", "population", "total", "murder_rate"))

## inspect
head(murders_raw)
str(murders_raw)

## To-Do
##    - Convert population & total to numeric
##      + population: remove commas and convert to numeric
##      + total & murder_rate: convert to numeric


murders_raw |> 
  ## Apply the same summary to each column
  summarize_all(
    ## Detect if there are commas
    function(x){
      any(str_detect(x, ","))
  })

## You can manually remove commas and convert to numeric
## with str_replace_all()
murders_raw$population <-
  str_replace_all(murders_raw$population, ",", "") |>
  as.numeric()

## Or you can use parse_number which does it automatically 
## In combination with mutate_at, we're able to selectively
## apply the transformation
murders_raw <- 
  murders_raw |>
  mutate_at(c("population", "total"), parse_number)

## Visualize  the relationship between population and murder rate
murders_raw |>
  ggplot(aes(population/10e5, murder_rate)) + 
  geom_point(shape = 21, size = 5, fill = "tomato2", alpha = 0.85) +
  labs(title = "Are Larger States Safer?",
       subtitle = "Murder rate vs. Population Size",
       caption = "Source: Wikipedia: Gun Violence in the United States by State",
       x = "Population (in millions)",
       y = "Murder Rate (per 100k)",
       color = "Total Murders") +
  ## Add a label for the largest murder rate
  annotate("label", x = 5, y = 23.5, label = "District of\nColumbia", fill = "lightblue") + 
  theme_bw() +
  theme(
    ## Align caption to right
    plot.caption.position = "plot")

## Observations: 
##  - DC is dangerous. 
##  - States with more than 10 million people have 
##    murder rates that are stabilized around 5 per 100K. 
##  - States with fewer than 10 million inhabitants
##    show much greater variability. Excluding DC, they 
##    have a range of 1.6 to 8 murders per 100K


## +++++++++++++++++++++++++
## Reported Heights Data ====
## +++++++++++++++++++++++++

## load data
data("reported_heights")


## Inspect for issues to correct
tail(reported_heights, n = 20)
reported_heights$height


## Issues: 
##    - Some heights in m|cm 
##      - some use the european decimal
##    - Some are written with words
##    - Some are single numbers
##    - Some written as F'I'' with and without spaces

## Helper function to identify entries that aren't
## written in inches or centimeters

not_inches_or_cm <- function(x, shortest = 50, tallest = 84){
  # convert to numeric
  # If it gets something that isn't numeric
  # it'll return an NA
  inches <- suppressWarnings(as.numeric(x))
  cm2in  <- inches/2.54
  ind <- 
    is.na(inches) | 
    ((inches < shortest & inches > tallest)|
       (cm2in < shortest & cm2in >=tallest))

  ind
}

problems <- 
  reported_heights |> 
  filter(not_inches_or_cm(height)) |> 
  pull(height)

## +++++++++++++++++++++++++
## Patterns needed
## ++++++++++++++++++++++++
pattern_1 <- "^([4-7]),(\\d*)$"
pattern_2 <- "^[4-7]'\\d{1,2}\"$"
pattern_3 <- "feet|foot|ft"
pattern_4 <- "inches|in|\"|''"
pattern_5 <- "^([4-7])'|\\s\\s?w{3}?\\s?(\\d{1,2}\\.?\\d{0,2})\\s*$"


converted <-  
  problems |> 
  str_replace(pattern_3, "'") |> 
  str_replace(pattern_4, "") #|>
  str_replace(pattern_5, "\\1'\\2") |>
  str_replace(",","'")

converted

g = "5 11"
f = "5' 4"
str_replace(f, "([4-7])*\\s(\\d{1,2})", "\\1\\2")

