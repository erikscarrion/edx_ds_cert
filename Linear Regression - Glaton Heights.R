
## Linear Regression: Galton Heights ====

# Environmental Configuration ====
library(Lahman)
library(dslabs)
library(HistData)
library(tidyverse)
library(broom)

# Set options
# options(digits = 3, scipen = 999)


## Load Galton Families Data ==== 
data("GaltonFamilies")

## Relationship between Father and Son ====
set.seed(1983)
galton_men <- 
  GaltonFamilies |> 
  filter(gender == "male") |>
  group_by(family) |> 
  # sample 1 pair of father-son heights
  # from each family  
  sample_n(1) |> 
  ungroup() |>
  select(father, childHeight) |> 
  rename(parent = father, child = childHeight) 

## Inspect father-son heights
head(galton_heights)
str(galton_heights)



## Monte Carlo Simulation: F-S Height Correlation
B <- 1000
N1 <- 25
N2 <- 50

correlations25 <- 
  replicate(B,{
    sample_n(galton_men, N1, replace = T) |> 
    summarize(r = cor(father,son)) |>
    pull(r)
})

## What's the standard error for a sample of 25? 0.1662
se25 <- sd(correlations25)

## Repeat, taking a sample of 50 this time
correlations50 <- replicate(B,{
  sample_n(galton_men, N2, replace = T) |> 
    summarize(r = cor(father,son)) |>
    pull(r)
  
})
## What's the standard error for a sample of 50? 0.1122
se50 <- sd(correlations50)

## How do the two compare? N=50 results in a 32.50% reduction in the std. error.
glue::glue("SE for n = 25: {round(se25,4)},
            SE for n = 50: {round(se50,4)}")


## What's the son's avg. height when their father is about 6 feet tall?
##   They're a little bit shorter on average - 70.5"

galton_men |> 
  filter(round(father)==72) |> 
  summarize(avg = mean(son)) |> 
  pull(avg)
  
# box plot of heights
galton_men |> 
  mutate(strata = factor(round(father))) |> 
  ggplot(aes(strata, son)) + 
  geom_boxplot() + 
  geom_point(shape = 21,
             size = 2,
             fill = "#D81B60") +
  labs(title = "Distribution of Son Heights",
       subtitle = "Stratified by level of father height",
       caption = "Data: R::HistData::GaltonFamilies",
       x = "Father",
       y = "Son") +
  theme_bw() +
  theme(plot.caption.position = "plot")



## ++++++++++++++++++++++++++++++++
## Estimating the Regression Line ====
## ++++++++++++++++++++++++++++++++


##  The line defined by y = sd(y)/sd(x) is known as the SD-Line
##  When we scale the SD-Line by the correlation coefficient, r(x,y)
##   we get the regression line
##  
##  We'll use the mothers and daughters subset as an example. 

## Galton Women ====
set.seed(1989)
galton_women <- 
  GaltonFamilies |> 
  filter(gender == "female") |> 
  group_by(family) |> 
  ## sample one pair from 
  ## each family
  sample_n(1) |> 
  ungroup() |> 
  select(mother, childHeight) |> 
  rename(parent = mother, child = childHeight)

  
## What are the mean and sd for 
## galton_women?
women_mean <- apply(galton_women, 2, mean)
women_sd   <- apply(galton_women, 2, sd)

## What is the correlation between mothers and daughters?
women_cor  <- cor(galton_women)[1,2]

## Compared to fathers whose heights have a correlation of .4331 with their sons, 
## mothers have a correlation of .3347

## Estimate the Least Squares Solutions using the correlation coefficient and the sd's of 
## mothers and daughters
mother.mu   <- women_mean[[1]]
mother.sd   <- women_sd[[1]] 
daughter.mu <- women_mean[[2]]
daughter.sd <- women_sd[[2]]
## slope and intercept calculations
b.1 <- women_cor * (mother.sd/daughter.sd)
b.0 <- mother.mu - b.1*daughter.mu

## women_lm ====
## run a linear model and compare your results
women_lm <- lm(child ~ parent, data = galton_women)

knitr::kable(tibble(group     = c('estimated', 'modeled'), 
                    intercept = c(b.0, women_lm$coefficients[[1]]),
                    slope     = c(b.1, women_lm$coefficients[[2]]))
             )

## ++++++++++++++++++++++++++++++++++++++
## Distribution of Coefficient Estimates
## ++++++++++++++++++++++++++++++++++++++

## Monte Carlo: Father-Son Coefficient Estimates ====

## Take a sample of 50
N3 <- 50
male_coefs <- 
  replicate(B, {
    x   <- sample_n(galton_men, N3, replace = T)
    mod <- lm(son ~ father, data = x)
    mod$coef
})

estimates <- tibble(B0 = male_coefs[1, ],
                    B1 = male_coefs[2, ])

## Plot the distributions of Beta0 and Beta1
estimates <- tibble(B0 = male_coefs[1, ],
                    B1 = male_coefs[2, ])

estimates |>
  ggplot(aes(B0)) +
  geom_histogram(col = "black", fill = "orange") +
  labs(title = "Distribution of Beta 0",
       subtitle = "Son ~ Father",
       caption = "Data: R::HistData::GaltonFamilies") +
  theme_bw() +
  theme(plot.caption.position = "plot")

estimates |>
  ggplot(aes(B1)) +
  geom_histogram(col = "black", fill = "orange") +
  labs(title = "Distribution of Beta 1",
       subtitle = "Son ~ Father",
       caption = "Data: R::HistData::GaltonFamilies") +
  theme_bw() +
  theme(plot.caption.position = "plot")

cor(estimates)

## There is a correlation of -0.9991. If we center X, can we de-correlate the estimates?
## What happens to the correlation if center both? 

galton_men <- 
  galton_men |> 
  mutate(centered.f = father-mean(father),
         centered.s = son - mean(son))

## Monte Carlo: Centered Father-Son Data ====
centered_father <- 
  replicate(B,{
    x <- sample_n(galton_men, 50, replace = T)
    mod <- lm(son ~ centered.f, data = x) 
    mod$coef
})

centered_son <- 
  replicate(B,{
    x <- sample_n(galton_men, 50, replace = T)
    mod <- lm(centered.s ~ father, data = x) 
    mod$coef
  })

centered_both <- 
  replicate(B,{
    x <- sample_n(galton_men, 50, replace = T)
    mod <- lm(centered.s ~ centered.f, data = x) 
    mod$coef
  })

## Collect Estimates & calculate the correlation coefficient for each of the simulations
centered_father_df <- 
  tibble(B0 = centered_father[1, ],
         B1 = centered_father[2, ],
         group = "Father")
centered_son_df <- 
  tibble(B0 = centered_son[1, ],
         B1 = centered_son[2, ],
         group = "Son")
centered_both_df <- 
  tibble(B0 = centered_both[1, ],
         B1 = centered_both[2, ],
         group = "Both")

centered_df <- 
  bind_rows(centered_father_df, centered_son_df, centered_both_df) |>
  mutate(group = factor(group))

glue::glue("Correlation: Centered-Father: {round(cor(centered_father_df[,1:2])[1,2], 4)},
            Correlation: Centered-Son   : {round(cor(centered_son_df[,1:2])[1,2], 4)},
            Correlation: Centered-Both  : {round(cor(centered_both_df[,1:2])[1,2], 4)}")



## Plot the distribution of the estimates
centered_df |>
  ggplot(aes(B0)) +
  geom_histogram(color = "black", 
                 fill = "orange") +
  labs(title = "Distribution of Beta 0",
       subtitle = "By Group",) +
  theme_bw() +
  theme(plot.caption.position = "plot") +
  facet_wrap(~ group,
             scales = 'free')

centered_df |>
  ggplot(aes(B1)) +
  geom_histogram(color = "black", 
                 fill = "orange") +
  labs(title = "Distribution of Beta 1",
       subtitle = "By Group",
       caption = "Data: R::HistData::GaltonFamiliesyy") +
  theme_bw() +
  theme(plot.caption.position = "plot") +
  facet_wrap(~ group,
             scales = 'free')


## Write a simple function to that returns the LSE estimates of single variable linear model 
lse_estimate <- function(x,y){
  # calculate estimates
  B1 = cor(x, y) * (sd(y)/sd(x))
  B0 = mean(y) - B1*mean(x)
  # Pack estimates into a list
  coefs <- list("Intercept" = B0, "Slope" = B1)
  # return value
  return(coefs)
}


# function for calculating RSS
rss <- function(data, beta0, beta1){
  # predict
  pred <- beta0 + beta1*data$parent
  # calculate residuals
  residuals <- data$child - beta0 - beta1*data$parent
  # return value
  return(sum(residuals^2))
}


## Combine the LSE & RSS Functions:
lse_rss <- function(data, centered = T, b0 = NULL, b1 = NULL){
  # Set x,y
  y = data$child
  if(centered == F){
    x = data$parent 
  }
  else{
    x = data$parent - mean(data$parent)
  }
  # estimate coefficients
  coefficients = lse_estimate(x, y)
  intercept = coefficients[[1]]
  slope = coefficients[[2]]
  
  # Which slope and intercept to use?
  if(is.null(b0) & is.null(b1)){
    return(sum((y - intercept - slope*x)^2))
  } else{
    return(sum((y - b0 - b1*x)^2))
  }
  
}

# Plot RSS for various values of B1 and B0
b1 <- seq(0,1, len = nrow(galton_men))

results <- data.frame(beta1 = b1,
                      rss   = sapply(b1, lse_rss, data = galton_men, b0 = 35, centered = T),
                      rss.1 = sapply(b1, lse_rss, data = galton_men, b0 = 40, centered = T),
                      rss.2 = sapply(b1, lse_rss, data = galton_men, b0 = 45, centered = T))

results |> 
  ggplot(aes(beta1, rss)) + 
  geom_line() + 
  labs(title = "Son~Father: RSS for varying estimates of the slope",
       subtitle = "Intercept = 35",
       caption = "Data: R::HistData::Galton Families",
       x = "Beta 1", y = "RSS") +
  theme_bw() +
  theme(plot.caption.position = "plot")

results |> 
  ggplot(aes(beta1, rss.1)) + 
  geom_line() +  
  labs(title = "Son~Father: RSS for Various Levels of Beta 1",
       subtitle = "Intercept = 40",
       caption = "Data: R::HistData::Galton Families",
       x = "Beta 1", y = "RSS") +
  theme_bw()

results |> 
  ggplot(aes(beta1, rss.2)) + 
  geom_line() + 
  labs(title = "Son~Father: RSS for Various Levels of Beta 1",
       subtitle = "Intercept = 45",
       caption = "Data: R::HistData::Galton Families",
       x = "Beta 1", y = "RSS") +
  theme_bw()
  
## Which value of Beta 1 minimizes RSS? 83
apply(results[,2:4],2, which.min)

slope.min.rss <- b1[83]
galton_men_lm <- 
  galton_men |>
  mutate(parent = parent - mean(parent))|>
  {\(df) lm(child ~ parent, data = galton_men)}()
  
lm_slope_mean <- galton_men_lm$coefficients[[2]]

## What's the difference in the estimates of the slope between
## a lm and using the r, sd.x, and sd.y?
slope.min.rss; lm_slope_mean
abs(slope.min.rss-lm_slope_mean)/lm_slope_mean*100


## Assuming B0 = 36, what is the LSE estimate for B1?
m <- (mean(galton_heights$son) - 36)/mean(galton_heights$father)


## Fit a linear model regressing son height on father height
## then predict on the father's height and check the fit
model <- lm(child ~ parent, data = galton_men)
predictions <- predict(model, interval = c("confidence"), level = 0.95)

model_predictions <- 
  as_tibble(predictions) |> 
  bind_cols(actual = galton_men$parent) |> 
  mutate(resid = actual - fit) 

## Plot the predictions against actuals
model_predictions |>
  ggplot(aes(x = actual, y = fit)) +
  geom_line(color = "blue", linewidth = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_men, aes(x = parent, y = child)) +
  labs(title = "Predicted Heights vs Actual Heights",
       caption = "Data: R::HistData:GaltonFamilies") +
  theme_bw()

## What does the distribution of residuals look like?
## They look normally distributed
model_predictions |>
  ggplot(aes(y = resid)) +
  geom_histogram(color = "black",
                 fill = "gold2") + coord_flip()

## Confirm normality with qqnorm
qqnorm(model_predictions$resid)

## ++++++++++++++++++++++++++++++++++++
## Regress Daughters onto Mothers ====
## ++++++++++++++++++++++++++++++++++++

## fit is done above with women_lm
daughter_preds <- 
  predict(women_lm,
          interval = "prediction", 
          type = "response")
galton_women$parent[1]; galton_women$child[1] ; daughter_preds[1]


## Assessment Continued - Heights
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- 
  GaltonFamilies |>
  group_by(family, gender) |>
  sample_n(1) |>
  ungroup() |> 
  gather(parent, parentHeight, father:mother) |>
  mutate(child = ifelse(gender == "female", "daughter", "son")) |>
  unite(pair, c("parent", "child"))

dat1 <- 
  galton |> 
  group_by(pair) |> 
  select(pair, parentHeight,childHeight) |> 
  summarize(correlation = cor(parentHeight,childHeight))

galton |> 
  group_by(pair) |> 
  summarize(tidy(lm(childHeight~parentHeight), conf.int=T)) |>
  filter(term == "parentHeight" & p.value < 0.05)
