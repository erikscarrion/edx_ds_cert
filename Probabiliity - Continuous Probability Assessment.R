# 2.2 Assessment - Continuous Probability
library(tidyverse)

set.seed(16, sample.kind =  "Rounding")

act_mu <- 20.9
act_sd <- 5.7

act_scores <- rnorm(10000, 20.9, 5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores >= 36)
mean(act_scores > 30)
mean(act_scores <= 10)

x = 1:36

f_x <- plot(x = x, y = dnorm(x, act_mu, act_sd))
f_x

# Standard Scoring

act_std <- (act_scores - mean(act_scores)) / sd(act_scores)

# P(z > 2)
mean(act_std > 2)

# Question 4

ff <- function(a){mean(act_scores <= a)}
scores = 1:36
fa_scores = sapply(scores, F)
cdf_df <- data.frame(score = scores,
                     cdf = fa_scores)
cdf_df %>%  ggplot(aes(x = score, y = cdf)) + geom_point()

qnorm(.95, act_mu, act_sd)
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, probs = p)
theoretical_quantiles <- qnorm(p, act_mu, act_sd)
plot(theoretical_quantiles, sample_quantiles)
abline()

qdf <- data.frame(th = theoretical_quantiles, 
                  sa = sample_quantiles)

qdf %>% 
  ggplot(aes(th,sa)) + 
  geom_point() + geom_abline()

# Datacamp - Probability Distributions
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Define the variable 'S' specifies sum of 100 samples
S <- replicate(B, {
  X <- sample(c(17, -1), n, c(p_green, p_not_green), replace = TRUE)
  sum(X)
})

mean(S)
sd(S)







