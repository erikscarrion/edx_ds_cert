# load dependencies
library(tidyverse)
library(dslabs)

# probabilities
points <- c(1, -0.25)
probs <- c(1/5, 4/5)
trials <- 44
choices_per_q <- 5

# Suppose that students guess on every question
# Each question is an independent trial
# with success p = 0.20 and q = 1-p = .80
p = 0.20
q = 1 - p
# Expected points  per question
exp_pts <- sum(points*probs)
exp_pts

se_pts <- abs(1 - -(1/4))*sqrt(trials*p*q)
se_pts

1 - pnorm(8, exp_pts, se_pts)

set.seed(21, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  X <- sample(points, trials, probs, replace=TRUE)
  sum(X)
})

mean(S > 8)

# The SAT reduced the number of choices to 4 and eliminated the 
# penalty for guessing. How does this affect the expected values?
p_new = 1/4
q_new = 1-p
pts_new = c(1, 0)
probs_new = c(p_new, q_new)
exp_pts_new <- sum(pts_new*probs_new)
exp_pts_new*44


# Consider a range of correct answer probabilities
# Whats the lowest P such that p(S > 35) > .80

p_range <- seq(0.25, .95, .05)
q_range = 1 - p_range
pts_range = rep(pts_new, length(p_range))
probs_range <- c(p_range, q_range)

exp_pts_range <- probs_range*pts_range * 44
exp_pts_range <- exp_pts_range[exp_pts_range !=  0]             
sd_pts_range <- sqrt(trials*p_range*q_range)

# probabilities of a score greater than 35
range_df <- data.frame(p = p_range,
                       exp_pts = exp_pts_range,
                       sd_pts = sd_pts_range)

range_df %>%  ggplot(aes(p, exp_pts)) + geom_line()

ones = rep(1, length(p_range))
ones
zeros = rep(0, length(p_range))


a <- data.frame(p = p_range,
                q = q_range,
                right = ones,
                wrong = zeros)
a <- a %>%  mutate(exp_pts = trials*(p*right + q*wrong),
                   sd_pts = sqrt(trials*p*q),
                   pxgt35 = 1 - pnorm(35, exp_pts, sd_pts))             

p_range[which(a$pxgt35>.80)[1]]


# Question 3 - Betting on Roulette
# House offers a Special Bet on 5 pockets = (00,0,1,2,3) out of 38
# the bet pays 6:1 i.e. winnings = c(6, -1)
# what is the probability of losing money if you place 500 bets on the house special


# What is the expected value of the payout on one bet
p3 <- 5/38
q3 <- 1 - p3

exp_3 <- p3*6 - q3
exp_3

se_3 <- abs(6 - -1)*sqrt(p3*q3)
se_3

# What is the expected value of the average payout over 500 bets
nbets = 500
exp_3

# what is the standard error of the average payout over 500 bets
se_3/sqrt(nbets)

# What is the expected value of the sum of 500 bets?
exp_3*nbets

# What is the standard error of the sum of 500 bets?
sqrt(sum(nbets*(se_3^2)))

# Use pnorm() with the expected value of the sum and standard error of the sum to 
# calculate the probability of losing money over 500 bets
pnorm(0, exp_3*nbets, sqrt(sum(nbets*(se_3^2))))


l <- loss_per_foreclosure
x = -l*( n*p_default - z*sqrt(n*p_default*(1-p_default)))/ ( n*(1-p_default) + z*sqrt(n*p_default*(1-p_default)))
l*p_default + x*(1-p_default)    # expected value of the profit per loan
n*(loss_per_foreclosure*p_default + x*(1-p_default)) # expected value of the profit over n loans

