library(dslabs)
library(tidyverse)
data("death_prob")


head(death_prob)
str(death_prob)
summary(death_prob)

# Q1, Q2
# An insurance company offers a one-year term life insurance policy that pays $150,000 in the event of 
# death within one year. The premium (annual cost) for this policy for a 50 year old female is $1,150. 
# Suppose that in the event of a claim, the company forfeits the premium and loses a total of $150,000, 
# and if there is no claim the company gains the premium amount of $1,150. The company plans to sell 
# 1,000 policies to this demographic.
premium <- 1150
l <- -150000
n = 1000


p <- death_prob %>%  filter(sex == "Female", age == 50) %>%  pull(prob)
q = 1-p

# What is the expected value on one policy
exp_profit <- l*p + premium*q

se_profit <- abs(premium - l)*sqrt(p*q)
se_profit

# What is the expected value of the company's profit over all 1,000 
mu_profit <- n*exp_profit

# What is the standard error of the sum of the expected value over all 1,000 policies
sd_profit <- sqrt(n*se_profit^2)

# Use the Central Limit Theorem to calculate the probability that the insurance company
# loses money on this set of 1,000 policies.
pnorm(0,mu_profit, sd_profit)

# 50 year old males have a different probability of death than 50 year old females. 
# We will calculate a profitable premium for 50 year old males in the following four-part question.

# Use death_prob to determine the probability of death within one year for a 50 year old male.
p1 <- death_prob %>% filter(sex =="Male", age == 50) %>% pull(prob)
q1 <- 1-p1

# Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 
# life insurance policies to be $700,000. Use the formula for expected value of the sum of draws 
# with the following values and solve for the premium :
exp_val <- (p1*l + q1*premium)*n
exp_val

mu_s <- 700000
premium_men <- (1/q1)*((mu_s / n) - l*p1)
exp_val_men <- (l*p1 + premium_men*q1)
se_men <- abs(premium_men - l)*sqrt(p1*q1)
se_men
# Using the new 50 year old male premium rate, calculate the standard error 
# of the sum of 1,000 premiums.
se_sum_men <- sqrt(n * se_men^2)
se_sum_men
# What is the probability of losing money on a series of 1,000 policies to 50 year old males?
pnorm(0, exp_val_men*n, se_sum_men)

# Questions 3 and 4
# In this 6-part question, we'll look at a scenario in which a lethal pandemic disease 
# increases the probability of death within 1 year for a 50 year old to .015. 
# Unable to predict the outbreak, the company has sold 1,000 $150,000 
# life insurance policies for $1,150.

p2 <- .015
q2 = 1 - p2
prem2 = 1150
exp_val2 <- p2*l + q2*prem2
exp_val2*n

# What is the standard error of the expected value of the company's profits over 1,000 policies?
se_men2 <- abs(prem2 - l)*sqrt(p2*q2)
se_sum_men2 <- sqrt(n * se_men2^2)
se_sum_men2

# What is the probability of the company losing money?
pnorm(0, exp_val2*n, se_sum_men2)

# What is the probability of losing more than $1 million?
pnorm(-1e6, exp_val2*n, se_sum_men2)

# What is the lowest death probability for which the chance of losing money exceeds 90%?
b <- abs(prem2 - l)
p2_range <- seq(.01, .03, .001)
q2_range <- 1-p2_range
mu2_range <- (p2_range*l + q2_range*prem2)*n
se_range <-sqrt(n * b^2 * p2_range*q2_range)


range_df <- data.frame(p = p2_range, 
                       q = q2_range, 
                       mu = mu2_range, 
                       sd = se_range) %>% mutate(ploss = pnorm(0, mu, sd))
range_df %>% ggplot(aes(p, ploss)) + geom_line()


p2_range[which(range_df$ploss >= .90)[1]]


### 3F
p3_range <- seq(.01, .03, .0025)
q3_range <- 1 - p3_range
mu3_range <- (p3_range*l + q3_range*prem2)*n
se3_range <-sqrt(n * b^2 * p3_range*q3_range)

range_df3 <- data.frame(p = p3_range, 
                        q = q3_range, 
                       mu = mu3_range, 
                       sd = se3_range) %>% mutate(ploss = pnorm(-1e6, mu, sd))

p3_range[which(range_df3$ploss >= .90)[1]]

# Q4
# Define a sampling model for simulating the total profit over 1,000 loans with probability 
# of claim p_loss = .015, loss of -$150,000 on a claim, and profit of $1,150 
# when there is no claim. Set the seed to 25, then run the model once.
set.seed(25, sample.kind = "Rounding")

p_loss = .015
q_loss = 1-p_loss
probs = c(q_loss, p_loss)

samp_model <- sample(c(prem2, l), 1000, probs, replace=TRUE)
sum(samp_model)/1000000

set.seed(27, sample.kind = "Rounding")
B <- 10000

S <- replicate(B, {
  X <- sample(c(prem2, l), 1000, probs, replace=TRUE)
  sum(X)
})

mean(S < -1e6)

#Q5 & 6 - Insurance Rates
# Suppose that there is a massive demand for life insurance due to the pandemic, 
# and the company wants to find a premium cost for which the probability of 
# losing money is under 5%, assuming the death rate stays stable at .015

# Calculate the premium required for a 5% chance of losing money given n = 1000 loans, 
# probability of death = .015 and loss per claim of l = 150000 Save this premium as x 
# for use in further questions.
z = qnorm(0.05)
z

x <- (l*n*p_loss - l*z*sqrt(n*p_loss*q_loss)) / (1- n*q_loss - z*sqrt(n*p_loss*q_loss))
x

# What is the expected profit per policy at this rate?
exp4 <- p_loss*l + q_loss*x

# What is the expected profit over 1,000 policies?
exp4*n
n

# Run a MonteCarlo Simulation with B = 10000 to determine the probability of loss on 1000
# policies given the new premium X with l = -150000 and p = .015
set.seed(28, sample.kind="Rounding")
B = 10000
S <- replicate(B, {
  x <- sample(c(l, x), n, c(p_loss, q_loss), replace = TRUE)
  sum(x)
})

mean(S < 0)
mean(S)

# Since the company can't predict if death rates will remain stable. Set the seed to 29, then
# write a Monte Carlo Simulation that for each of the 10000 iterations: 
# randomly changes p by adding a value between -0.01 to 0.01 
# uses the new p to generate a sample of n = 1000 policies with premium x and loss l
# returns the profit over n policiies

set.seed(29, sample.kind = "Rounding")
B = 10000
error <- seq(-.01,.01,length =100)
profit <- replicate(B, {
  new_p <- p_loss + sample(error,1)
  new_q <- 1 - new_p
  draws <- sample(c(x, l), n, c(new_q, new_p), replace = TRUE)
  sum(draws)
})
options(digits = 3)
mean(profit)
mean(profit < 0)
mean(profit < -1e6)


