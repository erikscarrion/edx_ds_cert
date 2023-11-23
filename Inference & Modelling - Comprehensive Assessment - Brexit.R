# Brexit Poll Analysis 
library(tidyverse)
library(dslabs)
options(digits = 3)

# load brexit polls data
data("brexit_polls")
str(brexit_polls)

# define actual p REMAIN IS THE POSITIVE OUTCOME
p = 0.481
d = 2*p - 1

# 1. Expected Value and SD for N = 1500
# exp val
N = 1500
N*p

# standard error of the sum
se_sum <- sqrt(N*p*(1-p))
se_sum

# standard error of the proportion X_hat
se_prop <- sqrt(p*(1-p) / N)
se_prop

# What is the expected value of the spread
d

# what is the standard error of the spread
se_spread <- 2*se_prop
se_spread

# 2. Actual Brexit Poll Estimates
brexit_polls <- brexit_polls %>%  mutate(x_hat = (spread+1)/2)


# avg,sd
brexit_polls %>%  summarize(avg_spread = mean(spread),
                            sd_spread = sd(spread),
                            avg_xhat = mean(x_hat),
                            sd_xhat = sd(x_hat))

# 3 - Confidence Interval of a Brexit Poll
brexit_polls[1,] %>%  mutate(se = sqrt(x_hat*(1-x_hat)/samplesize),
                             lower = x_hat - qnorm(.975)*se,
                             upper = x_hat + qnorm(.975)*se)
### Part 2 ###
june_polls <- brexit_polls %>%  
  filter(enddate >= "2016-06-01" & enddate <= "2016-06-30") %>%  
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread = 2*se_x_hat, 
         lower = spread - qnorm(.975)*se_spread,
         upper = spread + qnorm(.975)*se_spread,
         hit = ifelse(d<lower|d>upper,0,1))
str(june_polls)

# what proportion cover the value 0
mean(june_polls$lower<=0 & june_polls$upper >=0)

# what proportion predict remain i.e. a positive spread
mean(june_polls$lower > 0)

# what proportion of intervals cover actual value of d
mean(june_polls$hit)

## 5 hit rate by poster
june_polls %>%  group_by(pollster) %>%  summarize(tot_hits = sum(hit),
                                                  hit_rate = mean(hit), 
                                                  num_polls = n()) %>% 
  arrange(desc(hit_rate))

# boxplot by poll type

june_polls %>%  ggplot(aes(poll_type, spread)) + geom_boxplot() + geom_point()

# 7 spread across poll types
combined_by_type <-  june_polls %>% 
  group_by(poll_type) %>%  
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread+1)/2) %>% 
  mutate(q_hat = 1-p_hat, 
         se = 2*sqrt(1/N)*sqrt(p_hat*q_hat),
         lower = spread-qnorm(.975)*se,
         upper = spread+qnorm(.975)*se)
  
combined_by_type

# Q9 - ChiSq P-Value
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread+1)/2,
         se_spread = 2*sqrt(1/samplesize)*sqrt(p_hat*(1-p_hat)),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = d > spread_lower & d < spread_upper) %>%
  select(poll_type, hit)

hit_rate <- mean(brexit_hit$hit)

tab <- table(brexit_hit)
tab[1,1]
brexit_polls %>% group_by(poll_type) %>% summarize(n = n())

null <- tibble(hit = c("no", "yes"),
               online = round(85*c(1-hit_rate, hit_rate),0) ,
               phone = round(27*c(1-hit_rate, hit_rate),0))

brexit_hit
tab <- table(brexit_hit)
tab
online_odds <- tab[1,2]/tab[1,1]
online_odds
phone_odds <- tab[2,2]/tab[2,1]
phone_odds

online_odds/phone_odds

# Q11 - Spread Over Time
# update enddate to have an origin
brexit_polls$enddate <- as.Date(brexit_polls$enddate, origin="1970-01-01")

brexit_polls %>%  ggplot(aes(enddate,spread,color = poll_type)) + 
  geom_smooth(method="loess", span = 0.4) + geom_point() +
  #geom_hline(yintercept=d) + 
  labs(title = "Brexit polling spread over time",
       subtitle = "Colored by poll type",
       x = "Time",
       y = "Spread") +
  ylim(min(brexit_polls$spread), max(brexit_polls$spread))


# 12 Raw percentages over time

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>%  ggplot(aes(enddate, proportion, color = vote)) +
  geom_smooth(method="loess", span = 0.3) +
  geom_point() + 
  labs(title = "Brexit Polls - Raw Proportions Over Time",
       color = "Vote", 
       x = "Date", y = "Proportion")

