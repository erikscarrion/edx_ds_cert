# Ch. 6 assessment
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Filter the data
polls <- 
  polls_us_election_2016 |>
  filter(state != "U.S." & enddate >= "2016-10-31") |>
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100, 
         xhat = (spread + 1) / 2,
         se = 2*sqrt((xhat*qhat)/samplesize),
         lower = spread - qnorm(.975)*se,
         upper = spread + qnorm(.975)*se) |>
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)

ci_data <- cis %>% 
  mutate(state = as.character(state)) %>% 
  left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals 
# that contain the actual value. Print this object to the console.
str(ci_data)

p_hits <- 
  ci_data %>% 
  mutate(hit = ifelse(actual_spread >= lower & actual_spread <= upper, 1, 0)) %>% 
  summarize(mean = mean(hit))

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster 
# that has at least 5 polls.

p_hits <- 
  ci_data %>%  
  group_by(pollster) %>% 
  filter(n() >= 5) %>% 
  ungroup() %>%  
  mutate(hit = ifelse(actual_spread>=lower&actual_spread<=upper, 1, 0)) %>% 
  group_by(pollster) %>%  
  summarize(n = n(),
            grade = grade[1],
            proportion_hits = sum(hit)/n) %>% 
  arrange(desc(proportion_hits)) %>%  
  select(pollster, proportion_hits, n, grade)
  

p_hits1

# Create an object called `p_hits` that summarizes the proportion of hits 
# for each state that has more than 5 polls.
p_hits2 <- ci_data %>%  group_by(state) %>% filter(n() >= 5) %>% 
  ungroup() %>%  
  mutate(hit = ifelse(actual_spread>=lower&actual_spread<=upper, 1, 0)) %>% 
  group_by(state) %>%  
  summarize(n = n(),
            proportion_hits = sum(hit)/n) %>% 
  arrange(desc(proportion_hits)) %>%  
  select(state, proportion_hits, n)

# Make a barplot of the proportion of hits for each state

p_hits2 %>%  ggplot(aes(x = reorder(state, proportion_hits), y = proportion_hits)) +  
  geom_bar(stat = "identity") +
  coord_flip()


spread <- c(ci_data$spread)
actual <- c(ci_data$actual_spread)
sign_spread <- sapply(spread, FUN = sign)
sign_actual <- sapply(actual, FUN = sign)
errors <- ci_data %>%  mutate(error = spread-actual_spread,
                              hit = sign(spread) == sign(actual_spread))

cis <- cis %>% mutate(state = as.character(state))
actual_spread = results_us_election_2016$clinton/100 - results_us_election_2016$trump/100
actuals = data.frame(state = results_us_election_2016$state, actual_spread = actual_spread)

cis <- left_join(cis, actuals, by = "state")

errors <- 
  cis %>% 
  mutate(error = spread-actual_spread, 
         hit = sign(spread)==sign(actual_spread))
head(errors)

p_hits <- 
  errors %>% 
  mutate(state = factor(state)) %>%  
  group_by(state) %>%  
  filter(n() >= 5) %>%  
  summarize(n=n(), proportion_hits = mean(hit)) %>% 
  mutate(state = reorder(state, proportion_hits))

p_hits %>%  
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat="identity") + 
  coord_flip()
