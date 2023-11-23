
## Harvard EdX DS Cert - Course 3: Probability

# ++++++++++++++++++++++++ 
# Load Dependencies ====
# ++++++++++++++++++++++++ 
library(tidyverse)
library(gtools)
options(digits = 3)


# Q1 - Olympic Running
medal_winners <- permutations(8,3)
medal_perms <- permutations(3,3)
dim(medal_winners)[1]

dim(medal_perms)[1]/dim(medal_winners)[1]


# Monte Carlo Simulation on Runners
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA",
             "Ecuador", "Netherlands", "France", "South Africa")
B = 10000

results <- replicate(B, {
  winners <- sample(runners, 3)
  identical(winners,c("Jamaica", "Jamaica", "Jamaica"))
})

mean(results)

# Question 2 - Restaurant Management
# A meal consists of (1) entree, (2) different sides, and (1) drink
# There are (6) options for entrees, (6) options for sides, and (2) for drinks
# Total Permutations = 6*6*5*6. But since we're interested in the group 1,2,1 we have to divide out group size
meal_options = (6 * 6 * 5 * 2) / 2

# Manager Adds a 3rd Drink
third_drink = (6 * 6 * 5 * 3) / 2
third_drink

# What if we allow a 3rd side
third_side = (6 * (6 * 5 * 4) * 3) / 6
third_side

# Entree functions
entree_calc <- function(n){
  group <- c(1,2,1)
  choices <- c(n, 6, 3)
  
  opts <- (n * (6 * 5) * 3)/prod(factorial(group))
  opts
  
}
ns <- seq(1,12)
tot_opts <- sapply(ns, entree_calc)

side_choices <- function(N){
  group <- c(1,2,1)
  choices <- c(6,N,3)
  
  opts <- (6 * (N * (N-1))  * 3) / prod(factorial(group))
  opts
}

drinks <- seq(2,12)
tot_side_choices <- sapply(drinks, side_choices)
tot_side_choices
drinks[which(tot_side_choices > 365)[1]]

# Question 3 and 4
data("esoph")
str(esoph)

all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

## Pr(Cancer | alcgp == 120++)

high_alcohol <- esoph %>% filter(alcgp == "120+")
high_alcohol %>% filter(ncases > 0)
high_alcohol %>% filter(ncases == 0)
high_alc_canc_p <- sum(high_alcohol$ncases)/(sum(high_alcohol$ncases) + sum(high_alcohol$ncontrols))

low_alcohol <- esoph %>% filter(alcgp == "0-39g/day")
low_alc_canc_p <- sum(low_alcohol$ncases)/(sum(low_alcohol$ncases) + sum(low_alcohol$ncontrols))
low_alc_canc_p

# P(tobacco > 10g/day | cancer case)
p_case = sum(esoph$ncases) / (sum(esoph$ncases) + sum(esoph$ncontrols))  

cases <- esoph %>% filter(ncases>0)
cases_tot_people <- sum(cases$ncases) + sum(cases$ncontrols)
tob_cases <- cases %>% group_by(tobgp) %>% 
  summarize(cases = sum(ncases)) 

tob_cases <- data.frame(tob_cases)

1-tob_cases$cases[1]/sum(tob_cases$cases)

controls <- esoph %>% filter(ncontrols > 0)

con_tob <- controls %>% group_by(tobgp) %>%
  summarize(controls = sum(ncontrols))

con_tob <- data.frame(con_tob)

# P(tobacco > 10g/day | cancer case) = 
1 - con_tob$controls[1]/sum(con_tob$controls)

# or
sum(con_tob$controls[2:4]) / sum(con_tob$controls)

# Questions 5 and 6

# P(alcgp = "120+" | case)
alc_cases <- 
  cases %>% 
  group_by(alcgp) %>% 
  summarize(ncases = sum(ncases)) %>% 
  mutate(prop = ncases/sum(ncases),
         odds = prop/(1-prop))


p_alc_cases <- al_cases$cases[4]/sum(al_cases$cases)
p_alc_cases

# P(tobgp == 30+ | Case)
tob_cases <- 
  cases %>%  
  group_by(tobgp) %>% 
  summarize(ncases = sum(ncases)) %>% 
  mutate(prop = ncases/sum(ncases),
         odds = prop/(1-prop))

p_tob_cases <- tob_cases$prop[4]

# P(tobgp == 30+ & alcgp == 120+ | Case)
joint_cases <- 
  cases %>% 
  filter (tobgp == "30+" & alcgp == "120+") %>%  
  pull(ncases) %>%  
  sum()

p_joint_cases <- joint_cases/sum(cases$ncases)

# P(tobgp == "30"+ OR alcgp == "120+" | Case)

p_alc_OR_tob_cases = p_alc_cases + p_tob_cases - p_joint_cases
p_alc_OR_tob_cases
cases_odds <- p_alc_OR_tob_cases/(1-p_alc_OR_tob_cases)

# For controls, what is the probability of being in the highest alcohol group?
# P(alcgp = "120+" | Control)
# Step 1. Group by alcgp then get the sum of all controls
alc_control <- 
  esoph %>% 
  group_by(alcgp) %>% 
  summarize(ncontrols = sum(ncontrols)) %>% 
  mutate(prop = ncontrols/sum(ncontrols),
         odds = prop/(1-prop))


alc_cases <- 
  esoph %>% 
  filter(ncases > 0) %>% 
  group_by(alcgp) %>% 
  summarize(ncases = sum(ncases)) %>% 
  mutate(prop = ncases/sum(ncases),
         odds = prop/(1-prop))

alc_cases
alc_control

alc_cases$odds[4] / alc_control$odds[4]

tob_control <- 
  esoph %>% 
  group_by(tobgp) %>% 
  summarize(ncontrols = sum(ncontrols)) %>% 
  mutate(prop = ncontrols/sum(ncontrols),
         odds = prop/(1-prop))

# P(alcgp == "120+" | Control)
p_alc <- alc_control$prop[4]

# P(tobgp == "30+" | Control)
p_tob <- tob_control$prop[4]

# P(tobgp == "30+" & alcgp == "120+" | Control)
y <- 
  esoph %>%
  filter(tobgp == "30+" & alcgp == "120+") %>% 
  pull(ncontrols) %>% 
  sum()
y
p_joint = y / sum(esoph$ncontrols)

# P(tobgp == "30+" OR alcgp == "120+" | Control)
p_alc_or_tob_control <- p_alc + p_tob - p_joint
p_alc_or_tob_control

controls_odds <- p_alc_or_tob_control/(1-p_alc_or_tob_control)
cases_odds/controls_odds
