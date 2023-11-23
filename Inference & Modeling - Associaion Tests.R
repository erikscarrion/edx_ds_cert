## Load dependencies
library(tidyverse)
library(reshape2)
library(dslabs)

## Causation is not association
data("admissions")
summary(admissions)
head(admissions)

berkeley_admissions <- admissions %>% 
  pivot_wider(names_from = gender, values_from = c(admitted,applicants)) %>% 
  mutate(application_rate_women = applicants_women/(applicants_women+applicants_men),
         application_rate_men = applicants_men/(applicants_women+applicants_men),
         selectivity_men = admitted_men/applicants_men,
         selectivity_women = admitted_women/applicants_women,
         selectivity_total = (admitted_men + admitted_women)/(applicants_men+applicants_women),
         ApplicantDifference = applicants_women-applicants_men) %>%
  mutate(MajorRank = rank(-selectivity_total)) %>% 
  arrange(MajorRank) 
  
berkeley_admissions %>% 
  ggplot(aes(MajorRank, application_rate_women, label=major)) +
  geom_text(color="blue") + 
  labs(title = "Proportion of Female Applicants",
       subtitle = "UC Berkeley Admissions Data") +
  xlab("Major Selectivity") +
  ylab("Rate")


# Difference in admissions between men and women by major
admissions %>% 
  group_by(gender, major) %>% 
  summarize(admitted = sum(admitted)) %>% 
  pivot_wider(names_from = gender, values_from = admitted) %>% 
  mutate(difference = women-men)

admissions %>% 
  mutate(rejected = applicants - admitted) %>% 
  group_by(gender) %>% 
  ggplot(aes(gender,applicants)) +
  geom_col(fill="orange") +
  geom_col(aes(gender, rejected), fill = "blue") +
  facet_wrap(~major)
 
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))


## Exercises for Ch. 19 - Confounders
funding <- research_funding_rates

funding_data <- funding %>%
  pivot_longer(cols = -c(discipline, success_rates_total, 
                         success_rates_men, success_rates_women),
               names_to = c("status","gender"),
               names_pattern = "(applications|awards)_\\w?_?(men|women)") %>% 
  ## factorize discipline and re-order by success_rate
  mutate(discipline = factor(discipline)) %>%
  mutate(discipline = fct_reorder(discipline, success_rates_total)) %>% 
  filter(gender %in% c("men", "women")) %>% 
  pivot_wider(names_from  = status,
              values_from = value) %>% 
  mutate(denied = applications - awards) %>% 
  rename(awarded=awards)

funding_data %>% 
  ## Visualize distribution
  ggplot(aes(discipline, awarded/applications, color = gender)) +
  geom_point(aes(size = applications)) +
 # geom_point(aes(y = success_rates_total), color = "black") + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Is there a gender bias in grant awards?",
       subtitle = "Awards by Discipline",
       y = "Percent Awarded",
       x = "")
      
## Conduct a chi-square test for each discipline
## define a function that takes in a 2x2 table and returns a data frame
## with a p-value. Use the 0.5 correction then use the summarize function


### function to take in a 2x2 table and returns a data frame with a p-value
p_val <- function(table){
  test <- chisq.test(table)
  data.frame(p_value = test$p.value)
}


### Chi-Square test for each discipline
funding_data %>% 
  ## pivot_longer
  pivot_longer(cols = c(awarded,denied),
               names_to = "status") %>%
  group_by(discipline) %>% 
  summarize(p_val(acast(across(), gender~status,sum)))

### For the medical sciences, there appears to be a statistically significant 
### difference. But is this a spurious correlation? We performed 9 tests. 
### Reporting only the one case with a p-value less than 0.05 might be 
### considered an example of cherry picking. Repeat the exercise above, but 
### instead of a p-value, compute a log odds ratio divided by their standard error. 
### Then use qqplot to see how much these log odds ratios deviate from the 
### normal distribution we would expect: a standard normal distribution.

funding_data_odds <- 
  funding_data %>%
  # What are the odds of being accepted given you're a man
  group_by(discipline, gender) %>% 
  mutate(odds_ratio = (awarded/denied)) %>% 
  ungroup %>% 
  select(discipline, gender, odds_ratio) %>% 
  pivot_wider(names_from = gender, values_from = odds_ratio) %>% 
  mutate(log_odds = log(men/women)) %>% 
  pivot_longer(cols = c(men, women), names_to = "gender", values_to = "odds_ratio") %>% 
  left_join(funding_data, by = c("discipline", "gender"))

## Write a function that puts out the standard error of the log_odds_ratio

odds_ratio <- function(table){
  log_odds = (table[1,1]/table[1,2])
}


funding_data_odds %>% 
  select(discipline, gender, awarded, denied) %>% 
  pivot_wider(names_from = gender,
              values_from = c(awarded,denied)) %>% 
  mutate(se_log_odds = sqrt(1/awarded_men + 1/awarded_women +
                            1/denied_men + 1/denied_women)) %>% 
  pivot_longer(cols = -c(discipline, se_log_odds),
               names_to = c("status","gender"),
               names_pattern = "(\\w+)_(\\w+)",
               values_to = "count") %>% 
  left_join(funding_data_odds, by = c("discipline", "gender")) %>%
  ## keep a subset of columns for analysis
  select(discipline, status, gender, log_odds, se_log_odds) %>% 
  ## group by discipline
  group_by(discipline) %>% 
  ## calculate normalized log_odds
  summarize(or_norm = log_odds/se_log_odds) #%>% 
  ## keep discipline and or_norm
  select(discipline,or_norm)

funding %>% 
  pivot_longer(cols = - c(discipline,success_rates_total, 
                          success_rates_men,
                          success_rates_women),
               names_to = c("status", "gender"),
               names_pattern = "(\\w+)_(\\w+)") %>% 
  filter(gender %in% c("men", "women")) %>% 
  # *************************
  # calculate the odds ratio
  # *************************
  pivot_wider(names_from = status,
              values_from = value) %>%
  mutate(denied = applications-awards) %>%
  select(-c(starts_with("success"), applications)) %>% 
  pivot_wider(names_from = gender,
              values_from = c(awards,denied)) %>%
  mutate(odds_ratio = (awards_men/denied_men)/(awards_women/denied_women),
         log_odds_ratio = log((awards_men/denied_men)/(awards_women/denied_women)),
         se_log_odds_ratio = sqrt(1/awards_men + 1/denied_men + 
                                  1/awards_women + 1/denied_women)) %>% 
  mutate(norm_log_odds = log_odds_ratio/se_log_odds_ratio) %>% 
  select(-c(ends_with("en"))) %>% 
  pull(norm_log_odds) %>% 
  qqnorm(main="Normal Q-Q Plot Norm Log Odds") 
  
  
funding_data %>% 
  select(-c(starts_with("succ"))) %>% 
  group_by(gender) %>% 
  summarize(applications = sum(applications),
            awarded = sum(awarded), 
            denied = sum(denied)) %>% 
  select(awarded,denied) %>% 
  chisq.test()


  mutate(awarded = round(awarded/applications*100,1),
         denied = round(denied/applications*100,1)) %>% 
  select(awarded,denied) %>% 
  chisq.test()

  