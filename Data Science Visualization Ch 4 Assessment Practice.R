# Harvard EdX - Titanic Assessment

library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass), 
         Sex = factor(Sex))

str(titanic)
  
# Density plots of Age grouped by sex

titanic %>% ggplot(aes(Age, fill = Sex)) + 
  geom_density(alpha = 0.2)

titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age, y = ..count.., fill = Sex)) + geom_density(alpha = 0.2) +
  labs(title = 'Distribution of Age by Sex')

titanic %>% filter(!is.na(Age)) %>% 
  ggplot(aes(Age, fill = Sex, color = Sex)) + 
  geom_density(alpha = 0.2, bw = 2, position='stack') +
  geom_vline(xintercept = 18)+
  geom_vline(xintercept = 35) +
  labs(title = "Age Density Plot by Sex", subtitle = "Position: Stack, y = Density")

  titanic %>% filter(!is.na(Age)) %>% 
  ggplot(aes(Age, y = ..count..,fill = Sex, color = Sex)) + 
  geom_density(alpha = 0.2, bw = 2, position='stack') +
  labs(title = "Age Density Plot by Sex", subtitle = "Position: Stack, y= Count")

titanic %>% filter(!is.na(Age)) %>% 
  ggplot(aes(Age, y = ..count..,fill = Sex, color = Sex)) + 
  geom_density(alpha = 0.2, bw = 2) +
  labs(title = "Age Density Plot by Sex", subtitle = "y= Count")


binvals <- seq(20,60,10)
plot <- titanic %>% ggplot(aes(x = Age))

for(i in seq(1, length(bins))){
  p <- titanic %>% ggplot(aes(Age)) + geom_histogram(bins = binvals[i]) +
    labs(title = paste0("hist(Age, bins = ", binvals[i], ")")) +
    facet_grid(.~Sex)
  
  print(p)
}
# 40 year males vs 40 year old females
titanic %>% filter(!is.na(Age) & Age == 40) %>% group_by(Sex) %>% summarize(n = n(), prop = n/13)


# Which sex had a greater proportion of people 18-35
titanic %>% filter(!is.na(Age) & Age >= 18 & Age <35) %>% group_by(Sex) %>% summarize(n = n(), prop = n/366)

# Which sex had a greater proportion of people < 17
titanic %>% filter(!is.na(Age) & Age < 17) %>% group_by(Sex) %>% summarize(n = n())

# QQ Plot of Age
params <- titanic %>% filter(!is.na(Age)) %>% summarize(mean = mean(Age), sd = sd(Age))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample = Age)) + geom_qq(dparams = params) + geom_abline(slope = 1, intercept = 0)


# Survival By Sex
titanic %>% ggplot(aes(Survived, fill = Sex)) + geom_bar(position = "dodge")

titanic %>% ggplot(aes(Age, y = ..count.., fill = Survived)) + geom_density(alpha = 0.2)

# Survival by fare

titanic %>% filter(Fare != 0) %>% ggplot(aes(Survived, Fare)) +
  geom_boxplot() + geom_jitter()

titanic %>% filter(Fare != 0) %>% ggplot(aes(Survived, log2(Fare))) +
  geom_boxplot() + geom_point(position = "jitter", color = 'red')

# Survival By Passenger Class

titanic %>% ggplot(aes(Pclass, y = ..count.., fill = Survived)) + 
  geom_bar(position = "dodge") + 
  labs(title = "Survival by Class",
       subtitle = "Position Dodge")

titanic %>% ggplot(aes(Pclass, y = ..count.., fill = Survived)) + 
  geom_bar(position = "fill") +
  labs(title = "Survival Status by Class",
       subtitle = "Position Fill")

titanic %>% ggplot(aes(Survived, y = ..count.., fill = Pclass)) + 
  geom_bar(position = "fill") +
  labs(title = "Survival Stratified by Class",
       subtitle = "Position Fill")


# prop table
class_survival <- as.data.frame.matrix(prop.table(table(titanic$Pclass, titanic$Survived), margin = 1), 
                                       row.names = levels(titanic$Pclass)) %>% 
  rename(Survived = "1", Deceased = "0") %>% mutate(odds = Survived/Deceased, 
                                                    log_odds = log(odds, base = exp(1)))


tot_survival <- as.data.frame.matrix(prop.table(table(titanic$Pclass, titanic$Survived), margin =2),
                                 row.names = levels(titanic$Pclass))

tot_survival

g = as.data.frame(class_survival)

g <- g %>% rename(class = Var1, survived = Var2) %>% arrange(class, survived) 
g

deceased <- g %>% filter(survived == 0)
survived <- g %>% filter(survived == 1)

deceased
survived
odds = survived$Freq/deceased$Freq
odds

g$odds <- odds
g

odds_df <- data.frame(class = c(1,2,3), odds = odds)

survival_tot <- as.data.frame.matrix(prop.table(table(titanic$Pclass, titanic$Survived)),
                                     row.names = levels(titanic$Pclass))

# Survival by Age, Sex, and Class

titanic %>% filter(!is.na(Age)) %>% 
  ggplot(aes(Age, y = ..count.., fill=Survived)) + 
  geom_density(position = "stack", alpha = 0.3) +
  facet_grid(Pclass ~ Sex) + 
  labs(title = "Distribution of Age by Survival",
       subtitle = "Faceted by Class and Sex")
titanic %>% group_by(Pclass, Sex) %>% summarize(n = n())
