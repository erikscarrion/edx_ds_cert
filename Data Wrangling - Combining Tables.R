# joining data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)
str(polls_us_election_2016)

# If we try to left join murders onto the US election data, we'd have 
# a problem. But, we can left join polls onto murders

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)
summary(tab)


# electoral votes against population
tab %>% ggplot(aes(population/10^6, electoral_votes)) + 
  geom_point() + 
  geom_text_repel(aes(label = abb)) + 
  scale_x_continuous(trans="log2") +
  scale_y_continuous(trans="log2") +
  labs(title = "Population vs. Electoral Votes",
       subtitle = "US 2016 Election",
       x = "log(Pop. in millions)",
       y = "log(electoral votes)") + 
  geom_smooth(method = 'lm', se=F) + 
  theme_light()


# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2


anti_join(tab2, tab1)


# Set Operators
set1 <- 1:10
set2 <- 6:15
intersect(set1, set2)
union(set1, set2)
setdiff(set1,set2)
setequal(set1,set2)

# Assessment for Combining Tables
# Make a table for state population and a table with states and electoral votes
states <- murders$state[c(1:3, 8:9)]
tab1 <- murders %>% filter(state %in% states) %>% select(state,population)
tab2 <- results_us_election_2016 %>% arrange(state) %>% head(6) %>% select(state,electoral_votes)
tab2


# what is the dim of the tibble created by left_join(tab1,tab2,by='state')

dat <- left_join(tab1, tab2, by = 'state')
dim(dat)
dat

# 2 simple tables
df1 <- data.frame(x = c("a", "b"), y = c("a", "a"))
df2 <- data.frame(x = c("a", "a"), y = c("a", "b"))
union(df1, df2)
intersect(df1, df2)
setdiff(df2,df1)
setdiff(df1,df2)

# Install and load the Lahman library which has datasets related
# to US baseball
library(Lahman)

top <- Batting %>%
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%
  slice(1:10) %>% as.tibble()
top
People %>% as.tibble()

# create a combined table of the names and stats of the top 10 players
top_names <- left_join(top, People, by ="playerID") %>%
  select(playerID, nameFirst,nameLast,HR)

top_names

# Add salary information
top_10 <- Salaries %>% filter(yearID==2016) %>%
  right_join(top_names, by = "playerID") %>%
  select(playerID, nameFirst, nameLast, HR, salary)

# Inspect AwardsPlayers table
AwardsPlayers %>% as.tibble()

# Which of the top 10 HR hitters won at least 1 award?
AwardsPlayers %>% filter(yearID==2016) %>% 
  select(playerID, awardID) %>%
  group_by(playerID) %>%
  summarize(AwardsWon = n()) %>%
  left_join(top_10, by = "playerID")

# use a set operator
awards2016 <- AwardsPlayers %>% filter(yearID==2016)
intersect(awards2016$playerID,top_10$playerID)
