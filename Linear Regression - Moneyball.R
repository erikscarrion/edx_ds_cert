
## Linear Regression: Moneyball Case Study ====

## Load Dependencies ====
library(Lahman)
library(dslabs)
library(HistData)
library(tidyverse)
library(broom)
library(GGally)
library(mvnormtest)

## Load Teams Data ====
data("Teams")

## We need to be careful of spurious relationships. 
## Just because 2 variables are highly correlated  
## doesn't imply causation in any direction.  
## 
## For example, let's look at the relationship between
## home runs and walks on a per game basis

## First, we'll convert the stats we'll be looking at
## to a per game basis. 

stats <- c( "W", "R", "AB", "H", 
            "X1B", "X2B", "X3B", "HR", 
            "BB", "SO", "SB", "E")


## Per-Opportunity Function ====
## 
## Since we're applying a custom transformation to a subset of columns, we'll write a generic 
## helper function to reframe team|player statistics on a per-opportunity basis
##  For teams  : each game represents an opportunity 
##  For players: each plate appearance represents an opportunity


per.opp <- function(stat, opp){
  ifelse(opp > 0, stat/opp, 0)
  }

## Format teams data ====

baseball <-  
  Teams |> 
  filter(yearID %in% 1961:2002) |>
  mutate(X1B = (H-X2B-X3B-HR)) |> 
  ## Apply the helper function across the columns we specified above 
  ##   + passing a named list to .fns creates new columns 
  ##     instead of modifying in place
  mutate(across(all_of(stats),
                ~ per.opp(.x, G), 
                .names = "{.col}_pg")) |> 
  # Retain yearID, original & transformed stats
  select(c(1, 7, 15:17, "X1B", 18:24, "E", all_of(ends_with("pg"))))

## Association b/t walks and hrs
baseball |> 
  ggplot(aes(BB_pg, HR_pg)) + 
  geom_point(shape = 21, 
             fill = 'tomato',
             alpha = 0.85,
             size = 3.5) + 
  geom_smooth(method = "lm") +
  labs(title = "Do walks cause homeruns?",
       subtitle = "It might seem so.",
       caption = "Data: Lahman::Teams",
       x = "BB (per game)",
       y = "HR (per game)") + 
  theme_bw() + 
  theme(plot.caption.position = "plot")

## It's unlikely that more walks lead to more home runs
## so there's likely to be an underlying factor connecting
## the 2.
## 
## We can explore whether this is the case by looking at the
## relationships between the other variables.  

baseball |>
  select(!c(yearID,G, CS,all_of(ends_with("pg")))) |> 
  cor()

bb.hr.slope = cor(baseball$HR_pg, baseball$BB_pg)*(sd(baseball$HR_pg)/sd(baseball$BB_pg))
glue::glue("Correlation b/t Walks and Homeruns: {round(cor(baseball$HR_pg, baseball$BB_pg),4)}
            Slope Homeruns ~ Walks: {round(bb.hr.slope,4)}")

## Numerically, home runs and walks have a correlation coefficient of .40 which explains the 
## information in the scatter plot. 
## 
## The correlation isn't enough to justify the conclusion of a cause-effect relationship, though, so
## we need to investigate the other factors related to Wins. 

baseball |>
  ggplot(aes(HR_pg, R_pg)) + 
  geom_point(shape = 21, 
             fill = 'tomato',
             alpha = 0.85,
             size = 3.5) + 
  geom_smooth(method = "lm") +
  labs(title = "What is the relationship between\nHome Runs and Runs?",
       subtitle = NULL,
       caption = "Data: Lahman::Teams",
       x = "HR (per game)",
       y = "R (per game)") + 
  theme_bw() + 
  theme(plot.caption.position = "plot")

## Pair-wise Scatter Plot ====
## A pair-wise scatter plot is an efficient method of representing & analyzing multiple pair-wise
## relationships. 
##   + GGally::ggpairs() is very helpful for this.

baseball |>
  ## be careful to select a small subset of variables
  ## by default, you're capped to 15 vars with ggpairs.
  ## you can change that by setting the cardinality_threshold 
  select(all_of(ends_with("pg"))) |>
  ggpairs(title = "Pairwise relationships\nof per game statistics",
          cardinality_threshold = 15, 
          lower = list(continuous = wrap("points", 
                                         shape = 21, 
                                         fill = "tomato",
                                         alpha = .95)))
# Win Rate versus errors
baseball |> 
  ggplot(aes(E_pg, W_pg)) + 
  geom_point(shape = 21, 
             fill = 'tomato',
             alpha = 0.85,
             size = 3.5) + 
  geom_smooth(method = "lm") +
  labs(title = "What is the relationship between\nErrors and Winning Percentage?",
       subtitle = NULL,
       caption = "Data: Lahman::Teams",
       x = "Errors (per game)",
       y = "Win %") + 
  theme_bw() + 
  theme(plot.caption.position = "plot")

## Stratification is a useful tool for teasing apart the variances which define the relationships
## you're exploring. 
## 
## By stratifying, we can ask and aswer the question: Is the coefficient estimate for walks the
## same across each of the levels?


## Confounding - Exploring BB's vs HR's ====

## Analytic Inspection - compare mean BB, 1B, 2B, 3B, & HR across stratas
baseball |>
  mutate(strata = round(HR_pg, 1)) |> 
  filter(strata >= 0.4 & strata <= 1.2) |>
  group_by(strata) |>
  summarize(across(all_of(ends_with("pg")),
                   ~ mean(.x, na.rm = T),
                   .names = "avg.{.col}"))
            
## Plot: Walks/G vs. Runs/G by strata ====
baseball |>
  mutate(strata = round(HR_pg, 1)) |> 
  filter(strata >= 0.4 & strata <= 1.2) |>
  ggplot(aes(BB_pg, R_pg)) + 
  geom_point(shape = 21, 
             fill = 'tomato',
             alpha = 0.85,
             size = 3.5) + 
  geom_smooth(method = "lm") +
  facet_wrap(~strata)


## Plot: Base Production by Home Run Strata ====
baseball |>
  mutate(strata = round(HR_pg, 1)) |> 
  filter(strata >= 0.4 & strata <= 1.2) |>
  group_by(strata) |>
  summarize(runs = mean(R_pg),
            walks = mean(BB_pg),
            singles = mean(X1B_pg),
            doubles = mean(X2B_pg),
            triples = mean(X3B_pg),
            homeruns = mean(HR_pg)) |>
  ungroup() |>
  pivot_longer(cols = -strata,
               names_to = 'category',
               values_to = 'value') |>  
  mutate(strata = factor(strata),
         category = factor(category, 
                           levels = c("runs", "walks", "singles", "doubles", "triples", "homeruns"),
                           ordered = T)) |>
  ggplot() + 
  geom_bar(aes(x = category, y = value, fill = strata),
           color = "black",
           stat = 'identity',
           position = "dodge") +
  labs(title = "Base Production by Home Run Strata",
       caption = "Data: Lahman::Teams",
       fill = "HR Strata",
       y = "Mean Value",
       x = "") +
  theme_bw() +
  theme(plot.caption.position = "plot")
  
## Observations:  
##  Upper Strata Teams:
##    - Score 39.66% more runs than teams in the lower stratas
##    - Get more walks, doubles, and home runs than teams in the lower stratas
##    
##  - These differences create a distinction between low power and high power teams. 
##      + High power teams, on an average day, can be expected to put more of their players in 
##          scoring position than low power teams. 
##      + When combined with a higher rate of doubles and a higher rate of home runs, high power
##          teams will be more likely to drive in the players they put in scoring position.
## 


## A Linear Model for Runs per Game ====

## What does a model look like for walks and home runs individually?
tidy(lm(R_pg ~ BB_pg, data = baseball))
tidy(lm(R_pg ~ HR_pg, data = baseball))

## What does a model with both walks and home runs look like?
tidy(lm(R_pg ~ BB_pg + HR_pg, data = baseball))

## Observation: 
##  - The coefficient estimates for walks and home runs are both smaller in the 
##    larger model yet retain their significance. 
##  - The coefficient estimate for walks dropped 48%
##  - The coefficient estimate for home runs dropped 15%

## How do the coefficient estimates for walks and home runs change when we take 
## the presence of other variables into account?
## 
## Full Model: ====
##    R = Walks + Singles + Doubles + Triples + HomeRuns

fit <- 
  baseball |> 
  select(all_of(ends_with("pg")))|>
  rename(R = R_pg, 
         BB = BB_pg,
         X1B = X1B_pg,
         X2B = X2B_pg, 
         X3B = X3B_pg,
         HR  = HR_pg) |> 
  {\(x){lm(R ~ BB + X1B + X2B + X3B + HR, data = x)}}()
  

tidy(fit)

## The estimates for HR's and Walks were compressed further by 7% and 4%, respectively.
## The addition of the other variables does not diminish the significance of either. 
## This tells us two things:
##    1. The other variables definitely belong in the model
##    2. The model might benefit from the addition of other variables. 
## 
## So far we've been dealing with team level statistics. These numbers represent the accumulated
## performance of each team's roster. If we want to add a player-centric variable to the equation,
## it has to be expressed in comparable fashion to the team-centric per game stats. 
## 
## Where a game is an opportunity for a team to win, we can think of a plate appearance as an 
## opportunity for a hitter to `win` & increase their team's probability of scoring by 
## not being struck out or put out. 
## 
## We'll express hitter performance in terms of plate appearances, selecting only those
## players with more than 100 plate appearances. 
## 
## Playing time between players is highly variable. Some players are day-to-day while others are
## purely situational. In order to equate per-plate-appearance performance to per-game performance, 
## we'll take a player's total number of plate appearances and divide by the overall average number
## of plate appearances per game to get the player's per-game statistics.


## Batting Data ====

## We'll be using 1997-2001 to predict for 2002
batting <-
  Batting |>
  filter(yearID %in% 1997:2002) |>
  mutate(PA = BB + AB,
         X1B = H-X2B-X3B-HR) |>
  select(playerID, yearID, teamID, G, AB, PA, R, H, BB, X1B, X2B, X3B, HR, RBI) |> 
  ## calculate the per plate appearance statistics
  mutate(across(-c(playerID, yearID, teamID, G, PA),
                ~ per.opp(.x, PA),
                .names = "{.col}_ppa"))

batting.01 <- batting |> filter(yearID != 2002)
batting.02 <- batting |> filter(yearID == 2002)
## Average # of plate appearances per game:
##    Note: We're using 2002 numbers to get the average number of plate appearances
##          for each player. IRL, if 2002 is the year we're interested in predicting, 
##          we won't have access to that information.  Instead, we would use the average
##          from 1997-2001. 
##          
        
## Mean PA 1997:2001
mean_pa_pg <- 
  batting.01 |>
  group_by(teamID, yearID) |>
  summarize(pa_pg = sum(PA)/max(G)) |>
  pull(pa_pg) |>
  mean()

mean_pa_pg # 39.05556

# Equate stat per PA to stat per Game for 01
players_pg <-  
  batting.01 |> 
  group_by(playerID) |>
  summarize(PA = sum(PA),
            G = sum(PA)/mean_pa_pg,
            AVG = sum(H)/sum(AB),
            across(c(R, BB, X1B, X2B, X3B, HR), 
                   ~ ifelse(G > 0, sum(.x)/G, 0))) |>
  filter(PA >= 1000)

players_pg.02 <-
  batting.02 |>
  group_by(playerID) |>
  summarize(PA = sum(PA),
            G = sum(PA)/mean_pa_pg,
            AVG = sum(H)/sum(AB),
            across(c(R, BB, X1B, X2B, X3B, HR), 
                   ~ ifelse(G > 0, sum(.x)/G, 0))) |>
  filter(PA > 100)

## Make sure that we only have players that are represented in both datasets

players_pg <- players_pg |> semi_join(players_pg.02, by = "playerID")
players_pg.02 <- players_pg.02 |> semi_join(players_pg, by = "playerID")

## compare what we would expect using prior year's data to 
## what we actually saw 

players_combined <- 
  players_pg |>
  select(playerID, R, BB, X1B, X2B, X3B, HR) |>
  rename(R.01 = R) |>
  left_join(players_pg.02 |> 
               select(playerID, R) |>
               rename(R.02 = R)  , 
             by = "playerID")


r.hat <- predict(fit, newdata = players_pg)
r.actual <- players_pg.02 |> select(playerID, R)
## How well did the model predict player Runs/Game?
tibble(r.actual) |>
  mutate(Rhat = r.hat)|>
  ggplot(aes(R, Rhat)) + 
  geom_point(shape = 21,
             fill = "tomato",
             size = 2.5) + 
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Predicted R/G vs. Actual",
       caption = "Data: Lahman::Teams, Batting") +
  theme_bw() +
  theme(plot.caption.position = "plot")

  
## Did the average performance do better?
tibble(avg = players_pg$R, actual = players_pg.02$R) |>
  ggplot(aes(avg, actual)) + 
  geom_point(shape = 21,
             fill = "tomato",
             size = 2.5) + 
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Mean R/G vs. 2002 Actual",
       caption = "Data: Lahman::Teams, Batting") +
  theme_bw() +
  theme(plot.caption.position = "plot")


## When compared to a team level analysis, our predictions aren't 
## nearly as good. We can see this in the variability of R/G
players_pg.02 |>
  ggplot(aes(R)) + 
  geom_histogram(fill = "orange", col =  "black", bins = 15) +
  labs(title = "Distribution of Runs per Game",
       subtitle = "2022 Actuals",
       caption = "Data: Lahman::Teams, Batting",
       x = "Runs (per game)") +
  theme_bw() +
  theme(plot.caption.position = "plot")

players_pg |>
  ggplot(aes(Rhat)) + 
  geom_histogram(fill = "orange", col =  "black", bins = 15) +
  labs(title = "Distribution of Runs per Game",
       subtitle = "2022 Predicted",
       caption = "Data: Lahman::Teams, Batting",
       x = "Runs (per game)") +
  theme_bw() +
  theme(plot.caption.position = "plot")

## Pairwise Plot: Stat/PA ==== 
batting |> 
  select(all_of(ends_with("ppa"))) |> 
  ggpairs(lower = list(continuous = wrap("points",
                                         shape = 21,
                                         fill = "#7CAE00",
                                         alpha = 0.90)),
          title = "Per Plate Appearance Statistics") +
  theme_bw()


## How Do the player averages for the years prior to 2002 
## correlate with performance in 2002?
players_pg |>
  right_join(players_pg.02, by = "playerID") |>
  select(-c(playerID, PA.x, PA.y, AVG.x, AVG.y, G.x, G.y)) |>
  cor()

## Runs, Walks, Singles, and Home runs exhibit high levels of 
## correlation while the the relationship is weaker for doubles 
## and triples. 

cor(players_pg |> 
      select(-c(playerID, PA, G)), 
    players_pg.02 |> 
      select(-c(playerID, PA, G)))


## Coming back to walks at various levels of home runs, 
## we want to test whether the slopes for the same at 
## every level of stratification


## a helper function to compute retrieve the model coefficient 
## for walks at each level of home run strata. 

get_slope <- function(x,y){
  fit <- lm(y~x)
  tibble(slope = fit$coefficients[2],
         se = summary(fit)$coefficient[2,2])
}

## Average Runs, Walks, and Home Runs
baseball |>
  mutate(strata = round(HR_pg, 1)) |>
  select(strata, R_pg, BB_pg, HR_pg) |>
  filter(strata >= 0.4 & strata <= 1.2) |>
  rename(runs = R_pg, 
         walks = BB_pg,
         homeruns = HR_pg) |>
  group_by(strata) |>
  summarize(across(c(runs, walks, homeruns), ~mean(.x)))

homeruns.slope <- 
  baseball |>
  mutate(strata = round(HR_pg, 1)) |>
  select(strata, R_pg, BB_pg, HR_pg) |>
  filter(strata >= 0.4 & strata <= 1.2) |>
  rename(runs = R_pg, 
         walks = BB_pg,
         homeruns = HR_pg) |>
  group_by(strata) |>
  summarize(get_slope(homeruns, runs)) |>
  select(slope,se)

walks.slope <- 
  baseball |>
  mutate(strata = round(HR_pg, 1)) |>
  select(strata, R_pg, BB_pg, HR_pg) |>
  filter(strata >= 0.4 & strata <= 1.2) |>
  rename(runs = R_pg, 
         walks = BB_pg,
         homeruns = HR_pg) |>
  group_by(strata) |>
  summarize(get_slope(walks, runs)) |>
  select(slope, se)

## Are the slopes at each level significantly different from each other?
baseball |>
  mutate(strata = round(HR_pg, 1)) |>
  select(strata, R_pg, BB_pg, HR_pg) |>
  filter(strata >= 0.4 & strata <= 1.2) |>
  rename(runs = R_pg, 
         walks = BB_pg,
         homeruns = HR_pg) |>
  group_by(strata) |>
  summarize(tidy(lm(runs ~ walks), conf.int = T)) |>
  filter(term == "walks") |>
  select(strata, estimate, conf.low, conf.high) |>
  ggplot(aes(strata, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point()+
  geom_errorbar() +
  labs(title = "Coefficient estimate for walks/game",
       subtitle = "Stratified by Home Run Strata",
       caption = "Data: Lahman::Teams") +
  theme_bw()+
  theme(plot.caption.position = "plot")

## Every confidence interval overlaps, so we can't say they're different from each other. 

## Doing the same for homeruns:
baseball |>
  mutate(strata = round(HR_pg, 1)) |>
  select(strata, R_pg, BB_pg, HR_pg) |>
  filter(strata >= 0.4 & strata <= 1.2) |>
  rename(runs = R_pg, 
         walks = BB_pg,
         homeruns = HR_pg) |>
  group_by(strata) |>
  summarize(tidy(lm(runs ~ homeruns), conf.int = T)) |>
  filter(term == "homeruns") |>
  select(strata, estimate, conf.low, conf.high) |>
  ggplot(aes(strata, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point()+
  geom_errorbar() +
  labs(title = "Coefficient estimate for HRs/game",
       subtitle = "Stratified by Home Run Strata",
       caption = "Data: Lahman::Teams") +
  theme_bw()+
  theme(plot.caption.position = "plot")


## Building a team requires that we account for salaries and defensive position
## to do this, we'll be joining a couple of tables. First we pull salaries from the salary tables
## then we add defensive position by taking the position the player played most 

players <-
  players_pg.02 |>
  left_join(Salaries |> 
              filter(yearID == 2002) |> 
              select(playerID, salary), 
            by  = "playerID")

head(players)

position_names <- paste0("G_", c("p","c","1b","2b","3b","ss","lf","cf","rf", "dh"))

# Retrieve defensive position
appearances <- 
  Appearances |> 
  filter(yearID == 2002) |> 
  group_by(playerID) |> 
  summarize_at(position_names, sum) |> 
  ungroup()


most.appeared <- 
  appearances |>
  select(-playerID) |>
  {\(x){
    sapply(1:nrow(x), \(i){which.max(x[i,])})
    }}() |>
  unname()
  
pos <- 
  appearances |> 
  select(all_of(position_names)) |> 
  apply(X = ., 1, which.max)

## both methods give the same results
identical(pos, most.appeared)


players <- 
  tibble(playerID = appearances$playerID,
         POS = position_names[pos]) |> 
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) |> 
  filter(POS != "P") |> 
  right_join(players, by = 'playerID') |> 
  filter(!is.na(POS) & !is.na(salary))

## Add their first and last names
players <- 
  People |> 
  select(playerID, nameFirst, nameLast, debut) |> 
  rename(First = nameFirst,
         Last  = nameLast) |> 
  right_join(players, by = "playerID")

players <-
  players |> left_join(players_pg |> select(playerID, Rhat), by = "playerID")

## Who are the top 10 players by their
## predicted number of runs?
players |> 
  arrange(desc(Rhat)) |> 
  select(First ,Last ,salary, Rhat) |> 
  top_n(10)

## Relationship between predicted runs and salary
players |> 
  ggplot(aes(log(salary, base = 10), Rhat, fill = POS)) + 
  geom_point(shape = 21, size = 5, alpha = 0.95) +
  labs(title = "Salary vs. Predicted Runs per Game",
       subtitle = "Is there a linear relationship?",
       x = "Salary (log 10)",
       caption = "Data: Lahman::Teams, batting") +
  theme_bw() 

  

## remake the plot filtering out players who debuted after 1998
players |> 
  filter(debut < "1998-01-01") |> 
  ggplot(aes(log(salary, base = 10), Rhat, fill = POS)) + 
  geom_point(shape = 21, size = 5, alpha = 0.95) +
  labs(title = "Salary vs. Predicted Runs per Game",
       subtitle = "Is there a linear relationship?",
       x = "Salary (log 10)",
       caption = "Data: Lahman::Teams, batting") +
  theme_bw() 


## How to choose players: Linear Programming
library(reshape2)
library(lpSolve)

# filter players with a debut between 1988 and 1997
players_97 <- 
  players |> 
  filter(debut > "1988-01-01" & debut <= "1997-01-01")

# Constraint matrix
constraint_matrix <- 
  reshape2::acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1,npos), 50*(10^6))
lp_solution <- lp("max", players$Rhat,
                  constraint_matrix,
                  constraint_dir,
                  constraint_limit,
                  all.int = T)

our_team <- 
  players |> 
  filter(lp_solution$solution == 1) |> 
  arrange(desc(Rhat))

our_team |> 
  select(playerID, First, Last, POS, salary, Rhat)


## Investigating the sophomore slump 
## We need to pull data from their ROY year and their sophomore year
## Exclude players who didn't have a sophomore year

awards <- 
  AwardsPlayers |> 
  filter(awardID == "Rookie of the Year",
         yearID %in% 1997:2001) |> 
  select(playerID, yearID, lgID, awardID)
awards

## Get player information
playerInfo <- 
  Fielding |> 
  group_by(playerID) |> 
  arrange(desc(G)) |> 
  slice(1) |> 
  ungroup() |> 
  left_join(People, by = "playerID") |> 
  select(playerID, nameFirst, nameLast, POS)

head(playerInfo)

ROY <- 
  AwardsPlayers |> 
  filter(awardID == "Rookie of the Year") |> 
  left_join(playerInfo, by = "playerID") |> 
  rename(rookie_year = yearID) |> 
  right_join(Batting, by = "playerID") |> 
  mutate(AVG = H/AB) |> 
  filter(POS != "P")

head(ROY)

# Keep only rookie and sophomore season
ROY.1 <- 
  ROY |> 
  filter(yearID == rookie_year | yearID == rookie_year+1) |> 
  group_by(playerID) |> 
  mutate(rookie = ifelse(yearID == rookie_year, "rookie", "sophomore")) |> 
  filter(n()==2) |> 
  ungroup() |> 
  select(playerID, nameFirst,nameLast, rookie_year,rookie, AVG)
head(ROY.1)

roy.1 <- 
  ROY.1 |> 
  spread(rookie, AVG) |> 
  arrange(desc(rookie))
head(roy.1)

mean(roy.1$sophomore-roy.1$rookie <0)

## Visualize the distribution of batting average
## for rookie & sophomore year
ROY.1 |> 
  mutate(rookie = ifelse(rookie == "rookie", "Rookie Year", "Sophomore Year")) |> 
  ggplot(aes(AVG)) + 
  geom_histogram(color = "black", fill = "orange", bins = 25, position = "dodge") +
  labs(title = "Distribution of Batting Avg",
       subtitle = "Sophomore vs. Rookie Year",
       caption = "Data: R::Lahman Package",
       fill = "Season") +
  facet_grid(cols = vars(rookie))
  


## Assessment: Regression & Baseball ====

## Fit a model a simple 2 variable model to 
## data from 1961:2018
base.fit <- 
  baseball |> 
  filter(yearID %in% 1961:2018) |> 
  {\(x){lm(R_pg ~ BB_pg + HR_pg, data = x)}}() 
   
tidy(base.fit)

## Trend Effect: Walks over Time ====

## What's the trend for effect of BB over time? 
bb.1 <- 
  baseball |> 
  filter(yearID %in% 1961:2018) |> 
  group_by(yearID) |> 
  summarize(tidy(lm(R_pg ~ BB_pg + HR_pg), conf.int=T)) |> 
  filter(term=="BB_pg") |>
  mutate(model = "Model: R ~ BB")

bb.2 <- 
  baseball |> 
  filter(yearID %in% 1961:2018) |> 
  group_by(yearID) |> 
  summarize(tidy(lm(R~BB+HR), conf.int=T)) |> 
  filter(term == "BB") |>
  mutate(model = "Model: R~ BB + HR")

bb <- bind_rows(bb.1, bb.2)


bb |>
  ggplot(aes(yearID, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar() +
  geom_smooth(method = "loess") + 
  labs(title = "Effect of Walks over Time",
       subtitle = "For the 1 & 2 Variable Models",
       caption = "Data: R::Lahman Package") + 
  facet_grid(cols = vars(model))+
  theme_bw() +
  theme(plot.caption.position = "plot")

## Time is a factor. When looking at our estimate of walks
## as a function of time, time has a p-value < .01
tidy(lm(estimate ~ yearID, data = bb))

## Predicting Attendance ====
attendance <-  
  Teams |> 
  filter(yearID %in% 1961:2002) |> 
  mutate(X1B = H - X2B - X3B - HR,
         # Define Win Strata with
         # interval width of 10 hr
         win_strata = round(W/10)) |>
  # filter our sparsely populated strata
  filter(!(win_strata<5|win_strata>10)) |>
  # express on per-game basis
  mutate(across(c(attendance, all_of(stats)), ~ per.opp(.x, G))) |>
  select(all_of(keep)) 

head(attendance)

# Can Runs per game predict average attendance?
attendance.fit <- 
  attendance |> 
{\(x){lm(attendance ~ R, data = x)}}() 

tidy(attendance.fit)

hr.fit <- 
  attendance |> 
  {\(x){lm(attendance ~ HR, data = x)}}()

tidy(hr.fit)


# Use number of wins to predict avg_attendance
wins.fit <- lm(attendance ~ W, data = attendance)

tidy(wins.fit)


# use year to predict attendance
year.fit <-  lm(attendance ~ yearID, data = attendance)
tidy(year.fit)

# what's the correlation coefficient for runs and wins?
cor(attendance$R, attendance$W)

# what's the correlation between HR and wins?
cor(attendance$HR, attendance$W)

## Attendance - Stratified by Win Strata ====

## Estimates of the intercept for each strata 
## attendance ~ homeruns
attendance |> 
  group_by(win_strata) |> 
  summarize(tidy(lm(attendance~HR), conf.int=T)) |> 
  filter(term == "(Intercept)") |> 
  select(win_strata, estimate, statistic, p.value)

## The estimates for the intercept are significant for all but 
## the lowest win_strata

attendance |> 
  group_by(win_strata) |> 
  summarize(tidy(lm(avg_attendance~homeruns), conf.int=T)) |> 
  filter(term == "homeruns") |> 
  select(win_strata, estimate, statistic, p.value)

## Estimates for homeruns are significant for every level of win_strata

## Control for number of wins by including it in the model.
model_0 <- 
  attendance |> 
  group_by(win_strata) |> 
  summarize(tidy(lm(attendance ~ W + HR), conf.int=T)) |> 
  filter(term != "(Intercept)") |> 
  select(win_strata, term, estimate, statistic, p.value)

## At higher win strata, 

## Attendance: Keep Columns ====
keep <- c("yearID", "teamID", "G", "W", "L", "R", "AB", "H", 
          "BB", "X1B", "X2B", "X3B", "HR", "SO", "SB", "E", 
          "attendance", "win_strata")

attendance_5 <- 
  attendance |> 
  filter(win_strata == 5) |> 
  arrange(desc(HR))

## Attendance = W & HR ====

## Model attendance as a function of wins and homeruns 
##    Model 1: attendance ~ W
##    Model 2: attendance ~ HR
##    Model 3: attendance ~ W + HR  
model_1 <- 
  attendance_5 |>
  select(avg_attendance, win_strata, R, W, X1B, X2B, X3B, HR) |>
  {\(x){
    lm(avg_attendance ~ W, data = x)
  }}()

model_2 <- 
  attendance_5 |>
  select(avg_attendance, win_strata, R, W, X1B, X2B, X3B, HR) |>
  {\(x){
    lm(avg_attendance ~ HR, data = x)
  }}()

model_3 <- 
  attendance_5 |>
  select(avg_attendance, win_strata, R, W, X1B, X2B, X3B, HR) |>
  {\(x){
    lm(avg_attendance ~ W+HR, data = x)
  }}()

## What are the coefficient estimates for each of the 3 models?
tidy(model_1);tidy(model_2);tidy(model_3);


## Attendance = Runs ====
##    - For strata 5
## Do the number of runs scored matter?
##    model 4: attendance ~ R
##    model 5: attendance ~ R + W
##    model 6: attendance ~ R + HR
##    model 7: attendance ~ R + W + HR
model_4 <- 
  attendance_5 |>
  select(avg_attendance, win_strata, R, W, X1B, X2B, X3B, HR) |>
  {\(x){
    lm(avg_attendance ~ R, data = x)
  }}()

model_5 <- 
  attendance_5 |>
  select(avg_attendance, win_strata, R, W, X1B, X2B, X3B, HR) |>
  {\(x){
    lm(avg_attendance ~ R + W, data = x)
  }}()

model_6 <- 
  attendance_5 |>
  select(avg_attendance, win_strata, R, W, X1B, X2B, X3B, HR) |>
  {\(x){
    lm(avg_attendance ~ R + HR, data = x)
  }}()

model_7 <- 
  attendance_5 |>
  select(avg_attendance, win_strata, R, W, X1B, X2B, X3B, HR) |>
  {\(x){
    lm(avg_attendance ~ R + W + HR, data = x)
  }}()

## What are the coefficient estimates for each of the 3 models?
tidy(model_4);tidy(model_5);tidy(model_6);tidy(model_7)

## In the presence of Runs, HR & W are insignificant.

## Attendance : HR x win_strata ====

## How do HR's relate to attendance at every win strata?
attendance |>
  ggplot(aes(HR, attendance)) +
  geom_point(shape = 21,
             fill = "tomato",
             color = "black",
             size = 2.5,
             alpha = 0.8) +
  geom_smooth(method="lm") +
  facet_wrap(~win_strata) +
  labs(title = "Attendance Vs. Homeruns",
       subtitle = "Stratified by Wins", 
       caption = "Data: R::Lahman Package",
       y = "Average Attendance",
       x = "Homeruns") +
  theme_bw()+
  theme(plot.caption.position = "plot")


## Attendance = year ====

## Does time play a factor in attendance?

## Attendance Trend for the NY Mets
attendance |>
  filter(teamID == "NYN") |>
  ggplot(aes(yearID, attendance)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  labs(title = "Attendance Over Time - Mets",
       subtitle = "Is there a time effect?",
       caption = "Data: R::Lahman Package") +
  theme_bw() +
  theme(plot.caption.position = "plot")

## Attendance Trend for the BAL Orioles
attendance |>
  filter(teamID == "BAL") |>
  ggplot(aes(yearID, attendance)) +
  geom_point() +
  geom_smooth(span = 0.5) +
  labs(title = "Attendance Over Time - Orioles",
       subtitle = "Is there a time effect?",
       caption = "Data: R::Lahman Package") +
  theme_bw() +
  theme(plot.caption.position = "plot")

## There is a time effect for each team, but the shapes of their trends are incredibly distinct
## which would suggest high variability in the distribution of time effects. A model that only
## considers time isn't going to be enough. We need additional factors.

## Attendance = Year ====
model_8 <- 
  attendance |>
  select(attendance, win_strata, yearID) |>
  {\(x){
    lm(attendance ~ yearID, data = x)
  }}()
## Attendance = Year + Runs/G ====

## How does the coefficient for time change in the 
## presence of Runs?
##    estimate yearID alone: 243 & se = 10.5
##    estimate yearID w/ HR: 189 & se = 11.5
model_9 <- 
  attendance |>
  filter(yearID < 2002)|>
  select(attendance, win_strata, R, yearID) |>
  {\(x){
    lm(attendance ~ R + yearID, data = x)
  }}()

tidy(model_4); tidy(model_8); tidy(model_9)

## Predicted vs. Actual Attendance 2002 ====

att.2001 <-
  attendance |> 
  filter(yearID == 2001) |>
  select(teamID, yearID, R)

att.2002 <- 
  attendance |>
  select(yearID, teamID, attendance) |>
  filter(yearID == 2002 & teamID != "SEA") |>
  mutate(predicted = predict(model_9, newdata = att.2001|>filter(teamID != "SEA"))) 

## plot predicted attendance against actual attendance 
att.2002 |>
  ggplot(aes(attendance, predicted, label = teamID)) +
  geom_abline() +
  geom_text(position = "jitter") +
  labs(title = "2002 predicted attendance vs. actual",
       subtitle = "All Teams",
       caption = "Data: R::Lahman Package",
       x = "Predicted Attendance",
       y = "Actual Attendance") +
  theme_bw()+
  theme(plot.caption.position = "plot")

## What's the RMSE of the model taking win strata into account?
rmse_strata <-   
  att.2002 |>
  summarize(RMSE = sqrt(sum(((attendance-predicted)^2)/n())))

## Attendance - Model w/o Win Strata ====
## Does a model which disregards win strata fare better?
attendance_model <- 
  Teams |>
  filter(yearID <= 2001) |>
  mutate(runs = R/G,
         attendance = attendance/G,
         year = yearID) |>
  select(attendance, year, runs) |>
  {\(data){
    lm(attendance ~ year + runs, data = data)}
    }()

att.2001.1 <-
  Teams |>
  mutate(X1B = H-X2B-X3B-HR) |> 
  filter(yearID == 2001) |>
  mutate(across(all_of(stats), ~per.opp(.x, G))) |>
  select(teamID, yearID, R) |>
  rename(runs = R, year = yearID)

att.2002.1 <- 
  attendance |> 
  filter(yearID == 2002) |>
  select(teamID, attendance, yearID, R) |>
  mutate(predicted = predict(attendance_model, newdata = att.2001.1))

att.2002.1 |>
  ggplot(aes(attendance, predicted, label = teamID)) +
  geom_text() +
  geom_abline() +
  labs(title = "2002 predicted attendance vs. actual",
       subtitle = "All Teams - No Win Strata Filter",
       caption = "Data: R::Lahman Package",
       x = "Predicted Attendance",
       y = "Actual Attendance") +
  theme_bw()+
  theme(plot.caption.position = "plot")

## What's the RMSE of the model that doesn't account for win_strata?  
rmse_noStrata <-
  Teams |>
  filter(yearID == 2002) |>
  mutate(runs = R/G,
         attendance = attendance/G) |>
  select(teamID, yearID, runs, attendance) |>
  mutate(attendance.pred = predicted_attendance_2002) |>
  summarize(RMSE = sqrt(sum( ((attendance.pred-attendance)^2)/n() ) ))

## Overall, while year and per game run production are significant factors in determining
## attendance, it doesn't tell the whole story. We aren't accounting for other variables which play
## a part in determining attendance. For example:
##    + Average ticket price
##    + Market size 
##    + Concession prices
##    + Local & National economic factors
##    
## Taking into account win_strata doesn't do much for us in terms of RMSE. In fact, a model that
## doesn't account for win_strata performs marginally better than the model that does, resulting 
## in a .33% reduction in RMSE.

glue::glue("RMSE - Strata   Accounted For: {round(rmse_strata,2)}
            RMSE - Strata Unaccounted For: {round(rmse_noStrata,2)}
            Diff. in RMSE = ")

