
## Harvard EdX DS Cert - Course 6 Wrangling
## Section: Dates, Times, and Text Mining

# ++++++++++++++++++++++++++
# Load Dependencies ====
# ++++++++++++++++++++++++++
library(tidyverse)
library(dslabs)
library(scales)
library(tidytext)
library(broom)

# +++++++++++++++++++++++ 
# Load data ====
# +++++++++++++++++++++++
data("trump_tweets")

# ++++++++++++++++++++++++++
# Video 1 ====
# ++++++++++++++++++++++++++

# Load poll results from us elections
data("polls_us_election_2016")

# Inspect the class of start date
class(polls_us_election_2016$startdate)

# sample the start dates
dates <- sample(polls_us_election_2016$startdate, 10)
# inspect to get format: YYYY-MM-DD
dates

# Extract the months
data.frame(date = dates,
           year = year(dates),
           month = month(dates),
           day = day(dates))
# Use the now function to get current date and time for a time zone of your choosing
now(tzone = "Cuba")

# ++++++++++++++++++++++++++++++++
# Text Mining - Trump Tweets ====
# ++++++++++++++++++++++++++++++++

# url for the json file we need
url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'

# Use sprintf (C style string formatter)
# to pull all tweets from 2009-2017
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) |>
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) |>
  filter(!is_retweet & !str_detect(text, '^"')) |>
  mutate(created_at = parse_date_time(created_at, 
                                      orders = "a b! d! H!:M!:S! z!* Y!",
                                      tz="EST")) 

# The above code results in "Couldn't resolve host name error
# Load the tweets using the dslabs package instead

str(trump_tweets)
extract(trump_tweets, "source", "MobileOS") %>% select(created_at,MobileOS) %>% str() 

# we're primarily interested in what happened between the day he announced his candidacy
# and the election day

campaign_tweets <- 
  trump_tweets %>% 
  extract(source,"source","Twitter for (.*)") %>% 
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") &
           created_at < ymd("2016-11-08")) %>% 
  filter(!is_retweet) %>% 
  as_tibble()

# visualize the distribution of tweets over time for iphone and android
campaign_tweets %>% 
  ## extract the hour from created_at
  mutate(hour = with_tz(hour(created_at), "EST")) %>% 
  ## count for each source and hour 
  count(source, hour) %>% 
  ## group_by source and calculate percentage
  group_by(source) %>% mutate(percent = n/sum(n)) %>% 
  ## un-group and pass to ggplot
  ungroup() %>% ggplot(aes(hour, percent, color = source)) +
  geom_line() + 
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribution of tweets by hour of day")


## Text as Data

## looking at tweet 3008
i = 3008

campaign_tweets$text[i] %>% str_wrap(width=65) %>% cat()

# pattern for removing links
links <- "https://t.co/[A-Za-z\\d]+|&amp;"

tweet_words <- 
  campaign_tweets %>% 
  mutate(text = str_replace_all(text,links,"")) %>%
  unnest_tokens(word,text, token = "words")

# What are the most frequently used words
tweet_words %>% 
  count(word) %>% 
  arrange(desc(n))

## Initial inspection shows that a large majority of stop words, 
## so we can filter out the stop words using the tidytext package
tweet_words <- 
  campaign_tweets %>% 
  mutate(text = str_replace_all(text,links,"")) %>%
  unnest_tokens(word,text, token = "words") %>%
  filter(!word %in% stop_words$word)
 
# What are the most frequently used words after filtering
tweet_words %>% count(word) %>% arrange(desc(n))

## Upon further inspection we see that some tokens are just numbers and we don't want those
## and some are quotes which are wrapped in single quotes so we want to pop those out too
tweet_words <- 
  campaign_tweets %>% 
  mutate(text = str_replace_all(text,links,"")) %>%
  unnest_tokens(word,text, token = "words") %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>% 
  mutate(word = str_replace(word, "^'",""))


## Now that we've filtered everything out, we can proceed to analyze differences between
## Android and IOS

tweet_words %>% 
  count(word) %>% 
  top_n(10,n) %>% 
  mutate(word = reorder(word, n)) %>% 
  arrange(desc(n))

## We want to assign each word an odds ratio
os_odds_ratio <- 
  tweet_words %>% 
  count(word,source) %>% 
  pivot_wider(names_from = "source",
              values_from = n,
              values_fill = 0) %>% 
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5) ))

os_odds_ratio %>% filter(Android+iPhone>100) %>%  arrange(desc(or))

## Sentiment Analysis

## get nrc sentiment lexicon
nrc <- get_sentiments("nrc") %>% select(word,sentiment)


## Join tweet words and nrc on word
sentiment_counts <- 
  tweet_words %>% 
  left_join(nrc, by = "word") %>% 
  count(source, sentiment) %>% 
  pivot_wider(names_from = "source", values_from = "n") %>% 
  mutate(sentiment = replace_na(sentiment, replace = "none"))

sentiment_counts

# Compute Log-Odds Ratio

sentiment_counts <- sentiment_counts %>% 
  mutate(LogOddsRatio = log( (Android/(sum(Android)-Android)) /
                            (iPhone/(sum(iPhone)-iPhone))
                          ),
         se = sqrt(1/Android + 1/(sum(Android) - Android) +
                     1/iPhone + 1/(sum(iPhone)-iPhone)),
         conf.low = LogOddsRatio - qnorm(.975)*se,
         conf.hi  = LogOddsRatio + qnorm(.975)*se
         )

sentiment_counts %>% mutate(sentiment = reorder(sentiment,LogOddsRatio)) %>% 
  ggplot(aes(sentiment, ymin = conf.low, ymax = conf.hi)) +
  geom_errorbar() +
  geom_point(aes(sentiment,LogOddsRatio)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip()

## Which words are driving the difference between Android and Iphone?

os_odds_ratio %>% 
  inner_join(nrc,by="word") %>% 
  filter(sentiment == "disgust" & Android+iPhone>10) %>% 
  arrange(desc(or))

## Visualize the contribution of these words
os_odds_ratio %>% inner_join(nrc,by="word") %>%
  mutate(sentiment = factor(sentiment, levels = sentiment_counts$sentiment)) %>% 
  mutate(log_or = log(or)) %>% 
  filter(iPhone + Android > 10 & abs(log_or)>1) %>% 
  ggplot(aes(word, log_or, fill = log_or<0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow=2) +
  geom_bar(stat="identity", show.legend = F) +
  theme(axis.text.x = element_text(angle= 90, hjust = 1))


### Text Mining - Assessment ###

## load brexit polls and count the number of polls conducted in April
data("brexit_polls")


## How many polls were conducted in April?
brexit_polls %>% 
  mutate(start_month = month(startdate)) %>% 
  count(start_month)

## How many polls ended 2016-06-12?
brexit_polls %>% 
  mutate(week_end = round_date(enddate, unit = "week")) %>% 
  count(week_end) %>% 
  filter(week_end == "2016-06-12")

## using weekdays() determine weekday on which each poll ended
brexit_polls %>% mutate(end_day = weekdays(enddate)) %>% 
  count(end_day) %>% arrange(desc(n))

## Load movielens data from dslabs
data("movielens")

## Which year had the most reviews
movielens %>%  
  mutate(revdate = as_datetime(timestamp)) %>% 
  mutate(year = year(revdate)) %>% 
  count(year) %>% arrange(desc(n))

## Which hour had the most reviews
movielens %>%  
  mutate(revdate = as_datetime(timestamp)) %>% 
  mutate(hour = hour(revdate)) %>% 
  count(hour) %>% arrange(desc(n))

## Text Mining and Analysis Assessment
## Dependencies
install.packages("gutenbergr")
library(gutenbergr)
options(digits = 3)


# use str_detect to find the ID of Pride and Prejudice
title = "Pride and Prejudice"
gutenberg_metadata[str_detect(gutenberg_metadata$title, title),"title"]

## We note that there are 6 different id numbers associated with Pride and Prejudice
gutenberg_works(author == "Austen, Jane")

## Use gutenberg_download to download pride and prejudice
book <- gutenberg_download(gutenberg_id = 1342)

## Use unnest tokens to extract the words
words <- book %>% unnest_tokens(word,text)

# filter out stop words and digits
words %>% filter(!word %in% stop_words$word) %>% 
  filter(!str_detect(word, "\\d+")) %>%  str(.)

# what are the most frequent words after accounting for stops and digits
words %>% filter(!word %in% stop_words$word,
                 !str_detect(word, "\\d+")) %>% 
  count(word) %>% filter(n > 100) %>% str(.)


## Question 12 - Sentiment analysis

## define the afinn lexicon
afinn <- get_sentiments("afinn")

## how many elements of words have sentiments in afinn lexicon?
## save this as afinn_sentiments
afinn_sentiments <- words %>% 
  # Filter out stops and digits
  filter(!word %in% stop_words$word,
         !str_detect(word, "\\d+")) %>% 
  # join on afinn sentiments
  inner_join(afinn, by = "word")

## What proportion of values in afinn_sentiment are positive?
mean(afinn_sentiments$value>0)

## How many elements of afinn_sentiments have a value of 4
sum(afinn_sentiments$value==4)
