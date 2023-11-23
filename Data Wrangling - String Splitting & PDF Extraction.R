# dependencies
library(dslabs)
library(tidyverse)

# Wrangling - String Splitting
filename <-  system.file("extdata/murders.csv", package="dslabs")
lines <- readLines(filename)
lines  %>%  head()

# To split, we can use str_split
data <- lines %>% str_split(",")
cols <- data[[1]]
data <- data[-1]

# convert to dataframe with purr
library(purrr)
map(data, function(y) y[1] %>%head(2))

dat <- tibble(map_chr(data,1),
              map_chr(data,2),
              map_chr(data,3),
              map_chr(data,4),
              map_chr(data,5)) %>% 
  mutate_all(parse_guess) %>% 
  setNames(cols)

# We can do the same thing using more efficient code:
dat.1 <- data %>% 
  transpose() %>% 
  map(~ parse_guess(unlist(.))) %>% 
  setNames(cols) %>% 
  as_tibble()
head(dat.1)

dat.2 <- lines %>% str_split(",", simplify = T)
head(dat.2)  

cols.dat2 <- dat.2[1,]
dat.2 <- dat.2[-1,]
dat.2 <- dat.2 %>% 
  as.data.frame() %>%
  setNames(cols.dat2) %>% 
  mutate_all(parse_guess)
head(dat.2)
str(dat.2)


# Case Study - Extracting data from a pdf
library("pdftools")

# The data is held online at the following url:
url <-"https://www.pnas.org/action/downloadSupplement?doi=10.1073%2Fpnas.1510159112&file=pnas.201510159SI.pdf"

# Create a temp file
temp_file <- tempfile()

# Download the pdf to the temp file
download.file(url,temp_file, mode="wb")

# Extract the text
pdf.text <- pdf_text(temp_file)

# Remove the Temp File
file.remove(temp_file)

# inspect the downloaded file
head(pdf.text)
length(pdf.text)


# Since the data is in the second entry, we'll assign that to the raw_data
raw_data <- pdf.text[2]
raw_data

# set tables
tab <- str_split(raw_data, "\n")
tab <- tab[[1]]
names_1 <- tab[3]
names_2 <- tab[5]

# Extract Column Name Information
names_1 <- names_1 %>% 
  str_trim() %>% 
  str_replace_all(",\\s.", "") %>% 
  str_split("\\s{2,}", simplify = T)

names_2 <- names_2 %>% 
  str_trim() %>% 
  str_split("\\s+", simplify = T)

# Join them together

tmp_names <- str_c(rep(names_1, each = 3), names_2[-1], sep = "_")
col_names <- c(names_2[1], tmp_names) %>% 
  str_to_lower() %>% 
  str_replace_all("\\s", "_")
col_names

# Extract data
funding_rates <- tab[7:16] %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame() %>% 
  setNames(col_names) %>% 
  mutate_at(-1, parse_number)

funding_rates <- as_tibble(funding_rates)


# Assessment Part 1 - String Processing Part 3

staff <-  c("Mandy, Chris and Laura")
str_split(staff, pattern = ", | and ")
str_split(staff, pattern = ",\\s|\\sand\\s")

schedule <- data.frame(day = c("Monday", "Tuesday"),
                       staff = c(c("Mandy, Chris, and Laura"),
                                 c("Steve, Ruth, and Frank")))
schedule %>% 
  mutate(staff = str_split(staff, ",\\s|\\sand\\s")) %>% 
  unnest(cols = staff)

separate(schedule, staff, into = c("s1", "s2", "s3"), sep = ",") %>% 
  gather(key = s, value = staff, s1:s3)

num <-"19.5"
str_detect(num, "^1\\d*$")
str_detect(num, "1\\d*")
str_detect(num, "^1\\d+\\.\\d?$")
str_detect(num, "[1-9]*\\.5")

# Import Raw Brexit Referendum Polling Data from Wikipedia
library(rvest)

brexit.url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

brexit <- read_html(brexit.url) %>% html_nodes("table")
polls <- brexit[[6]] %>% html_table(fill = T)

# Update polls to change col names to 
brexit_cols <- names(polls)
brexit_cols <- brexit_cols %>% str_to_lower()
names(polls) <- brexit_cols
polls <- polls %>% rename(dates = "date(s) conducted",
                          poll_type = "polling type",
                          pollster = "conducted by")
poll_filter <- str_detect(polls$remain, "\\d+%$")
polls <- polls[poll_filter,]
dim(polls)

# Extract the end day and month from dates
# We have 1 or 2 digits followed by at least one space
# followed by three letter month

dpat1 <- "\\d+\\s[a-zA-Z]+"
dpat2 <- "[0-9]+\\s[a-zA-Z]+"
dpat3 <- "\\d{1,2}\\s[a-zA-Z]+"
dpat4 <- "\\d+\\s[a-zA-Z]{3,5}"

temp1 <- str_extract_all(polls$dates, dpat1)
temp2 <- str_extract_all(polls$dates, dpat2)
temp3 <- str_extract_all(polls$dates, dpat3)
temp4 <- str_extract_all(polls$dates, dpat4)

end1 <- sapply(temp1, function(x) x[length(x)])
end2 <- sapply(temp2, function(x) x[length(x)])
end3 <- sapply(temp3, function(x) x[length(x)])
end4 <- sapply(temp4, function(x) x[length(x)])