## Comprehensive Assessment:
##    Puerto Rico & Hurricane Maria

## load dependencies
library(dslabs)
library(tidyverse)
library(pdftools)
options(digits=3)

## Pull mortality rate data from dslabs
file <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

# View the file
system("cmd.exe", input=paste("start", file))

# load the pdf up using pdftools's pdf_text() function
txt <- pdf_text(file)

# extract the 9th page of the pdf
x <- txt[9] %>% str_split(pattern = "\n")
x

## assign the first entry of x to s
s <- x[[1]] %>% str_trim()
s[nchar(s)>0]

## Remove the non-numeric data from the table
# Store the header index
pattern = "SEP\\s+2015\\s+"
header_index <- str_which(s, pattern = pattern)

# split the header row into month and column names
header <- s[header_index]
month <- header %>% str_split(pattern = "\\s+", simplify = T)
columns <- header %>% str_split_1(pattern = "\\s+") %>% .[2:5]
header <- c(month, columns)
header
length(header)

# tail index for total row
tail_index <- str_which(s, pattern = "Total")

## Remove entries from s that we don't need: 
## Remove anything before the header index
## Remove lines with only 1 number
## Remove all rows after and including the tail index
s <- s[(header_index+1):(tail_index-1)]
s <- s[!str_detect(s, pattern = "^\\d+$")]

## Now Remove anything that isn't a digit or a space and pull out a matrix of just data
s <- str_remove_all(s, "[^\\d\\s]") 
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
s <- matrix(as.numeric(s), nrow=30,ncol =5)
colnames(s) <- c("day", columns)
tab$month = rep("SEP", 30)

s <- data.frame(s) %>% mutate(month = "SEP")
s <- s %>% rename("2015" = "X2015",
                  "2016" = "X2016",
                  "2017" = "X2017",
                  "2018" = "X2018")
s

# Put the data into tidy format
s <- s %>% gather(year, deaths, -c(day,month)) %>% mutate(deaths = as.numeric(deaths))

# Plot Daily Deaths with color denoting year, excluding 2018

s %>% filter(year != 2018) %>% 
  ggplot(aes(day,deaths,group = year)) +
  geom_line(aes(color =year)) +
  labs(title = "September Daily Deaths in Puerto Rico",
       subtitle = "Years: 2015-2017")
