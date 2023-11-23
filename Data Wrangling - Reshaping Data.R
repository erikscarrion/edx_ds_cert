
## Data Wrangling 

## ++++++++++++++++++++++++++
## Load Dependencies ====
## ++++++++++++++++++++++++++
library(tidyverse)
library(dslabs)
path <-system.file("extdata", package = "dslabs")

## ++++++++++++++++++++++++++
## Load Fertility Data ====
## ++++++++++++++++++++++++++

filename <- file.path(path, "fertility-two-countries-example.csv")
fertility_wide <- read_csv(filename)

head(fertility_wide)

## ++++++++++++++++++++++++++++++
## Reshape Fertility Data ====
## ++++++++++++++++++++++++++++++

## This data set contains fertility rates from
## 1960:2015 for Germany and South Korea. 
## 
## In its original form, it's a 2 x 57 table with
## years in the columns and observations for each country
## in the rows. 
## 
## Plotting and analysis of data is best performed when the
## data is tidy which is when every column is a variable and
## every row is a distinct observation.
## 
## By using pivot_longer, we go from a wide, 2 x 57 table, to a long, 
## 112 x 3 table.
##   

fertility_tidy <- 
  fertility_wide |>
  pivot_longer( 
    # pivot everything but the counry column
    -country,
    # Specify the column for column names
    names_to  = "year",
    # Specify the column for measurements
    values_to = "fertility",
    # Format the year column as numeric
    names_transform = list(year=as.numeric)) 
str(fertility_tidy)


## Now that the data is tidy, what do 
## fertility rates look like over time?
fertility_tidy |> 
  ggplot(aes(year, fertility, color=country)) + 
  geom_point() +
  labs(title = "Fertility rates over time",
       subtitle = "For Germany & South Korea",
       caption  = "Dataset: DSLABS::Fertilty - 2 Country Example",
       x = "Year", y = "Fertility") +
  theme_bw()+
  theme(plot.caption.position = "plot")

## ++++++++++++++++++++++++++
## Assessment - CO2 ====
## ++++++++++++++++++++++++++

## Load data
data(co2)
CO2 <- co23

## Data Description:
##    The dataset is a time series object with monthly CO2 concentration
##    measurements (recorded in parts per million) recorded from 1959-1997
##    
##    Note: Concentrations for Feb, March, and April of 1964
##          were interpolated using concentrations recorded
##          in January and May 1964
##    
##    Source: https://scrippsco2.ucsd.edu/data/atmospheric_co2/.

CO2_wide <- 
  # Convert the time series into a matrix of 12 columns
  # going by row, and make a data frame out of it
  data.frame(matrix(CO2, ncol=12, byrow=T)) |> 
  # Name the columns 1-12 
  set_names(1:12) |>
  # Create a year column
  mutate(year = as.character(1959:1997))
head(CO2_wide)

## Tidy the CO2 Data
CO2_tidy <- 
  # Take the wide format data
  CO2_wide |>
  pivot_longer(
    # And pivot everything but the year column
    -year, 
    # into columns for Month and CO2 Concentration 
    names_to = "month", values_to="CO2")

summary(CO2_tidy)
str(CO2_tidy)

## +++++++++++++++++++++++++++++++++++++++++++
## How do CO2 concentrations behave over time?
## +++++++++++++++++++++++++++++++++++++++++++

CO2_tidy |>
  ggplot(aes(x = as.integer(month), y = CO2)) +
  geom_line(aes(color = year)) +
  # Format the X Axis
  scale_x_continuous(
    name = "Month",
    breaks = c(1, 4, 7, 10, 12),
    labels = c("Jan", "Apr", "Jul", "Oct", "Dec")
  ) +
  scale_y_continuous(
    # Label the primary axis
    name = "Co2 Concentration",
    # Add a secondary axis for the year
    # corresponding to the default breaks
    sec.axis =
      sec_axis(
        name   = "Year",
        # Keep the scale of the primary axis
        trans  = ~. * 1,
        # Add breaks at breaks defined by primary y-axis
        breaks = c(320, 330, 340, 350, 360),
        # And label them according to the appropriate year
        # Determine the break:label combination by:
        #   Grouping the data by year and calculating
        #   the mean Co2 Concentration
        labels = c("1965","1973", "1981", "1988", "1994")
      )
    ) +
  labs(title = "Co2 Concentrations Over Time",
       x = "Month") +
  theme_bw() +
  # Since we're using the secondary y-axis 
  # to display the year, which is our grouping variable,
  # we can remove the legend. 
  # 
  # We used the secondary y-axis because otherwise, the 
  # legend would have 39 labels and would clutter the
  # plot
  theme(legend.position = "none")
  
## ++++++++++++++++++++++
## Admissions Data ====
## ++++++++++++++++++++++
data("admissions")

## Is there a gender bias in admissions
## at UC Berkeley?
##
## The data isn't quite tidy so we'll need to perform
## a couple of different transformations to get it
## in the format we need.
## 
## First off, admitted is a percentage, so we need to 
## work out the actual number of admitted applicants. 
data("admissions")


admitted.wide <- 
  admissions |>
  ## calculate the raw admission counts
  ## and the number of people rejected
  mutate(admitted = round((admitted*applicants)/100),
         rejected = applicants - admitted) |> 
  select(-applicants)|> 
  ## Widen the data to have columns for: 
  ## admitted_men, rejected_men, admitted_women, rejected_women
  pivot_wider(id_cols = major, 
              names_from = gender,
              values_from = c(admitted, rejected))

## To test if gender and admissions are independent or not,
##   we perform a chi-squared test on the 2x2 table defined by
##   the summary data for men and women
counts <- 
  admitted.wide |>
  pivot_longer(cols = -major,
               names_to = c("status", "gender"),
               values_to = "count",
               ## For each column specified in `names_to` we need
               ## to specify a group in the names_pattern argument
               ##   - specify a group by enclosing it in parentheses 
               ## 
               ## Make sure the order of your groupings matches
               ## the order of the columns you specified in `names_to`
               names_pattern = "(admitted|rejected)_(men|women)") |>
  mutate(gender = factor(gender), status = factor(status)) |>
  ## Get the summary counts for the grouping
  ## defined by gender and status
  group_by(gender,status) |>
  summarize(count = sum(count)) |>
  pull(count) |> 
  matrix(nrow = 2, ncol = 2, byrow = T)

## Assign names to the columns and rows of counts
## and perform a chi-square test of independence
colnames(counts) <- c("admitted", "rejected")
rownames(counts) <- c("men","women")

chisq.test(counts, correct = F)

## Based on the Chi-Square test, it's evident there's a lack of 
## independence between Gender and Admissions Status. 
## 
## Is this enough to make a claim of bias?
## If so, does this bias hold across the different majors?
## 
## To begin, we need to inspect the overall acceptance rates
## for men and women

## 2 x 2 table gender ~ status
## With marginal totals
row.total <- rowSums(counts)
counts.table <- cbind(counts, row.total)
col.total <- colSums(counts.table)
counts.table <- rbind(counts.table, col.total)

counts.table
## Proportion tables
prop.table(counts)
prop.table(counts, margin = 1)
prop.table(counts, margin = 2)

## Observations:
##    - In total, men are accepted at more than twice the rate of women (26.47% : 12.31%)
##      - Odds of being accepted if you're a man = 1.84:1
##        - Odds of being rejected if you're a woman = 1.84:1
##    - Of those admitted, men make up more than 2/3 of the incoming class   
##      - Men put in nearly 47% more applications than women
##      - Men are rejected at a lower rate than women
##    
##  These numbers give the impression of a bias in favor of men. However, we've yet to 
##  investigate the effect Major has, if any, on the overall bias we're seeing. 
##  
##  To compare the Majors, we'll use their overall selectivity

admitted.long <- 
  admitted.wide |>
  pivot_longer(cols = -major,
               names_to = c("status", "gender"),
               values_to = "count",
               ## For each column specified in `names_to` we need
               ## to specify a group in the names_pattern argument
               ##   - specify a group by enclosing it in parentheses 
               ## 
               ## Make sure the order of your groupings matches
               ## the order of the columns you specified in `names_to`
               names_pattern = "(admitted|rejected)_(men|women)") |>
  mutate(gender = factor(gender), status = factor(status))


major.ranks <-
  admitted.wide |>
  mutate(total_applicants = admitted_men+admitted_women+rejected_men+rejected_women,
         major_selectivity = (admitted_men+admitted_women)/total_applicants, 
         major_rank = rank(major_selectivity))|>
    arrange(major_rank) |>
    ## re-order the major by rank/selectivity
    ## This is important when working in ggplot:
    ##  It uses the order of the factor levels 
    ##  in deciding how to arrange the plots.  
    ## 
    mutate(major = fct_reorder(major, major_rank)) |>
    select(major, major_rank)

admitted.long <-
  admitted.long |> 
  left_join(major.ranks, by = "major") |>
  ## use the already ordered levels from major.ranks
  mutate(major = factor(major, levels = levels(major.ranks$major))) 

## how do admissions look given gender and status?

admitted.long |>
  filter(status == "admitted")|>
  ggplot(aes(x = major, y = count, 
             fill = gender)) +
  geom_point(shape = 21, size = 8, alpha = 0.88) +
  scale_fill_discrete(type = c("#E1BE6A", "#40B0A6")) +
  labs(title = 'Do UC Berkeley Admissions Favor Men',
       subtitle = "Admission Counts Ordered by Selectivity of Major",
       caption = "Data: DSLABS::admissions",
       fill = "Gender", 
       x = "Major\nMost Selective >> Least Selective") +
  theme_bw() +
  theme(plot.caption.position = "plot")

## Observations:
##  - In the most selective majors, there's little to no evidence of a 
##    bias in favor of men. If anything, women are either on equal footing 
##    or have the upper hand. In the least selective majors (A & B), however, 
##    men far outpace women. And in those majors, women only make up 11.6% and 4.27% 
##    of total applications. 

admitted.long |>
  pivot_wider(id_cols = c(major, major_rank),
              names_from= c(status, gender),
              values_from = count) |>
  mutate(total_applicants = (admitted_men + admitted_women + 
                             rejected_men + rejected_women),
         ## How many male & female applicants 
         ## were there?
         females = admitted_women + rejected_women,
         males = admitted_men + rejected_men,
         ## What percentage of all applications
         ## are submitted by females|males?
         f.applied = females / total_applicants,
         m.applied = males / total_applicants,
         ## What percentage of female|male applicants
         ## get admitted?
         f.apps.admitted = admitted_women / females,
         m.apps.admitted = admitted_men / males,
         ## What's the percentage of women|men
         ## each program admits?
         f.admitted = admitted_women / total_applicants,
         m.admitted = admitted_men / total_applicants
  ) |>
  select(-matches("^admitted|rejected")) |>
  arrange(major_rank)


## There's insufficient evidence to claim a gender bias except within the least selective
## majors, and women simply aren't applying to those programs at nearly the same rate
## as men:
##    - Of the 1,835 female applicants,   133 applied to A & B,
##    - Of the 2,691   male applicants, 1,385 applied to A & B,
##    
##    - The odds a female is applying to 1 of the top 4: 13.80:1
##    - The odds a   male is applying to 1 of the top 4: 0.943:1
##  
##    - The odds a random application is a female applying to 1 of the top 4: 14.63:1
## 
## Had the reporters or researchers who made the initial claim took a little time 
## break the numbers out by major, they'd have revealed a completely different picture 
## than the one they painted. 
##
