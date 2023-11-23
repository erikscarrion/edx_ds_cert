# Wrangling - Web Scraping
library(rvest)

# download a wikipedia page
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
h

tab <- h %>% html_elements("table")
tab.1 <- tab[[3]] %>% html_table 
tab.1 <- tab.1 %>% setNames(c("state", "population",
                              "total", "murders",
                              "gun_murders","gun_ownership",
                              "total_rate", "murder_rate",
                              "gun_murder_rate"))

# Guacamole Recipe
guac <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- guac %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- guac %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- guac %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
directions <- guac %>% html_nodes(".o-Method__m-Step") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

# we can use this to create a function called get_recipe
get_recipe <- function(url){
  # Extract the html 
  web.page <- read_html(url)
  # Pull out recipe, prep_time, ingredients, and instructions
  recipe <- web.page %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- web.page %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- web.page %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text() 
  ingredients <- ingredients[-1] %>% 
    # remove white space and new line characters at front and rear
    sub(pattern = "\n            \n              \n                \n              \n              ", 
        replacement = "") %>% 
    sub(pattern = "\n            \n          ",
        replacement = "")
  directions <- web.page %>% html_nodes(".o-Method__m-Step") %>% html_text() %>% 
    # remove white space and new line characters at front and rear
    sub(pattern  = "\n              ",
        replacement = "") %>%
    sub(pattern = "\n            ",
        replacement = "")
  
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients, directions=directions))
}

salad_stuffed_peppers = get_recipe("https://www.foodnetwork.com/recipes/food-network-kitchen/salad-stuffed-peppers-9970168")

# Assessment - Web Scraping
url.1 <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
# get the full html
page <- read_html(url.1)
# select tables
nodes <- html_nodes(page, "table")

# convert the 1st 4 tables into data frames and inspect them
salary.2018 <- nodes[[2]] %>% html_table(header = T)
salary.2017 <- nodes[[3]] %>% html_table(header = T)
salary.2016 <- nodes[[4]] %>% html_table(header = T)
salary.2015 <- nodes[[5]] %>% html_table(header = T)

# Which of the 1st 4 nodes are tables with salary information
for(i in 1:4){
  tab = h[[i]] %>% html_table(header=T)
  print(tab)
} 

html_table(nodes[[21]], header = T)


# Create a table called tab_1 using entry 10 of nodes. Create a table called tab_2 using entry 19 of nodes.
# 
# Note that the column names should be c("Team", "Payroll", "Average"). You can see that these column names are actually in the first data row of each table, and that tab_1 has an extra first column No. that should be removed so that the column names for both tables match.
# 
# Remove the extra column in tab_1, remove the first row of each dataset, and change the column names for each table to c("Team", "Payroll", "Average"). Use a full_join() by the Team to combine these two tables.
# 
# How many rows are in the joined data table?

tab_1 <- html_table(nodes[[10]], header = T) %>% select(-No.)
tab_1
tab_2 <- html_table(nodes[[19]], header = T)
tab_2

full_join(tab_1,tab_2, by="Team")

# Wikipedia opinion polling
wiki.url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
 
wiki.page <- read_html(wiki.url)
wiki.nodes <- html_elements(wiki.page, "table")

# check the 1st 9 tables
for(i in 1:9){
  table = html_table(wiki.nodes[[i]], fill = T)
  print(table)
}
