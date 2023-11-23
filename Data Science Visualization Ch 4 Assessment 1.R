library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3) 

summary(stars)


# Density plot of magnitude
stars %>% ggplot(aes(magnitude)) + geom_density()

# Question 3
stars %>% ggplot(aes(log(temp, base = 10)), y = ..count..) + geom_density()
stars %>% ggplot(aes(temp, y = ..count..)) + geom_density()
stars %>% ggplot(aes(temp)) + geom_histogram()

# Question 4
stars %>% ggplot(aes(temp, magnitude)) + geom_point()

# Question 5
stars %>% ggplot(aes(log10(temp),magnitude)) + 
  geom_point() +
  scale_x_reverse() + 
  scale_y_reverse(breaks = c(20,10,0,-10),
                  labels = c("20", "10", "0", "-10")) + 
  labs(title = "Star Temperatures vs Magnitude")

# No log Transform
stars %>% ggplot(aes(temp,magnitude)) + 
  geom_point() +
  scale_x_reverse() + 
  scale_y_reverse(breaks = c(20,10,0,-10),
                  labels = c("20", "10", "0", "-10")) + 
  labs(title = "Star Temperatures vs Magnitude",
       subtitle = "No log transform")

# Plot the labels
stars %>% ggplot(aes(log(temp, base = exp(1)),magnitude, label = star)) + 
  geom_label() +
  scale_x_reverse() + 
  scale_y_reverse() + 
  labs(title = "Star Temperatures vs Magnitude",
       subtitle = "Log transform")



# Color by star type
stars %>% ggplot(aes(log(temp, base = exp(1)) , magnitude, color = type)) + 
  geom_point(size = 2.5) + geom_jitter() +
  scale_colour_viridis_d("Star Type", option = "H") +
  theme_light() +
  scale_x_reverse() + 
  scale_y_reverse() + 
  labs(title = "Star Temperatures vs Magnitude by Type",
       subtitle = "Log transform")

stars %>% mutate()
str(stars)

stars %>% filter(type == "G") %>% .$magnitude %>% min()

stars %>% filter(type == "G")
