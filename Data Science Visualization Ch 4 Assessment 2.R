library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

str(historic_co2)
unique(historic_co2$source)
historic_co2 <- historic_co2 %>% mutate(source = factor(source),
                                        year   = factor(year))
str(historic_co2)
head(historic_co2)


summary(temp_carbon)
  
temp_carbon %>% arrange(desc(year))

temp_carbon %>% filter(!is.na(carbon_emissions)) %>% select(year) %>% max()
temp_carbon %>% filter(!is.na(carbon_emissions)) %>% max(.$year)

min_year <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% min()
max_year <- temp_carbon %>% filter(!is.na(carbon_emissions)) %>% pull(year) %>% max()

temp_carbon[temp_carbon$year %in% c(min_year, max_year),]

# Question 3

min_year_temp <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% pull(year) %>% min()
max_year_temp <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% pull(year) %>% max()

temp_carbon[temp_carbon$year %in% c(min_year_temp, max_year_temp),]


# Question 4
p <- temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>%  
  ggplot(aes(year, temp_anomaly, label = "Temperature Anomaly")) + 
  geom_line() + 
  labs(title = "Temperature anomaly relative to 20th century mean, 1880-2018") + 
  ylab("Temperature anomaly (degrees C)")+
  geom_hline(aes(yintercept = 0), color  = "blue")

p + geom_text(aes(2000, .05, label = "20th century mean"), col = "blue") + 
  geom_line(aes(year, ocean_anomaly, label = "Ocean Anomaly"), color = "#268a5e") +
  geom_line(aes(year, land_anomaly, label = "Land Anomaly"), color = "#fa5b1d")


data <- temp_carbon %>% pivot_longer(cols=c("temp_anomaly","land_anomaly","ocean_anomaly","carbon_emissions"),
                                     names_to = "Type",
                                     values_to = "Measurement")

data %>% mutate(Type = factor(Type))

# Question 7
data %>% filter(Type != "carbon_emissions", year >= 1875) %>%
  ggplot(aes(year, Measurement, color = Type)) + geom_line() + 
  geom_hline(aes(yintercept = 0), color = "#3581b0")+
  geom_vline(aes(xintercept = 2018), color = "#b095cf")

# Questions 8 - 12
ann_text = data.frame(label = c('Ind. Rev.\nBegins', 'Ind. Rev.\nBegins', 'Ind. Rev.\nBegins'),
                      gas = c("CH4", "CO2", "N2O"),
                      x = c(1750, 1750, 1750),
                      y = c(1250, 335, 300))

greenhouse_gases %>% ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  labs(title = "Concentration of Greenhouse Gases", subtitle = "Faceted by Gas Type") +
  geom_vline(aes(xintercept = 1850)) +
  geom_text(data = ann_text, mapping = aes(x = x, y = y, label = label))


temp_carbon %>% filter(!is.na(carbon_emissions)) %>% 
                         ggplot(aes(x = year, y = carbon_emissions)) +
  geom_line() + geom_vline(aes(xintercept = 1850)) +
  geom_vline(aes(xintercept = 1960, label = "1960")) +
  geom_vline(aes(xintercept = 1978, label = "Late 70's"))

historic_co2 %>% filter(!is.na(co2)) %>% ggplot(aes(x = year, y = co2, color = factor(source))) +
  xlim(c(-800000, -775000)) +
  geom_line() + 
  labs(title = "Concentration of CO2",
       subtitle = "775k-850K years ago")

historic_co2 %>% filter(!is.na(co2)) %>% ggplot(aes(x = year, y = co2, color = factor(source))) +
  xlim(c(-375000, -330000)) +
  geom_line() + 
  labs(title = "Concentration of CO2",
       subtitle = "330k-375K years ago")

historic_co2 %>% filter(!is.na(co2)) %>% ggplot(aes(x = year, y = co2, color = factor(source))) +
  xlim(c(-3000, 2018)) +
  geom_line() + 
  labs(title = "Concentration of CO2",
       subtitle = "3000 years ago to present")


                         