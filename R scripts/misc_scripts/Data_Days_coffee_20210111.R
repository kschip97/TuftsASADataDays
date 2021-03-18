## Data_Days_coffee_20210111.R ##
## Programmer: Kees Schipper ##
## Date Created: 2020-01-11 ##
## Date last updated: 2020-01-11 ##


# load packages -----------------------------------------------------------

library(tidyverse)
library(tidytuesdayR)
library(ggdark)
library(broom)


# get some tt_data --------------------------------------------------------

coffee <- tt_load(x = 2020, week = 28)

cfe_data <- coffee[[1]]

cfe_clean <- cfe_data %>%
  filter(total_cup_points > 50) %>%
  separate(bag_weight, into = c("weight", "weight_unit"), sep = " ") %>%
  mutate(weight = as.numeric(weight),
         weight = ifelse(weight_unit == "lbs", weight/2.2, weight)) %>%
  filter(!is.na(weight))

lm(total_cup_points ~ number_of_bags + bag_weight + species, data = cfe_data)




# plotting ----------------------------------------------------------------

ggplot(data = cfe_data, aes(x = country_of_origin, y = total_cup_points)) +
  geom_boxplot(aes(fill = species)) +
  geom_jitter(aes(col = species), alpha = 0.2) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Coffee Ratings by country of origin and species",
       x = "country of origin", y = "total cup points") +
  ylim(55, 95)

count(cfe_data, species, sort = T)

ggplot(data = cfe_data, aes(x = owner)) +
  geom_freqpoly(aes(col = country_of_origin), size = 1.5) +
  theme(legend.position = "none") +
  xlim(55, 100) +
  labs(title = str_to_title("frequency polygons of coffee ratings by country"))


cfe_model <- lm(total_cup_points ~ altitude_mean_meters, data = cfe_data)

summary(cfe_model)

plot(predict(cfe_model))

plot(cfe_data$altitude_mean_meters, cfe_data$total_cup_points, xlim = c(0, 1000))
lines(predict(cfe_model), col = "red")

ggplot(data = cfe_data, aes(x = altitude_mean_meters, y = total_cup_points)) +
  geom_point(aes(col = species)) +
  geom_smooth(aes(col = species), method = "gam", se = F) +
  xlim(0, 2500) +
  labs(title = "Cup points vs. altitude")

