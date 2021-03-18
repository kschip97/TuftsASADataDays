## Data_Days_20200114.R ##
## Programmer: Kees Schipper ##
## Date Created: 1/8/2021 ##
## Last Updated: 1/8/2021 ##

library(tidyverse)
library(tidytuesdayR)


# Load in tidy tuesday data -----------------------------------------------

bigmac <- tt_load(2020, week = 52)[[1]]

names(bigmac)
unique(bigmac$iso_a3)
unique(bigmac$currency_code)

hist(bigmac$local_price) # doesn't make sense because in different currencies
hist(bigmac$dollar_price)
hist(bigmac$adj_price)

bigmac_plot <- bigmac %>%
  select(where(is.numeric))

plot(bigmac_plot[1:5])
plot(bigmac_plot[6:10])
plot(bigmac_plot[11:15])

ggplot(data = bigmac, aes(x = iso_a3, y = adj_price)) +
  geom_boxplot(aes(fill = iso_a3)) +
  geom_jitter(color = "red", alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")

ggplot(data = bigmac, aes(x = iso_a3, y = gdp_dollar)) +
  geom_boxplot(fill = "gray", color = "black") +
  geom_jitter(color = "red", alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  theme(legend.position = "none")


bigmac %>%
  filter(date > "2013-07-27") %>%
ggplot(aes(x = date, y = adj_price)) +
  geom_smooth(method = "loess", aes(col = iso_a3), span = 0.5, se = F) +
  geom_point(aes(col = iso_a3)) +
  theme_dark() +
  theme(legend.position = "none")


nh <- tibble(temp = nhtemp)
nh$time <- seq(1, nrow(nh))
tempmod <- lm(temp ~ time, data = nh)
summary(tempmod)
coefs <- tempmod$coefficients

plot(nh$time, nh$temp, type = "l", col = "red", main = "New Haven Temperatures Over Time \n
  with linear regression")
lines(predict(tempmod), col = "blue")
text(
  x = 30,
  y = 54,
  labels = paste0("temp = ", round(coefs[1],2), " + ", round(coefs[2], 2), "(time) + ",
                  round(coefs[3],2), "(time)^2 + ", round(coefs[4], 2), "(time)^3"),
  col = "blue"
  )

